library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
library(quantmod)
library(RPostgreSQL)

source("helpers.R")

# TODO: Temp way to save option list
optionListFile 	<- read.csv(file = "/var/spool/chun/shiny/optionList.txt", header = TRUE)
optionList 		<- optionListFile[,'code']

option_stock_code <- sapply(optionList, function(x){
											paste0("'", str_pad(x,width=4, side="left", pad="0"), "'")
										})
option_stock_list <- paste0(option_stock_code, collapse = ",")


drv <- dbDriver("PostgreSQL")

function(input, output, session){
	datasetInput <- reactive({
		stock_code <- NA
		df_stock <- NA
		result <- NA


		if(input$i_code != ""){
			
			
			con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")
		
			stock_code = str_pad(input$i_code, width=4, side="left", pad="0")
			query_str <- sprintf("SELECT * from stock_price where code = '%s' order by date desc", stock_code)
			
			df_stock <- dbGetQuery(con, query_str)
			
			if(!is.na(input$i_daterange[1]) & !is.na(input$i_daterange[2]) & input$i_code != ""){
				result  <- df_stock %>% filter(date <= input$i_daterange[2] & date >= input$i_daterange[1])
			}
			else{
				result <- df_stock
			}
			
			
			dbDisconnect(con)
			
		}
	

	
	

		result

	})

	output$out_mainplot <- renderPlot({
		df_stock <- datasetInput()
		if(!is.na(df_stock)){
			df_stock.zoo <- zoo(x = df_stock[,3:8], order.by = df_stock$date)
			df_stock.zoo.ohlc <- as.quantmod.OHLC(df_stock.zoo,
								   col.names = c("open", "high",
												 "low", "close",
												 "volumn", "adj"))
			chartSeries(df_stock.zoo.ohlc, name = df_stock$code[1], theme='white')
			
			

		}
	})

	output$out_summary <- renderPrint({
		df_stock <- datasetInput()
		# if(!is.na(df_stock)){
			# head(df_stock)
		# }
		# else{
			# cat("No Data")
		# }
		
		head(df_stock)

	})
	
	output$aboutTab_about <- renderPrint({

		print("About the project")

	})
	
	output$out_debug <- renderPrint({
		df_stock <- datasetInput()
		if(!is.na(df_stock)){
			df_stock.zoo <- zoo(x = df_stock[,3:9], order.by = df_stock$date)
			df_stock.zoo.ohlc <- as.quantmod.OHLC(df_stock.zoo,
								   col.names = c("open", "high",
												 "low", "close",
												 "volumn", "adj", "code"))
			print(df_stock.zoo.ohlc)
			
		}
		
		
		
	})
	
	#Summary Tab
	output$sumTab_summary <- renderPrint({

		# Define Column Order for elementwise matrix multiplication
		col_order <- c('code', 's_bull_stick','s_bear_stick','s_bull_engulf','s_bear_engulf', 's_bull_harami','s_bear_harami','s_2day_reverse_good','s_2day_reverse_bad','s_bull_pierce','s_bear_pierce','s_hammer','s_shooting_star')
		
		con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")

		
		# TODO: Add input box for date 
		i_start_date <- '2017-09-13'
		i_end_date <- '2017-09-18'
		
		option.data.query <- sprintf("SELECT * from stock_price where code in (%s) and date >= '%s' and date <= '%s' order by date desc", option_stock_list, i_start_date, i_end_date)
		option.data.signal.query <- sprintf("SELECT * from signal_strength where code in (%s)", option_stock_list)
		
		df.option <- dbGetQuery(con, option.data.query)
		df.option.signalStrength <- dbGetQuery(con, option.data.signal.query)
		
		dbDisconnect(con)
		
		df.option <- df.option %>% arrange(code, desc(date))
		
		
		# Calculate signal for the latest date
		df.option.signal <- getSignal(df.option)
		df.option.signal <- df.option.signal %>% arrange(code, desc(date)) %>% group_by_(~ code) %>% do(head(., n = 1)) %>% ungroup()
		df.option.signal <- df.option.signal[, c('date', col_order)] %>% arrange(code)
		
		# Get Signal Strength for each code in option list
		df.option.signalStrength <- df.option.signalStrength %>% select(-id, -value_recent)
		df.option.signalStrength <- dcast(df.option.signalStrength, code ~ signal)
		df.option.signalStrength <- df.option.signalStrength[, col_order] %>% arrange(code)
		
		# Signal Strength for 1880 missing, find intersect
		codeIntersect <- intersect(df.option.signalStrength$code, df.option.signal$code)
		df.option.signalStrength <- df.option.signalStrength %>% filter(code %in% codeIntersect)
		
		# Elementwise Multiplication 
		df.option.signalStrength.onlySignal <- df.option.signalStrength %>% select(-code)
		df.option.signal.onlySignal 		<- df.option.signal 		%>% select(-date,-code)
		signal.matrix <- df.option.signalStrength.onlySignal * df.option.signal.onlySignal
		
		# Append date, code column back to final df
		df.result.wide 	<- cbind(date = df.option.signal$date, code = df.option.signal$code, signal.matrix)
		df.result.long 	<- melt(df.result.wide, id.vars=c("date", "code"), variable.name="signal", value.name="value") %>% arrange(code)
		df.result 		<- df.result.long %>% filter(value != 0)
		
		print(df.result)
		
	})

  
}