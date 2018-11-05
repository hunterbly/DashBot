source("helpers.R")


navbarPage("Signal",
		   
		   
           tabPanel("Option",
				fluidRow(
					column(4,
						   textInput("optionTab_dateInput", "Date", value = "2017-01-01")
					),
					column(4,
						   selectInput("optionTab_optionType","Option:", 
										choices = c("All" = "All","Call" = "C", "Put" = "P"))
					),
					column(4,
						   selectInput("optionTab_exercise","Exercise Date:", 
										choices = c("All" = "All",
													"2018-05" = "2018-05-01",
													"2018-06" = "2018-06-01", 
													"2018-07" = "2018-07-01",
													"2018-08" = "2018-08-01",
													"2018-09" = "2018-09-01",
													"2018-10" = "2018-10-01",
													"2018-11" = "2018-11-01",
													"2018-12" = "2018-12-01"))
					)
				),
				fluidRow(
					column(width = 10, offset = 1,
						DT::dataTableOutput("optionTab_mainTable")
					)
					
					
				),
				verbatimTextOutput("optionTab_test")
			),
			
			tabPanel("Summary",
                    
					textInput("sumTab_dateInput", "Date range:", value = "2017-01-01"),
					hr(),
					
					h4(textOutput("sumTab_dateDisplay")),
					fluidRow(
						
						box(formattableOutput("sumTab_summary"))
						
					),
					verbatimTextOutput("sumTab_test"),
					verbatimTextOutput("sumTab_debug")
					
					
					
					
           ),
			
			tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        
                        textInput("plotTab_code", "Stock Number", placeholder = "Stock Number"),
                        
                        # TODO: 
                        dateRangeInput("plotTab_daterange", "Date range:", format = "yyyy-mm-dd", start = "2017-01-01", end = "2018-12-31", min = "2010-01-01", max = "2018-12-31"),
                        
						# Radio button to choose signal for evalutaion
						radioButtons("plotTab_chooseSignal", 
									label = h3("Signal"),
									choices = signal_radioButton_list, 
									selected = "none"),
                        hr(),
                        
                        actionButton("plotTab_button", "Update View")
                        
                        
                      ),
                      
                      mainPanel(
                        h4("Plot"),
                        plotOutput("plotTab_mainplot"),
                        
						h4("Signal History"),
                        verbatimTextOutput("plotTab_signalHistory"),
						
                        h4("Summary"),
                        verbatimTextOutput("plotTab_summary"),
                        
						
						
                        h4("Debug"),
                        verbatimTextOutput("plotTab_debug")
                      )
                    )
           ),
		   
		   tabPanel("About",

					# Test include html
					includeHTML("./aboutPage.html")
           )
           

)
