options(dplyr.print_max = 1e9)
options(scipen=999)

signal_list <- c('s_bull_stick', 's_bear_stick', 's_bull_engulf', 's_bear_engulf', 's_bull_harami', 's_bear_harami', 's_2day_reverse_good', 's_2day_reverse_bad', 's_bull_pierce', 's_bear_pierce', 's_hammer', 's_shooting_star')

f_bull_stick <- function(open.lag0, close.lag0, upper.limit = 0.05, lower.limit = 0.04)
{
	boolean <- 	ifelse(
					((close.lag0 - open.lag0) / open.lag0) > lower.limit & 
					((close.lag0 - open.lag0) / open.lag0) < upper.limit, 
					1, 0
				)
				
	return(boolean)	
}

f_bear_stick <- function(open.lag0, close.lag0, upper.limit = -0.05, lower.limit = -0.04)
{
	boolean <- 	ifelse(
					((close.lag0 - open.lag0) / open.lag0) < lower.limit & 
					((close.lag0 - open.lag0) / open.lag0) > upper.limit, 1, 0
				)

	return(boolean)	
}

f_bull_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
	
	boolean <- 	ifelse(
					((close.lag0 > open.lag0) & 
					(open.lag1 > close.lag1) &
					(close.lag0 > open.lag1) &
					(close.lag1 > open.lag0))
					, 1, 0
				)

	return(boolean)	
}

f_bear_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{	
	
	boolean <- 	ifelse(
					(close.lag0 < open.lag0) & 
					(close.lag1 > open.lag1) &
					(close.lag0 < open.lag1) &
					(close.lag1 < open.lag0)
					, 1, 0
				)

	return(boolean)	
}

f_bull_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
	boolean <- 	ifelse(
					(close.lag0 > open.lag0) & 
					(close.lag1 < open.lag1) &
					(close.lag0 < open.lag1) &
					(close.lag1 < open.lag0)
					, 1, 0
				)
 
	return(boolean)	
}

f_bear_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
	boolean <- 	ifelse(
					(close.lag0 < open.lag0) & 
					(close.lag1 > open.lag1) &
					(close.lag0 > open.lag1) &
					(close.lag1 > open.lag0)
					, 1, 0
				)

	return(boolean)	
}

f_2day_reverse_good<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_up_threshold = 0.03, ytd_down_threshold = -0.03)
{
	boolean <- 	ifelse(
					(((close.lag0 - close.lag1) / close.lag1) > today_up_threshold) &
					(((close.lag1 - close.lag2) / close.lag2) < ytd_down_threshold)
					, 1, 0
				)

	return(boolean)	
}

f_2day_reverse_bad<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_down_threshold = -0.03, ytd_up_threshold = 0.03)
{
	boolean <- 	ifelse(
					(((close.lag0 - close.lag1) / close.lag1) < today_down_threshold) &
					(((close.lag1 - close.lag2) / close.lag2) > ytd_up_threshold)
					, 1, 0
				)

	return(boolean)	
}

f_bull_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
	boolean <- 	ifelse(
					(close.lag0 > open.lag0) &
					(close.lag1 < open.lag1) &
					(open.lag0 < close.lag1) &
					(close.lag0 < open.lag1) &
					(close.lag0 >((open.lag1 + close.lag1)/2))
					, 1, 0
				)
				
	return(boolean)
}

f_bear_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
	boolean <- 	ifelse(
					(close.lag1 > open.lag1) &
					(open.lag0 > close.lag0) &
					(close.lag0 > open.lag1) &
					(close.lag0 < ((open.lag1 + close.lag1)/2)) &
					(open.lag0 > close.lag1)
					, 1, 0
				)
				
	return(boolean)
}

f_hammer <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
	boolean <- 	ifelse(
					(close.lag0 > open.lag0) &
					((open.lag0 - low.lag0) >= (tail_multiplier * (close.lag0 - open.lag0))) &
					(((close.lag0 - open.lag0) / open.lag0) > least_body_length) &
					((high.lag0 - close.lag0) <= (close.lag0 - open.lag0))
					, 1, 0
				)
				
	return(boolean)
}

f_shooting_star <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
	boolean <- 	ifelse(
				(open.lag0 > close.lag0) &
				((high.lag0 - open.lag0) >= (tail_multiplier * (open.lag0 - close.lag0))) &
				(((close.lag0 - low.lag0) <= (open.lag0 - close.lag0))) &
				(((open.lag0 - close.lag0) / open.lag0) > least_body_length)
				, 1, 0
			)
				
	return(boolean)

}


calSignal <- function(df){
	
	n = 5
	
	data <- df %>% filter(volumn != 0) %>% arrange(date)
	
	
	data <- data %>% mutate(open.lag1 = lag(open, n = 1),
						open.lag2 = lag(open, n = 2),
						close.lag1 = lag(close, n = 1),
						close.lag2 = lag(close, n = 2))
	
	data <- data %>% mutate(s_bull_stick = f_bull_stick(open, close),
					s_bear_stick = f_bear_stick(open, close),
					s_bull_engulf = f_bull_engulf(open, close, open.lag1, close.lag1),
					s_bear_engulf = f_bear_engulf(open, close, open.lag1, close.lag1),
					s_bull_harami = f_bull_harami(open, close, open.lag1, close.lag1),
					s_bear_harami = f_bear_harami(open, close, open.lag1, close.lag1),
					s_2day_reverse_good = f_2day_reverse_good(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
					s_2day_reverse_bad = f_2day_reverse_bad(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
					s_bull_pierce = f_bull_pierce(open, close, open.lag1, close.lag1),
					s_bear_pierce = f_bear_pierce(open, close, open.lag1, close.lag1),
					s_hammer = f_hammer(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005),
					s_shooting_star = f_shooting_star(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005))

	
	for(i in 1:5) {				 
		data <- high_return(df = data, n=i)
		data <-  low_return(df = data, n=i)
	}
	
	data %>% arrange(desc(date))
}

getSignal <- function(df){
	data <- df %>% filter(volumn != 0) %>% arrange(code, date)
	
	data <- data %>% mutate(open.lag1 = lag(open, n = 1),
						open.lag2 = lag(open, n = 2),
						close.lag1 = lag(close, n = 1),
						close.lag2 = lag(close, n = 2))
	
	data <- data %>% mutate(s_bull_stick = f_bull_stick(open, close),
					s_bear_stick = f_bear_stick(open, close),
					s_bull_engulf = f_bull_engulf(open, close, open.lag1, close.lag1),
					s_bear_engulf = f_bear_engulf(open, close, open.lag1, close.lag1),
					s_bull_harami = f_bull_harami(open, close, open.lag1, close.lag1),
					s_bear_harami = f_bear_harami(open, close, open.lag1, close.lag1),
					s_2day_reverse_good = f_2day_reverse_good(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
					s_2day_reverse_bad = f_2day_reverse_bad(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
					s_bull_pierce = f_bull_pierce(open, close, open.lag1, close.lag1),
					s_bear_pierce = f_bear_pierce(open, close, open.lag1, close.lag1),
					s_hammer = f_hammer(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005),
					s_shooting_star = f_shooting_star(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005))
	
	data <- data %>% select(date, code, contains('s_')) %>% arrange(code)
	
	data
}