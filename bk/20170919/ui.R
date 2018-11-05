source("helpers.R")


navbarPage("Signal",
           tabPanel("Summary",
                    verbatimTextOutput("sumTab_summary")
           ),
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        
                        textInput("i_code", "Stock Number (No description)", placeholder = "Stock Number"),
                        
                        #dateRangeInput("i_daterange", "Date range:", format = "yyyy-mm-dd", start = (Sys.Date() - 30), end = Sys.Date(), min = "2010-01-01", max = Sys.Date() + 7),
                        dateRangeInput("i_daterange", "Date range:", format = "yyyy-mm-dd", start = "2017-01-01", end = "2018-12-31", min = "2010-01-01", max = "2018-12-31"),
                        
                        hr(),
                        
                        submitButton("Update View")
                        
                        
                      ),
                      
                      mainPanel(
                        h4("Plot"),
                        plotOutput("out_mainplot"),
                        
                        h4("Summary"),
                        verbatimTextOutput("out_summary"),
                        
                        h4("Debug"),
                        verbatimTextOutput("out_debug")
                      )
                    )
           ),
		   tabPanel("About",
                    verbatimTextOutput("aboutTab_about")
           )
           
)
