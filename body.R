UIBody=dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidPage(
                fluidRow(
                  valueBox(
                    "File Input","", icon = icon("upload"),
                    color = "blue", width = "100%"
                  )
                ),
                
                fluidRow(
                  column(12,fileInput("file", 
                                      label = h6(" "),
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values",
                                                 ".csv"))
                  )
                ),
                fluidRow(
                  column(12,textOutput("text"))
                  # tableOutput("contents")
                  
                ),
                fluidRow(
                 column(12,plotOutput("plotPiePublisher"),height="10px"),
                 hr()
                ),
                fluidRow(
                  valueBox(
                    "Predicted Revenue","", icon = icon(""),
                    color = "blue", width = "100%"
                  )
                ),
                
                fluidRow(
                  # column(12,plotOutput("plotPieFormat"))
                  
                  # text("Revenue : Actual & Precicted"),
                  valueBoxOutput("uploadedRevenue"),
                 valueBoxOutput("predictedMaxRevenue")
                )
                
              )
      ),
      tabItem("uploadedData",
              fluidPage(
                tableOutput("contents")
              )
        
      ),
      tabItem("predictionModel",
              fluidPage(
                verticalLayout(
                  fluidRow(
                    valueBox(
                      "Prediction Model","", icon = icon("line-chart"),
                      color = "blue", width = "100%"
                    )
                  ),
                  fluidRow(
                    box(selectInput("selectGroup1", label = h4("Book Format"), 
                                    choices = list("Audible" = 1, "Hard Cover" = 2, "Kindle Edition" = 3,"Paperback"=4),
                                    selected = 1),width = "6"),
                    box(selectInput("selectGroup2", label = h4("Publisher Type"), 
                                    choices = list("Indie Publisher" = 1, "Small-Medium Publisher" = 2, "AmazonPublsher" = 3,"BigFivePublisher"=4),
                                    selected = 1),width = "6")
                  ),
                  
                  fluidRow(
                    
                    # Copy the line below to make a number input box into the UI.
                    box(numericInput("num1", label = h4("Sale Price"), value = 1),width = "6"),
                    # Copy the line below to make a number input box into the UI.
                    box(numericInput("num2", label = h4("Number of Pages"), value = 1),width = "6")
                    
                  ),
           
                  fluidRow(
                    
                    # Copy the line below to make a number input box into the UI.
                    box(numericInput("num3", label = h4("Expected Rating"), value = 1),width = "6"),
                    # fluidRow(column(3, verbatimTextOutput("value")))
                    box(numericInput("num4", label = h4("Expected Count of Reviews"),value = 1),width = "6")
                    
                    
                  ),
                  fluidRow(
                    
                    column(4,style = "font-style: bold;",submitButton("Submit"))
                    # tags$style(type='text/css', "#button_sub { width:100%; margin-left: 105px;}")
                    
                  )
                  # fluidRow(
                  #   column(6,verbatimTextOutput("text")
                  #   )
                  # )
                  
                ),
                
                  fluidRow(
                   valueBoxOutput("sales"),
                    # valueBoxOutput("minSold"),
                   valueBoxOutput("Revenue")
                  )
                  
                
                
              )
      )
    )
)
  
