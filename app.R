## app.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(broom)
library(plotly)
library(ggplot2)
library(plotrix)
source("body.R")
source("dataManipulation.R")
source("DataQualityReport.R")

amazon_logo = "http://cdn.wccftech.com/wp-content/uploads/2015/03/amazon.png"

abs <- data.frame(read.csv(file = "ABS.csv", sep = ",", header = TRUE))
head(abs,5)
source("Analysis.R")

regressionFit = runRegression();

header <- dashboardHeader(
  title = tags$ul(a(href = 'http://amazon.com',
            img(src = amazon_logo,align="center",height = "55px",width="120px"),style="header.css"),
          class = "dropdown")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Prediction Model", icon = icon("th"), tabName = "predictionModel",
             badgeLabel = "", badgeColor = "green"),
    menuItem("Data Overview", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Uploaded Data", tabName = "uploadedData", icon = icon("table"))
    
  )
)


body <- UIBody


ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$action <- renderPrint({ input$submit })
  output$num1 <- renderPrint({ input$num1 })
  output$num2 <- renderPrint({ input$num2 })
  output$num3 <- renderPrint({ input$num3 })
  
  # inputDataPoint = data.frame()
  # inputDataPoint = abs[0,]
  regResults = tidy(regressionFit)
  rResults = as.data.frame(tidy(regressionFit))
  
 
  calculatedValue = reactive({
    x=0
    if(input$selectGroup2 == "1"){
      x = x +rResults$estimate[5]
    }
    if(input$selectGroup2 == "2"){
      x = x +rResults$estimate[6]
    }
    if(input$selectGroup1 == "1"){
      x = x +rResults$estimate[8]
    }
    if(input$selectGroup1 == "3"){
      x = x +rResults$estimate[9]
    }
    return(x+rResults$estimate[1]+
             rResults$estimate[2]*input$num1+
             (rResults$estimate[3]*input$num4)+
             (rResults$estimate[4]*input$num3))
    
    
    }) 
    
  output$sales<- renderValueBox({
    valueBox(
      paste0(ceiling(as.numeric(calculatedValue()))), "Expected Units Sold Daily", icon = icon("book"),
      color = "orange"
    )
  })
  
                                       
  output$Revenue<- renderValueBox({
    valueBox(
      paste0("$",ceiling(input$num1*as.numeric(calculatedValue()))),
      "Expected Revenue", icon = icon("money"),
      color = "orange"
    )
  })
  # output$minSold<- renderValueBox({
  #   valueBox(
  #     paste0(ceiling(pResults[1])), "Minimum Units Sold", icon = icon("book"),
  #     color = "orange"
  #   )
  # })
    
  # output$plotPiePublisher = renderPlot({
  #   piePlotData = c(sum(abs$IndiePublisher), sum(abs$AmazonPublsher), sum(abs$BigFivePublisher), sum(abs$SmallMediumPublisher))
  #   plotLabels = c("Indie Publisher", "Amazon Publisher", "Big Five Publisher", "Small-Medium Publisher")
  #   pie(piePlotData,labels=plotLabels,col=rainbow(length(plotLabels)),
  #         main="Publishers")
  # })
  # sumAudible=sum(abs$abs.FormatAudible)
  # 
  # sumHardCover= sum(abs$abs.FormatHardcover)
  # sumKindle = sum(abs$abs.FormatKindle)
  # sumPaperback =sum(abs$abs.FormatPaperback )
  
  # output$plotPieFormat = renderPlot({
  #   PlotData = c(25521,32463,195793,110209)
  #   pieLabels = c("Audibler", "Hard Cover", "Kindle", "Paperback")
  #   pie(PlotData,labels=pieLabels,col=rainbow(length(pieLabels)),main="Formats")
  # })
  

  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = TRUE,sep=",")
  })
  
  # fileDataframe = data.frame()
  # data1 <- reactive({
  # 
  #   inFile <- input$file
  # 
  #   if (is.null(inFile))
  #     return(NULL)
  # 
  # isolate({
  #   fileDataframe=read.csv(inFile$datapath, header = TRUE,sep=",")
  #   output$text <- renderText({
  #     
  #     paste("Input text is:", names(fileDataframe))
  #   })
  #   })
  # fileDataframe
  # })
  # newData=data.frame()
 
    output$text <- renderText({
      
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
      fileDataframe=read.csv(inFile$datapath, header = TRUE,sep=",")
      # newData = fileDataframe
      # newData[0,] = abs[0,]
      paste0("File Uploaded. Switch to Uploaded Data to view data")
      
    })

    newData =reactive({
      
      file1 <- input$file
      if(is.null(file1)){return()}
      read.csv(file=file1$datapath, header=TRUE,sep=",")
    })
    
    output$plotPiePublisher = renderPlot({
      validate(need(input$file," "))
      newData()
      piePlotData = c(sum(newData()[,13]), sum(newData()[,15]), sum(newData()[,16]), sum(newData()[,14]))
      plotLabels = c("Indie Publisher", "Amazon Publisher", "Big Five Publisher", "Small-Medium Publisher")
      pie(piePlotData,labels=plotLabels,col=rainbow(length(plotLabels)),
            main="Publishers")
    })
    output$uploadedRevenue = renderValueBox({
      validate(need(input$file," "))
      newData()
      rValue = 0
      for (row in 1:nrow(newData())) {
        rValue = rValue + as.numeric(newData()[row,19])*as.numeric(newData()[row,7]) 
        
      }
      valueBox(
        paste0("$",ceiling(rValue/1000000),"M"),
        "Actual Revenue", icon = icon("money"),
        color = "orange"
      )
    })
    # output$predictedMinRevenue = renderValueBox({
    #   validate(need(input$file," "))
    #   newData()
    #   finalValue = 0
    #   pValue = predict(fit,data=newData(),interval = "confidence")
    #   for (row in 1:nrow(newData())) {
    #     finalValue = finalValue + as.numeric(newData()[row,7])*as.numeric(pValue[row,1]) 
    #     
    #   }
    #   
    #   valueBox(
    #     paste0("$",ceiling(finalValue/1000000),"M"),
    #     "Predicted Minimum Revenue", icon = icon("money"),
    #     color = "red"
    #   )
    # })
    output$predictedMaxRevenue = renderValueBox({
      validate(need(input$file," "))
      newData()
      
      finalValue = 0
      pValue = predict(fit,data=newData(),interval = "confidence")
      for (row in 1:nrow(newData())) {
        finalValue = finalValue + as.numeric(newData()[row,7])*as.numeric(pValue[row,3]) 
        
      }
      
      valueBox(
        paste0("$",ceiling(finalValue/1000000),"M"),
        "Predicted Maximum Revenue", icon = icon("money"),
        color = "green"
      )
    })
  
}



shinyApp(ui, server)