
library("shiny")
library(shinythemes)

shinyApp(
  ui = navbarPage(theme = shinytheme("united"),strong("AL & AL Construction"),
                    tabPanel("Model Predictions",
                             sidebarLayout(
                               sidebarPanel(
                                 
                                 titlePanel("Enter the Required Information"),
                                 selectInput("estTime", "Estimated Time Until Completion:",
                                             c("Quick (< 1 Year)" = "tQuick",
                                               "Medium (1-5 Years)" = "tMedium",
                                               "Long (> 5 Years)" = "tLong")),
                                 selectInput("estCost", "Estimated Cost of Project:",
                                             c("Low (< $100 Million)" = "cLow",
                                               "Medium ($100 - $300 Million)" = "cMedium",
                                               "High (> $300 Million" = "cHigh")),
                                 titlePanel("Who Made Bids?"),
                                 radioButtons("compC", "Competitor C",
                                              c("Yes" = "Yes",
                                                "No" = "No")),
                                 radioButtons("compE", "Competitor E",
                                              c("Yes" = "Yes",
                                                "No" = "No")),
                                 radioButtons("compF", "Competitor F",
                                              c("Yes" = "Yes",
                                                "No" = "No")),
                                 actionButton("calcButton", "Calculate")
                                
                               ),
                               mainPanel(
                                 h2(textOutput("description")),
                                 h3(textOutput("result")),
                                 h2(textOutput("bid"))
                               )
                             )
                    ),
                    tabPanel("Data Exploration",
                             sidebarLayout(
                               sidebarPanel(
                                selectInput("xaxis", "Choose the horizontal axis",
                                         c("Estimated Cost (Millions)" = "Estimated_Cost__Millions_",
                                           "Estimated Years Till Complete" = "Estimated_Years_to_Complete",
                                           "AL & AL Bid Price" = "Bid_Price__Millions_",
                                           "Winning Bid Price" = "Winning_Bid_Price__Millions_",
                                           "Region of Country" = "Region_of_Country",
                                           "Number of Competitor Bids" = "Number_of_Competitor_Bids",
                                           "Win Bid" = "Win_Bid"
                                           )),
                                 selectInput("yaxis", "Choose the vertical axis",
                                             c("Estimated Cost (Millions)" = "Estimated_Cost__Millions_",
                                               "Estimated Years Till Complete" = "Estimated_Years_to_Complete",
                                               "AL & AL Bid Price" = "Bid_Price__Millions_",
                                               "Winning Bid Price" = "Winning_Bid_Price__Millions_",
                                               "Region of Country" = "Region_of_Country",
                                               "Number of Competitor Bids" = "Number_of_Competitor_Bids",
                                               "Win Bid" = "Win_Bid"
                                             ))
                             ),
                                mainPanel(
                                  plotOutput("dataExplore")
                                )
                              )
                    )
),
  server = function(input, output) { 
    constData <- read.csv("Construction.csv")
    Intercept = -2.7262
    estCostHigh = 2.9401
    estCostMedium = 1.8423
    estTimeLong = 1.2643
    estTimeMedium = -0.4993
    compC = 1.9871
    compE = -1.1128
    compF = -1.2818
    cutoff = 0.085766
    
    cHighDummy = 0
    cMediumDummy = 0
    tLongDummy = 0
    tMediumDummy = 0
    compCDummy = 0
    compEDummy = 0
    compFDummy = 0
    
    observeEvent(input$calcButton,{
      if(input$estTime=="tQuick"){
        tLongDummy = 0
        tMediumDummy = 0
      } else if(input$estTime == "tMedium") {    
        tLongDummy = 0
        tMediumDummy = 1
      }else{
        tLongDummy = 1
        tMediumDummy = 0
      }
      if(input$estCost=="cLow"){
        cHighDummy = 0
        cMediumDummy = 0
      }else if(input$estCost=="cMedium"){
        cHighDummy = 0
        cMediumDummy = 1
      }else{
        cHighDummy = 1
        cMediumDummy = 0
      }
      if(input$compC=="Yes"){
        compCDummy=1
      }else{
        compCDummy=0
      }
      if(input$compE=="Yes"){
        compEDummy=1
      }else{
        compEDummy=0
      }
      if(input$compF=="Yes"){
        compFDummy=1
      }else{
        compFDummy=0
      }
      message = ""
      
      
      Prob <- 1/(1+exp(-Intercept - estCostHigh*cHighDummy - estCostMedium*cMediumDummy - estTimeLong*tLongDummy - estTimeMedium*tMediumDummy - compC*compCDummy - compE*compEDummy - compF*compFDummy))
      if(Prob > cutoff){
        message = "Yes, we suggest you make a bid!"
      }else{
        message = "Hold out on this project, let's wait for another one."
      }
      
      output$description <- renderText({"The probability of winning the given bid is:"})
      output$result <- renderText({paste(round(Prob,4)*100,"%",sep="") })
      output$bid <- renderText({message})
      
    })
    observe({
      input$estCost
      input$estTime
      input$compC
      input$compE
      input$compF
      output$description <- renderText({})
      output$result <- renderText({" "})
      output$bid <- renderText({""})
    })
    
    #server code for data exploration panel
    selectedData <- reactive({
      constData[, c(input$xaxis, input$yaxis)]
    })
    
    observe({
      input$xaxis
      input$yaxis
      
      output$dataExplore <- renderPlot(
        
        plot(selectedData(), main="Construction Project History Data Set", 
             xlab=input$xaxis, ylab=input$yaxis)
      )
    })
    
    
    
    
    
  }
)



