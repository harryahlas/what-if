
options(scipen=999)

library(tidyverse)
library(scales)
library(shiny)
library(DT)






#RV <- reactiveValues(data = df)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("What if scenarios"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the annual cost of each reviewer ----
      sliderInput(inputId = "manualReviewCostPerYear",
                  label = "Annual Total Cost of Manual Reviewer (Salary/Benefits/etc) in dollars:",
                  min = 25000,
                  max = 150000,
                  value = 75000),
      
      # Input: text input for the cost of each manual review ----
      textInput(inputId = "minutesPerManualReview",
                  label = "Minutes required per review:",
                  value = 4),

      # Input: text input for the cost of each false negative ----
      textInput(inputId = "costPerFalseNegative",
                label = "Estimated cost of each missed referral in dollars:",
                value = 500),
      
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      actionButton("refresh", "Refresh")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      tableOutput(outputId = "comparisonTable")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  loadData <- function() {
#     df <- reactive({
#       read_csv("process_name,true_positives,false_negatives,false_positives,true_negatives
# ideal_state,10000,0,0,90000
# current_state,10000,0,90000,0
# wide_net,9900,100,86150,3850
# medium_net,7500,2500,40000,50000
# narrow_net,2000,8000,2500,87500
# goal,8000,2000,30000,60000") %>% 
#         mutate(total_positives = true_positives + false_negatives,
#                total_negatives = false_positives + true_negatives,
#                total_reviews = total_positives + total_negatives,
#                cost_of_false_positives = cost_per_false_positive * false_positives,
#                cost_of_false_negatives = cost_per_false_negative * false_negatives,
#                total_cost = dollar(cost_of_false_positives + cost_of_false_negatives))
#       df
      cars
  #})
  }
  
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$comparisonTable <- renderTable({ 
    
    
    
    workdays_each_year <- 220                                                         # 220 average
    manual_review_cost_per_year <- 100000  #input$manualReviewCostPerYear                                           # annual salary/benefits etc
    manual_review_cost_per_day <- manual_review_cost_per_year / workdays_each_year    # daily salary
    manual_review_cost_per_minute <- manual_review_cost_per_day / (8 * 60)            # salary by minute
    minutes_per_review <- 5 #input$minutesPerManualReview                                                           # minutes each review takes

    cost_per_false_positive <- minutes_per_review * manual_review_cost_per_minute

    cost_per_false_negative <- as.numeric(input$costPerFalseNegative) # Cost of each missed positive
    
    df <- read_csv("process_name,true_positives,false_negatives,false_positives,true_negatives
    ideal_state,10000,0,0,90000
    current_state,10000,0,90000,0
    wide_net,9900,100,86150,3850
    medium_net,7500,2500,40000,50000
    narrow_net,2000,8000,2500,87500
    goal,8000,2000,30000,60000") %>%
        mutate(total_positives = true_positives + false_negatives,
               total_negatives = false_positives + true_negatives,
               total_reviews = total_positives + total_negatives,
               cost_of_false_positives = cost_per_false_positive * false_positives,
               cost_of_false_negatives = cost_per_false_negative * false_negatives,
               total_cost = dollar(cost_of_false_positives + cost_of_false_negatives))
      df

    # input$refresh
    # loadData()
    })#renderDataTable({df})
  
   


  #   observeEvent(input$costPerFalseNegative, {
  #     cost_per_false_negative(input$costPerFalseNegative)    # newValue <- rv$value + 1
  #   
  # })
}

shinyApp(ui, server)
