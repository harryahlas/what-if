
options(scipen=999)

library(tidyverse)
library(scales)
library(shiny)
library(DT)


# Main data
df <- read_csv("process_name,true_positives,false_negatives,false_positives,true_negatives
    ideal_state,10000,0,0,90000
    current_state,10000,0,90000,0
    wide_net,9900,100,86150,3850
    medium_net,7500,2500,40000,50000
    narrow_net,2000,8000,2500,87500
    goal,8000,2000,30000,60000") 

ui <- fluidPage(
  
  titlePanel("What if scenarios"),
  sidebarLayout(
    sidebarPanel(width = 3,
      
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
      
      # Input: text input for the work days per year ----
      textInput(inputId = "workdaysEachYear",
                label = "Estimated work days each year:",
                value = 200),
      
      textOutput("Cost per minute = (Manual reviewer annual cost / workdays each year) / ( 8 hours * 60 minutes )
                 \nTotal cost = (Cost per minute x cost of each missed referral) + (Cost per minute x cost of each manual review)")
    ),
    mainPanel(
      plotOutput(outputId = "distPlot"),
      DT::dataTableOutput(outputId = "comparisonTable")
    )
  )
)

server <- function(input, output) {
  
  plotData <- reactive({
    workdays_each_year <- as.numeric(input$workdaysEachYear)  # 220 average
    manual_review_cost_per_year <- as.numeric(input$manualReviewCostPerYear)                                           # annual salary/benefits etc
    manual_review_cost_per_day <- manual_review_cost_per_year / workdays_each_year    # daily salary
    manual_review_cost_per_minute <- manual_review_cost_per_day / (8 * 60)            # salary by minute
    minutes_per_review <- as.numeric(input$minutesPerManualReview)                                                           # minutes each review takes
    cost_per_false_positive <- minutes_per_review * manual_review_cost_per_minute
    
    df %>% 
      mutate(total_positives = true_positives + false_negatives,
             total_negatives = false_positives + true_negatives,
             total_reviews = total_positives + total_negatives,
             cost_of_false_positives = cost_per_false_positive * false_positives,
             cost_of_false_negatives = as.numeric(input$costPerFalseNegative) * false_negatives,
             total_cost = cost_of_false_positives + cost_of_false_negatives) %>% 
      mutate_if(is.numeric, as.integer, 0) %>% 
      mutate(cost_of_false_positives = dollar(cost_of_false_positives),
             cost_of_false_negatives = dollar(cost_of_false_negatives),
             total_cost = dollar(total_cost)) %>% 
      select(process_name, total_cost, everything()) 
  })
  
  output$distPlot <- renderPlot({
    plotData() %>%
      ggplot(aes(process_name, total_cost)) +
      geom_col() + 
      theme(text = element_text(size=20)) +
            #axis.text.x = element_text(angle=90, hjust=1)) +
      coord_flip() +
      labs(x = "Total cost", y = "Process Name")
  })
  
  output$comparisonTable <- DT::renderDataTable({ 
    DT::datatable(plotData(), 
                  rownames= FALSE, 
                  options = list(bFilter = 0,     #remove filter
                                 bLengthChange=0))#remove number of items to display dropdown
  })
  
}

shinyApp(ui, server)
