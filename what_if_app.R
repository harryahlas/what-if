
options(scipen=999)

library(tidyverse)
library(scales)
library(shiny)
library(DT)


# Main data
df <- read_csv("process_name,true_positives,false_negatives,false_positives,true_negatives,sensitivity,specificity
    1. Current State,10000,0,90000,0,1,0
    2. Ideal State,10000,0,0,90000,1,1
    3. Wide Net Proposal,9900,100,86150,3850,.99,.043
    4. Medium Net Proposal,7500,2500,40000,50000,.75,.55
    5. Narrow Net Proposal,2000,8000,2500,87500,.2,.97
    6. Goal,8000,2000,30000,60000,.8,.67") 

ui <- fluidPage(
  
  titlePanel("What if scenarios"),
  sidebarLayout(
    sidebarPanel(width = 3,
      
      # Input: text input for the number of texts to be reviewed ----
      textInput(inputId = "numberOfTexts",
                label = "Number of Texts:",
                value = 100000),  
      
      # Input: text input for the number of texts that are positives ----
      textInput(inputId = "numberOfPositives",
                label = "Number of Texts that should be Referred:",
                value = 10000), 
      
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
    total_texts <- as.numeric(input$numberOfTexts)
    positive_texts <- as.numeric(input$numberOfPositives)
    negative_texts <- total_texts - positive_texts
    workdays_each_year <- as.numeric(input$workdaysEachYear)  # 220 average
    manual_review_cost_per_year <- as.numeric(input$manualReviewCostPerYear)                                           # annual salary/benefits etc
    manual_review_cost_per_day <- manual_review_cost_per_year / workdays_each_year    # daily salary
    manual_review_cost_per_minute <- manual_review_cost_per_day / (8 * 60)            # salary by minute
    minutes_per_review <- as.numeric(input$minutesPerManualReview)                                                           # minutes each review takes
    cost_per_false_positive <- minutes_per_review * manual_review_cost_per_minute
    
    df %>% 
      mutate(true_positives = positive_texts * sensitivity,
             false_negatives = positive_texts - true_positives,
             false_positives = negative_texts - (negative_texts * specificity),
             true_negatives = negative_texts * specificity,
             total_positives = true_positives + false_negatives,
             total_negatives = false_positives + true_negatives,
             total_reviews = total_positives + total_negatives,
             cost_of_false_positives = cost_per_false_positive * false_positives,
             cost_of_false_negatives = as.numeric(input$costPerFalseNegative) * false_negatives,
             total_cost = cost_of_false_positives + cost_of_false_negatives) %>% 
      #mutate_if(is.numeric, as.integer, 0) %>% 
      mutate_at(vars(-process_name, -specificity, -sensitivity), as.integer, 0) %>% 
      mutate(cost_of_false_positives = dollar(cost_of_false_positives),
             cost_of_false_negatives = dollar(cost_of_false_negatives),
             `Total Cost` = dollar(total_cost)) %>% 
      select(process_name,`Total Cost`, everything()) 
  })
  
  output$distPlot <- renderPlot({
    plotData() %>%
      ggplot(aes(process_name, total_cost)) +
      geom_col() + 
      theme(text = element_text(size=15)) +
            #axis.text.x = element_text(angle=90, hjust=1)) +
      coord_flip() +
      scale_y_continuous(labels=dollar_format(prefix="$")) +
      labs(x = "Process Name", y = "Total cost")
  })
  
  output$comparisonTable <- DT::renderDataTable({ 
    DT::datatable(plotData() %>% select(-total_cost), 
                  rownames= FALSE, 
                  options = list(bFilter = 0,     #remove filter
                                 bLengthChange=0))#remove number of items to display dropdown
  })
  
}

shinyApp(ui, server)
