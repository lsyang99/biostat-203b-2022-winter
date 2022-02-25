---
  output: html_document
runtime: shiny
---
  # Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(shinythemes)
library(magrittr)

# Load data ----
icu_cohort <- readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

shinyApp(
  
  ui = fluidPage(
    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("cosmo"),
    navbarPage(
      
      "BIOSTAT 203B-HW3",
      tabPanel("Data",  # 1. 
               h3("Data Preview"),
               fluidRow(
                 column(4,
                        selectInput("age_cate",
                                    "Age:",
                                    c("All",
                                      unique(as.character(icu_cohort$age_cate))))
                 ),
                 column(4,
                        selectInput("gender",
                                    "Gender:",
                                    c("All",
                                      unique(as.character(icu_cohort$gender))))
                 ),
                 column(4,
                        selectInput("ethnicity",
                                    "Ethnicity:",
                                    c("All",
                                      unique(as.character(icu_cohort$ethnicity))))
                 ),
                 column(4,
                        selectInput("thirty_day_mort",
                                    "30-Day Mortality:",
                                    c("All",
                                      unique(as.character(icu_cohort$thirty_day_mort))))
                 )
               ),
               # Create a new row for the table.
               DT::dataTableOutput("table")
      ),
      
      tabPanel("Demographics",  # 2.
               h3("Barplot for demographics, admission time (year/month/week day/hour), and 30-day mortality"),
               sidebarPanel(
                 selectInput("var1", "Choose a variate:", choices = c("age_cate", "gender", "insurance", "language", "marital_status", "ethnicity", "thirty_day_mort")),
                 selectInput("var2", "Choose a covariate:", choices = c("age_cate", "gender", "insurance", "language", "marital_status", "ethnicity", "thirty_day_mort", "year", "month", "wday", "hour"))
               ),
               
               mainPanel(
                 #h4("Plot"),
                 plotOutput("BarPlot")
               )
      ),
      
      tabPanel("Measurements",  # 3.
               h3("Summary of Lab/Vital Measurements"),
               sidebarPanel( 
                 selectInput("meas", "Choose a measurement:", choices = c("sodium", "hematocrit", "wbcc", "chloride", "bicarbonate", "glucose", "calcium", "potassium", "creatinine", "magnesium", "snibp", "body_temp", "heart_rate", "resp", "mnibp")),
                 selectInput("var", "Choose a covariate:", choices = c("N/A", "age_cate", "thirty_day_mort", "gender", "ethnicity")),
                 
               ),
               mainPanel(
                 h4("Plot"),
                 plotOutput("HistogramPlot"),
                 # Output: Header + summary of distribution ----
                 h4("Summary"),
                 verbatimTextOutput("summary")
               )
      )
    )
  ),
  server = function(input, output) {
    # 1.
    output$table <- DT::renderDataTable(DT::datatable({
      data <- icu_cohort
      if (input$age_cate != "All") {
        data <- data[data$age_cate == input$age_cate,]
      }
      if (input$gender != "All") {
        data <- data[data$gender == input$gender,]
      }
      if (input$ethnicity != "All") {
        data <- data[data$ethnicity == input$ethnicity,]
      }
      if (input$thirty_day_mort != "All") {
        data <- data[data$thirty_day_mort == input$thirty_day_mort,]
      }
      data
    }))
    # 2.
    output$BarPlot <- renderPlot({
      if(input$var2 %in% c("month", "hour")){
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$var2]], fill = .data[[input$var1]])) + 
          geom_bar() + 
          labs(title = "") +
          xlab(input$var2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_continuous(breaks = seq(min(icu_cohort[, input$var2]), max(icu_cohort[, input$var2]), by = 1))
      }else if(input$var2 %in% c("year")){
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$var2]], fill = .data[[input$var1]])) + 
          geom_bar() + 
          labs(title = "") +
          xlab(input$var2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_continuous(breaks = seq(min(icu_cohort[, input$var2]), max(icu_cohort[, input$var2]), by = 10))
      }else if(input$var2 %in% c("wday")){
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$var2]], fill = .data[[input$var1]])) + 
          geom_bar() + 
          labs(title = "") +
          xlab(input$var2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }else if(input$var1 == "ethnicity" | input$var2 == "ethnicity"){
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$var2]], fill = .data[[input$var1]])) + 
          geom_bar(position = "dodge2") + 
          labs(title = "") +
          xlab(input$var2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_discrete(guide = guide_axis(angle = 90))
      }
      else{
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$var2]], fill = .data[[input$var1]])) + 
          geom_bar(position = "dodge2") + 
          labs(title = "") +
          xlab(input$var2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    # 3.
    output$HistogramPlot <- renderPlot({
      if(input$var == "N/A"){
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$meas]])) + 
          geom_freqpoly(size = 0.9) + 
          labs(xlab = input$var, ylab = "Count") + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }else{
        icu_cohort %>% 
          ggplot(aes(x = .data[[input$meas]], color = factor(as.character(.data[[input$var]])))) + 
          geom_freqpoly(size = 0.9) + 
          labs(xlab = input$var, ylab = "Count", color = input$var) + 
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    output$summary <- renderPrint({
      if(input$var == "N/A"){
        summary(icu_cohort[, input$meas])
      }else{
        by(icu_cohort[, input$meas], icu_cohort[, input$var], summary)
      }
    })
    
  }
  
)
# 
# ###############################################################################
# # User interface ----
# ui <- fluidPage(
#   titlePanel("censusVis"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Create demographic maps with 
#                information from the 2010 US Census."),
#       
#       selectInput("var", 
#                   label = "Choose a variable to display",
#                   choices = c("Percent White", "Percent Black",
#                               "Percent Hispanic", "Percent Asian"),
#                   selected = "Percent White"),
#       
#       sliderInput("range", 
#                   label = "Range of interest:",
#                   min = 0, max = 100, value = c(0, 100))
#     ),
#     
#     mainPanel(plotOutput("map"))
#   )
# )
# 
# # Server logic ----
# # server function is run once each time a user visits the app
# server <- function(input, output) {
#   # renderPlot is run once each time a user changes a widget that output$map depends on
#   output$map <- renderPlot({
#     data <- switch(input$var, 
#                    "Percent White" = counties$white,
#                    "Percent Black" = counties$black,
#                    "Percent Hispanic" = counties$hispanic,
#                    "Percent Asian" = counties$asian)
#     
#     color <- switch(input$var, 
#                     "Percent White" = "darkgreen",
#                     "Percent Black" = "black",
#                     "Percent Hispanic" = "darkorange",
#                     "Percent Asian" = "darkviolet")
#     
#     legend <- switch(input$var, 
#                      "Percent White" = "% White",
#                      "Percent Black" = "% Black",
#                      "Percent Hispanic" = "% Hispanic",
#                      "Percent Asian" = "% Asian")
#     
#     percent_map(data, color, legend, input$range[1], input$range[2])
#   })
# }
# 
# # Run app ----
# shinyApp(ui, server)
