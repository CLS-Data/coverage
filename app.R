library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(shiny)
# library(reactable)
# library(shinyjs)  # Add shinyjs for extended capabilities
library(markdown)

rm(list = ls())

load("shiny_data.Rdata")


ui <- fluidPage(
  titlePanel("Coverage in the CLS Cohort Studies"),
  
  tabsetPanel(
    tabPanel("Background",
             fluidRow(
               column(12, includeMarkdown("background.md")
               )
             )),
    
    tabPanel("Variable and Category Selection",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Variable:", choices = levels(df_count$name_clean)),
                 uiOutput("category_ui"),
                 actionButton("update", "Update Table")
               ),
               
               mainPanel(
                 tableOutput("count_table")
               )
             )),
    
    tabPanel("Age Range Selection",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_range", "Select Variable:", choices = levels(df_range$name_clean)),
                 sliderInput("age_range", "Select Age Range:",
                             min = min(df_range$low), max = max(df_range$high),
                             value = c(min(df_range$low), max(df_range$high)),
                             step = 1, round = TRUE),
                 checkboxGroupInput("study_range", "Combine Studies:", choices = unique(df_range$study_clean)),
                 actionButton("update_range", "Update Table")
               ),
               
               mainPanel(
                 tableOutput("range_table")
               )
             )),
    
    tabPanel("Notes",
             fluidRow(
               column(12,
                      includeMarkdown("notes.md") #, reactableOutput("interactive_table")
               )
             ))
    
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to filter categories based on selected variable
  categories <- reactive({
    req(input$variable)
    
    df_count %>%
      filter(name_clean == input$variable) %>%
      left_join(df_levels, by = c("name_clean", "value")) %>%
      mutate(value = fct_drop(value) %>% fct_reorder(level)) %>%
      pull(value) %>%
      levels()
  })
  
  # Dynamically generate the category selection UI
  output$category_ui <- renderUI({
    req(categories())
    
    checkboxGroupInput("selected_categories", "Select Categories:", choices = categories())
  })
  
  # Reactive expression to filter data based on user selections
  filtered_data <- reactive({
    req(input$variable, input$selected_categories)
    
    df_count %>%
      filter(name_clean == input$variable, value %in% input$selected_categories)
  })
  
  # Observe the update button and create the summary table for variable and category selection
  observeEvent(input$update, {
    output$count_table <- renderTable({
      req(filtered_data())
      
      df_count %>%
        filter(name_clean == input$variable, 
               value %in% input$selected_categories) %>%
        group_by(study_clean, fup) %>%
        summarise(total_n = sum(n),
                  .groups = "drop") %>%
        mutate(total_n = format(total_n, big.mark = ",") %>% str_trim()) %>%
        select(study_clean, fup, total_n) %>%
        arrange(study_clean) %>%
        pivot_wider(names_from = study_clean, values_from = total_n, values_fill = "") %>%
        arrange(fup) %>%
        mutate(fup = as.integer(fup)) %>%
        rename(`Follow-Up` = fup)
    })
  })
  
  # Reactive expression to filter data based on age range and selected study
  filtered_range_data <- reactive({
    req(input$age_range, input$variable_range, input$study_range)
    
    df_range %>%
      filter(low == input$age_range[1], 
             high == input$age_range[2], 
             name_clean == input$variable_range,
             study_clean %in% input$study_range)
  })
  
  # Observe the update button and create the summary table for age range selection
  observeEvent(input$update_range, {
    output$range_table <- renderTable({
      req(filtered_range_data())
      
      correct_levels <- setdiff(var_levels[[input$variable_range]], "No")
      
      df_filtered <- df_range %>%
        filter(name_clean == !!input$variable_range,
               study_clean %in% input$study_range) %>%
        left_join(df_levels, by = c("name_clean", "value")) %>%
        mutate(value = fct_drop(value) %>% fct_reorder(level), 
               #value = fct_drop(value) %>% factor(levels = !!correct_levels),
               study_clean = fct_drop(study_clean)) %>%
        select(-name_clean, -level) %>%
        complete(low, high, study_clean, value, fill = list(n = 0)) %>%
        filter(low == input$age_range[1], 
               high == input$age_range[2])
      
      df_total <- df_filtered %>%
        group_by(value) %>%
        summarise(n = sum(n)) %>%
        mutate(study_clean = factor("Total", "Total"))
      
      df_filtered %>%
        select(study_clean, value, n) %>%
        bind_rows(df_total) %>%
        mutate(total_n = format(n, big.mark = ",") %>% str_trim()) %>%
        select(study_clean, value, total_n) %>%
        arrange(study_clean) %>%
        pivot_wider(names_from = study_clean, values_from = total_n, values_fill = "") %>%
        arrange(value) %>%
        rename(`Category` = value)
    })
  })
  
}

shinyApp(ui, server)



