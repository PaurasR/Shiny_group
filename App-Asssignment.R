library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(DT)
library(waiter)

DIG <- read.csv("DIG.csv")
DIG <- DIG %>% filter(!is.na(AGE) & !is.na(DEATH))  

DIG$TRTMT <- factor(DIG$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
DIG$SEX <- factor(DIG$SEX, levels = c(1, 2), labels = c("Male", "Female"))
DIG$RACE <- factor(DIG$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Clinical Measures", tabName = "clinical", icon = icon("stethoscope")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    use_waiter(),  
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Dataset Overview", 
            width = 12, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            dataTableOutput("overviewTable")
          )
        )
      ),
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            title = "Demographic Filters", 
            width = 4, 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            sliderInput("ageRange", "Select Age Range:", 
                        min = min(DIG$AGE), max = max(DIG$AGE), 
                        value = c(min(DIG$AGE), max(DIG$AGE))),
            checkboxGroupInput("genderFilter", "Select Gender:", 
                               choices = levels(DIG$SEX), 
                               selected = levels(DIG$SEX)),
            checkboxGroupInput("raceFilter", "Select Race:", 
                               choices = levels(DIG$RACE), 
                               selected = levels(DIG$RACE))
          ),
          box(
            title = "Age Distribution", 
            width = 8, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("agePlot")
          )
        ),
        fluidRow(
          box(
            title = "Race Distribution", 
            width = 12, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("racePlot")
          )
        )
      ),
      
      tabItem(
        tabName = "clinical",
        fluidRow(
          box(
            title = "Dynamic Variable Selection", 
            width = 4, 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("xVar", "Select X-axis Variable:", 
                        choices = c("BMI", "SYSBP", "DIABP"), 
                        selected = "BMI"),
            selectInput("yVar", "Select Y-axis Variable:", 
                        choices = c("SYSBP", "DIABP", "BMI"), 
                        selected = "SYSBP")
          ),
          box(
            title = "Clinical Measure Scatter Plot", 
            width = 8, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("clinicalPlot")
          )
        )
      ),
      
      tabItem(
        tabName = "outcomes",
        fluidRow(
          box(
            title = "Mortality by Treatment", 
            width = 6, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("deathPlot")
          ),
          box(
            title = "Summary Table", 
            width = 6, 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            dataTableOutput("summaryTable")
          )
        )
      )
    )
  )
)

# Server Section - Define what happens when users interact with the app 


server <- function(input, output, session) {
  
  # Show the loading animation while the app loads
  
  waiter_show(html = tags$img(src = "https://media.giphy.com/media/5VK6e0F2DFwFEdt0Er/giphy.gif"))
  
  
  
  
  # Overview: Show the data table
  
  
  output$overviewTable <- renderDataTable({
    datatable(DIG)
  })
  
  
  output$agePlot <- renderPlot({
    filtered_data <- DIG %>%
      filter(AGE >= input$ageRange[1], AGE <= input$ageRange[2],
             SEX %in% input$genderFilter, RACE %in% input$raceFilter)
    
    ggplot(filtered_data, aes(x = AGE)) +
      geom_histogram(binwidth = 5, fill = "#4CAF50", color = "#2c3e50") +
      labs(title = "Age Distribution", x = "Age", y = "Frequency") +
      theme_minimal()
  })
  
  output$racePlot <- renderPlot({
    filtered_data <- DIG %>%
      filter(SEX %in% input$genderFilter, RACE %in% input$raceFilter)
    
    ggplot(filtered_data, aes(x = RACE, fill = RACE)) +
      geom_bar() +
      labs(title = "Race Distribution", x = "Race", y = "Count") +
      scale_fill_manual(values = c("#3498db", "#f39c12")) +
      theme_minimal()
  })
  
  
  output$clinicalPlot <- renderPlot({
    ggplot(DIG, aes_string(x = input$xVar, y = input$yVar)) +
      geom_point(color = "#9b59b6", alpha = 0.7) +
      labs(title = paste(input$xVar, "vs", input$yVar), 
           x = input$xVar, y = input$yVar) +
      theme_minimal()
  })
  
  output$deathPlot <- renderPlot({
    ggplot(DIG, aes(x = TRTMT, fill = as.factor(DEATH))) +
      geom_bar(position = "dodge") +
      labs(title = "Mortality by Treatment", x = "Treatment", y = "Count") +
      scale_fill_manual(values = c("#e74c3c", "#2ecc71"), name = "Death (1=Yes, 0=No)") +
      theme_minimal()
  })
  
  output$summaryTable <- renderDataTable({
    DIG %>%
      group_by(TRTMT) %>%
      summarise(
        Total = n(),
        Deaths = sum(DEATH, na.rm = TRUE),
        DeathRate = round(mean(DEATH, na.rm = TRUE) * 100, 2)
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  
  #hide the animation
  
  waiter_hide()
}

# for Running the App
shinyApp(ui = ui, server = server)
