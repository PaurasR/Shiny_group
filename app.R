#Load the necessary libraries rquired for building the app
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

#for data
library(dplyr)
library(here)

#for plots creation
library(ggplot2)
library(DT)

#for animation
library(waiter)

#load DIG dataset
DIG <- read.csv(here::here("data","DIG.csv"))

DIG <- DIG %>% filter(!is.na(AGE) & !is.na(DEATH))  #remove missing values 

#converting specific values in dataset to categorical values for easier use in plots and filters
DIG$TRTMT <- factor(DIG$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
DIG$SEX <- factor(DIG$SEX, levels = c(1, 2), labels = c("Male", "Female"))
DIG$RACE <- factor(DIG$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))

#UI desion section layout beginning
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Dashboard"), # App title
  dashboardSidebar(
    sidebarMenu( #Sidebar menu for better user experience 
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Clinical Measures", tabName = "clinical", icon = icon("stethoscope")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    use_waiter(),  #animation for UI interactive
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
            dataTableOutput("overviewTable") #display the dataset in table format
          )
        )
      ),
      tabItem( #filter function for age, race and gender
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
                        value = c(min(DIG$AGE), max(DIG$AGE))), #Age upper and lower bounds
            checkboxGroupInput("genderFilter", "Select Gender:", 
                               choices = levels(DIG$SEX), 
                               selected = levels(DIG$SEX)), #Sex filter
            checkboxGroupInput("raceFilter", "Select Race:", 
                               choices = levels(DIG$RACE), 
                               selected = levels(DIG$RACE)) #Race Fliter
          ),
          #plot for age distribution
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
        #plot for race distribution
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
      #Clinical observed data
      tabItem(
        tabName = "clinical",
        fluidRow(
          box(
            title = "Dynamic Variable Selection", 
            width = 4, 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            #Select X and Y coordinate variables
            selectInput("xVar", "Select X-axis Variable:", 
                        choices = c("BMI", "SYSBP", "DIABP"), 
                        selected = "BMI"),
            selectInput("yVar", "Select Y-axis Variable:", 
                        choices = c("SYSBP", "DIABP", "BMI"), 
                        selected = "SYSBP")
          ),
          #Generating scatter plot for clinical observed data
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
      #generate plots showing death count by treatment groups
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
          #Generating Table containing summary of deaths by treatment groups
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

server <- function(input, output, session) {

  waiter_show(html = tags$img(src = "https://media.giphy.com/media/5VK6e0F2DFwFEdt0Er/giphy.gif"))
  
  output$overviewTable <- renderDataTable({
    datatable(
      DIG,
      options = list(scrollX = TRUE)  
    )
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

shinyApp(ui = ui, server = server)
