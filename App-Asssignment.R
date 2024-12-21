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
