library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(DT)
library(waiter)

DIG <- read.csv("DIG.csv")
DIG <- DIG %>% filter(!is.na(AGE) & !is.na(DEATH))

# Convert categorical variables to factors
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
)
dashboardBody(
  use_waiter(),  # Using waiter package to show a loading animation
  tabItems(
    # Overview Tab
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
    
  )))

