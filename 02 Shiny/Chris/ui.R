#aggregated ui.R 

library(shiny)
library(shinydashboard)
library(leaflet)
library(RCurl)
library("jsonlite")



dashboardPage(
  skin = "green",
  dashboardHeader(title = "Refugee Movements: A Recent History", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Normalized Refugee Intakes", tabName = "chris_linegraph", icon = icon("line-chart")),
      menuItem("Quan's Graph", tabname = "quan_crosstab", icon = icon("server")),
      menuItem("James' Graph", tabName = "james_bargraph", icon = icon("bar-chart")) 
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "chris_linegraph",
        # inputs defined here
        sliderInput("refs",
                    "Refugee Population Size",
                    min = 0,
                    max = 4000000, 
                    value = c(0, 4000000), 
                    step = 10000, 
                    round = TRUE),
        sliderInput("hosts",
                    "Host Country Population",
                    min = 0,
                    max = 1400000000, 
                    value = c(0, 1400000000), 
                    step = 500000, 
                    round = TRUE),
        actionButton("clicks", "Generate Plot"),
        plotOutput("normal_refs")
      ),
      tabItem(
        tabName = "quan_crosstab"
        # slider inputs defined here
        #actionButton("quan_plot", "Generate Plot"),
        #plotOutput("QUAN")
      ),
      tabItem(
        tabName = "james_bargraph"
        # slider inputs defined here
        #actionButton("james_plot", "Generate Plot"),
        #plotOutput("JAMES")
      )
    )
  )
  )

