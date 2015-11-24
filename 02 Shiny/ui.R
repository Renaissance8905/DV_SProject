#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "Refugee Movements: A Recent History", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("James' Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Quan's Barchart", tabName = "barchart", icon = icon("bar-chart")),
      menuItem("Chris' Blended Lines", tabName = "blending", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
              sliderInput("KPI1", "KPI_Low_Max_value:", 
                          min = 0, max = 1,  value = 0.8),
              sliderInput("KPI2", "KPI_Medium_Max_value:", 
                          min = 1, max = 2,  value = 1.2),
              textInput(inputId = "title", 
                        label = "Crosstab Title",
                        value = "Asylum Preferences by Origin Country"),
              plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
              plotOutput("distPlot2")
      ),
      
      # Third tab content
      tabItem(
        tabName = "blending",
        # inputs defined here
        sliderInput("refs",
                    "Refugee Population Size",
                    min = 0,
                    max = 4000000, 
                    value = c(10, 4000000), 
                    step = 10000, 
                    round = TRUE),
        sliderInput("hosts",
                    "Host Country Population",
                    min = 0,
                    max = 1400000000, 
                    value = c(10, 1400000000), 
                    step = 500000, 
                    round = TRUE),
        actionButton("clicks", "Generate Plot"),
        plotOutput("normal_refs")
      )
    )
  )
)
