#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("James' Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Quan's Barchart", tabName = "barchart", icon = icon("th")),
      menuItem("Blending", tabName = "blending", icon = icon("th")),
      menuItem("Map", tabName = "map", icon = icon("th")),
      menuItem("Table", tabName = "table", icon = icon("th"))
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
      tabItem(tabName = "blending",
              actionButton(inputId = "clicks3",  label = "Click me"),
              plotOutput("distPlot3")
      ),
      
      # Fourth tab content
      tabItem(tabName = "map",
              leafletOutput("map")
      ),
      
      # Fifth tab content
      tabItem(tabName = "table",
              dataTableOutput("table")
      )
    )
  )
)
