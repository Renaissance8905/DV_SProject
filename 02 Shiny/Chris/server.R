###################
# CHRIS' server.R #
###################

require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {

  REFUGEE_LOW <- reactive({as.numeric(input$refs[1])})
  REFUGEE_HIGH <- reactive({as.numeric(input$refs[2])})
  HOST_LOW <- reactive({as.numeric(input$host[1])})
  HOST_HIGH <- reactive({as.numeric(input$host[2])})
  
      
  norm_refs <- eventReactive(input$clicks, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"SELECT asylum_country, record_year, (all_refugees / current_pop * 100) normalized_refugees FROM (SELECT asylum_country, record_year, SUM(total_population) all_refugees, x2014 current_pop FROM (select * from REFUGEE_STATS LEFT JOIN (select cname, continent_code from COUNTRIES) co ON co.cname = REFUGEE_STATS.asylum_country WHERE REFUGEE_STATS.asylum_country != \\\'Various/Unknown\\\' AND REFUGEE_STATS.total_population > "RL" AND REFUGEE_STATS.total_population < "RH" ) refs INNER JOIN (select * from YEARLY_POP_BY_COUNTRY_NUMERIC WHERE X2014 > "HL" AND X2014 < "HH" ) pops ON pops.country_name = refs.asylum_country GROUP BY asylum_country, record_year, x2014);"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', RL = REFUGEE_LOW(), RH = REFUGEE_HIGH(), HL = HOST_LOW(), HH = HOST_HIGH()), verbose = TRUE)))
    })
      

  output$normal_refs <- renderPlot({
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Refugee Influxes Over Time, Normalized for Host Populations') +
      labs(x=paste("Year"), y=paste("Yearly Refugee Influx as % of Host Population")) +
      layer(data=norm_refs(), 
            mapping=aes(x=RECORD_YEAR, y=NORMALIZED_REFUGEES, color=ASYLUM_COUNTRY), 
            stat="identity", 
            stat_params=list(), 
            geom="line",
            geom_params=list(), 
            position=position_identity()
      )
    plot
  })
    observeEvent(input$refs, {print(input$refs[1])})
    observeEvent(input$refs, {print(input$refs[2])})
    observeEvent(input$hosts, {print(input$hosts[1])})
    observeEvent(input$hosts, {print(input$hosts[1])})
    
})
