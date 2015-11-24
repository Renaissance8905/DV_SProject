# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  output$distPlot1 <- renderPlot(width=1100,{
    # Start your code here.
    
    # The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"
    
    KPI_Low_Max_value = input$KPI1     
    KPI_Medium_Max_value = input$KPI2
    
    #getting data for Germany, filter by 
    data_GER <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="""select Asylum_Country, Origin_Country, sum(Total_Population) as Tot_Ger, sum(Asylum_Seekers) as Asy_Ger, sum(Refugees) as Ref_Ger from REFUGEE_STATS where Asylum_Country = \\\'Germany\\\' group by Asylum_Country, Origin_Country; """')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE)));
    
    #getting Data for USA
    data_USA <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="""select Asylum_Country, Origin_Country, sum(Total_Population) as Tot_USA, sum(Asylum_Seekers) as Asy_USA, sum(Refugees) as Ref_USA from REFUGEE_STATS where Asylum_Country = \\\'United States of America\\\'group by Asylum_Country, Origin_Country;
                                                          """')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); 
    
    #join the two tables
    data_all <- dplyr::inner_join(data_GER, data_USA, by = "ORIGIN_COUNTRY")
    
    data_final <- data_all %>% select (ORIGIN_COUNTRY, ASY_GER, REF_GER, ASY_USA, REF_USA) %>% filter (as.numeric(as.character(ASY_GER)) > -1, as.numeric(as.character(ASY_USA)) > -1, as.numeric(as.character(REF_GER)) > -1, as.numeric(as.character(REF_USA)) > -1) %>% mutate("ref_per_asy_GER" = as.numeric(as.character(REF_GER))/as.numeric(as.character(ASY_GER)), "ref_per_asy_USA" = as.numeric(as.character(REF_USA)) / as.numeric(as.character(ASY_USA)) ) %>% arrange(ORIGIN_COUNTRY) %>% filter("ref_per_asy_GER" > -1, "ref_per_asy_USA" > -1) 
    
    #mutate in KPI
    data_final <- data_final %>% mutate (KPI = ifelse((ref_per_asy_GER/ref_per_asy_USA) < KPI_Low_Max_value, "GER favored Asylum", ifelse ((ref_per_asy_GER/ref_per_asy_USA) < KPI_Medium_Max_value, "Neutral", ifelse((ref_per_asy_GER/ref_per_asy_USA) > KPI_Medium_Max_value, "USA favored Asylum", "N/A"))))
    
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Asylum Preference of US vs Germany by Origin') +
      labs(x=paste("Values"), y=paste("ORIGIN_COUNTRY")) +
      theme(axis.text.y=element_text(size = 6))+ 
      theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5))+
      layer(data=data_final, 
            mapping=aes(x="ASY_GER", y=as.character(ORIGIN_COUNTRY), label=as.character(ASY_GER)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size=2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="REF_GER", y=as.character(ORIGIN_COUNTRY), label=as.character(REF_GER)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ASY_USA", y=as.character(ORIGIN_COUNTRY), label=as.character(ASY_USA)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="REF_USA", y=as.character(ORIGIN_COUNTRY), label=as.character(REF_USA)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ASY_USA", y=as.character(ORIGIN_COUNTRY), label=as.character(ASY_USA)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ref per asy GER", y=as.character(ORIGIN_COUNTRY), label=as.character(round(ref_per_asy_GER))), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ref per asy USA", y=as.character(ORIGIN_COUNTRY), label=as.character(round(ref_per_asy_USA))), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size = 2), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ASY_GER", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ASY_USA", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="REF_GER", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="REF_USA", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ref per asy GER", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +
      layer(data=data_final, 
            mapping=aes(x="ref per asy USA", y=as.character(ORIGIN_COUNTRY), fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      )
    
    # End your code here.
    return(plot)
  }) 
  
  dfQ <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"""select ASYLUM_COUNTRY, RECORD_YEAR, REFUGEES, ORIGIN_COUNTRY from Refugee_Stats;"""')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)));
  
  dfQ <- dfQ %>% filter (as.character(ORIGIN_COUNTRY) == "Viet Nam", as.numeric(as.character(REFUGEES)) != "null") %>% group_by(RECORD_YEAR) %>% arrange(desc(as.character(ASYLUM_COUNTRY)))
  
  dfQ2 <- dfQ %>% group_by(RECORD_YEAR) %>% summarise(sum_refugees = sum(as.numeric(as.character(REFUGEES))))
  
  output$distPlot2 <- renderPlot(height=600, width=1100, {
    pQ <- ggplot(dfQ, aes(x=as.character(RECORD_YEAR), y = as.numeric(as.character(REFUGEES)), fill = as.character(ASYLUM_COUNTRY))) + 
      geom_bar(stat = "identity") + 
      labs(title='A Look at Migration Out of Vietnam') + 
      labs(x="Year", y="Refugees")
    
    pQ <- pQ  +
      geom_text(data = dfQ2, 
                aes(y = sum_refugees, label = sum_refugees, fill = NULL), size = 4,
                vjust = -0.5) +
      geom_hline(data = dfQ2, aes(yintercept = mean(as.numeric(as.character(sum_refugees))))) + 
      annotate("text", x = 1.5, y = 310000, label = 301027, size = 4)
    
    pQ
  })

  
  # Begin code for third Tab:
  
  refugee_low <- reactive({input$refs[1]})
  refugee_high <- reactive({input$refs[2]})
  host_low <- reactive({input$hosts[1]})
  host_high <- reactive({input$hosts[2]})
  
  
  norm_refs <- eventReactive(input$clicks, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                      "SELECT asylum_country, record_year, (all_refugees / current_pop * 100) normalized_refugees FROM (SELECT asylum_country, record_year, SUM(total_population) all_refugees, x2014 current_pop FROM (select * from REFUGEE_STATS LEFT JOIN (select cname, continent_code from COUNTRIES) co ON co.cname = REFUGEE_STATS.asylum_country WHERE REFUGEE_STATS.asylum_country != \\\'Various/Unknown\\\' AND REFUGEE_STATS.total_population > "p4" AND REFUGEE_STATS.total_population < "p2" ) refs INNER JOIN (select * from YEARLY_POP_BY_COUNTRY_NUMERIC WHERE X2014 > "p3" AND X2014 < "p1" ) pops ON pops.country_name = refs.asylum_country GROUP BY asylum_country, record_year, x2014);"
                                                                                      ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p2=refugee_high(), p1=host_high(), p3=host_low(), p4=refugee_low()), verbose = TRUE)))
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
  
})
