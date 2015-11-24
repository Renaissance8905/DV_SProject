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
  
  output$distPlot1 <- renderPlot({
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
  
  # Begin code for Fourth Tab:
  output$map <- renderLeaflet({leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17) %>% addPopups(-93.65, 42.0285, 'Here is the Department of Statistics, ISU')
  })
  
  # Begin code for Fifth Tab:
  output$table <- renderDataTable({datatable(df1())
  })
})
