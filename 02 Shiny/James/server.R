# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {
        
      KPI_Low_Max_value <- input$KPI1     
      KPI_Medium_Max_value <- input$KPI2
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
      
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
    
      output$distPlot1 <- renderPlot({             
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
        
       return(plot)})

      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })

# Begin code for Second Tab:

      df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select color, clarity, avg_price, avg(avg_price) 
             OVER (PARTITION BY clarity ) as window_avg_price
             from (select color, clarity, avg(price) avg_price
                   from diamonds
                   group by color, clarity)
            order by clarity;"
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
            MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot2 <- renderPlot(height=1000, width=2000, {
            plot1 <- ggplot() + 
              coord_cartesian() + 
              scale_x_discrete() +
              scale_y_continuous() +
              facet_wrap(~CLARITY, ncol=1) +
              labs(title='Diamonds Barchart\nAVERAGE_PRICE, WINDOW_AVG_PRICE, ') +
              labs(x=paste("COLOR"), y=paste("AVG_PRICE")) +
              layer(data=df2(), 
                    mapping=aes(x=COLOR, y=AVG_PRICE), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="bar",
                    geom_params=list(colour="blue"), 
                    position=position_identity()
              ) + coord_flip() +
              layer(data=df2(), 
                    mapping=aes(x=COLOR, y=AVG_PRICE, label=round(AVG_PRICE - WINDOW_AVG_PRICE)), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="text",
                    geom_params=list(colour="black", hjust=-1), 
                    position=position_identity()
              ) +
              layer(data=df2(), 
                    mapping=aes(yintercept = WINDOW_AVG_PRICE), 
                    geom="hline",
                    geom_params=list(colour="red")
              )
              plot1
      })

# Begin code for Third Tab:

      df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            """select region || \\\' \\\' || \\\'Sales\\\' as measure_names, sum(sales) as measure_values from SUPERSTORE_SALES_ORDERS
            where country_region = \\\'United States of America\\\'
            group by region
            union all
            select market || \\\' \\\' || \\\'Coffee_Sales\\\' as measure_names, sum(coffee_sales) as measure_values from COFFEE_CHAIN
            group by market
            order by 1;"""
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
            MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot3 <- renderPlot(height=1000, width=2000, {
            plot3 <- ggplot() + 
              coord_cartesian() + 
              scale_x_discrete() +
              scale_y_continuous() +
              #facet_wrap(~CLARITY, ncol=1) +
              labs(title='Blending 2 Data Sources') +
              labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
              layer(data=df3(), 
                    mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="bar",
                    geom_params=list(colour="blue"), 
                    position=position_identity()
              ) + coord_flip() +
              layer(data=df3(), 
                    mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES)), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="text",
                    geom_params=list(colour="black", hjust=-0.5), 
                    position=position_identity()
              )
              plot3
      })
})
