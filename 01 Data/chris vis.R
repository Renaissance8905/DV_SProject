require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="""SELECT asylum_country, record_year, (all_refugees / current_pop * 100) normalized_refugees FROM (SELECT asylum_country, record_year, SUM(total_population) all_refugees,
x2014 current_pop FROM (select * from REFUGEE_STATS LEFT JOIN (select cname, continent_code from COUNTRIES) co ON co.cname = REFUGEE_STATS.asylum_country WHERE (REFUGEE_STATS.asylum_country != \\\'Various/Unknown\\\')) refs INNER JOIN (select * from YEARLY_POP_BY_COUNTRY_NUMERIC) pops ON pops.country_name = refs.asylum_country GROUP BY asylum_country, record_year, x2014);"""')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); View(df)

df %>% filter(NORMALIZED_REFUGEES > 3) -> df


ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Refugee Influxes Over Time, Normalized for Host Populations') +
  labs(x=paste("Year"), y=paste("Yearly Refugee Influx as % of Host Population")) +
  layer(data=df, 
        mapping=aes(x=RECORD_YEAR, y=NORMALIZED_REFUGEES, color=ASYLUM_COUNTRY), 
        stat="identity", 
        stat_params=list(), 
        geom="line",
        geom_params=list(), 
        position=position_identity(),
  )