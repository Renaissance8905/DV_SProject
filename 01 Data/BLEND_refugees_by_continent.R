require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

by_cont <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="""SELECT * FROM (SELECT tot_pop, continents.NAME continent FROM CONTINENTS INNER JOIN (SELECT SUM(X2014) tot_pop, continent_code FROM YEARLY_POP_BY_COUNTRY_NUMERIC INNER JOIN COUNTRIES ON COUNTRIES.iso3 = YEARLY_POP_BY_COUNTRY_NUMERIC.country_code GROUP BY continent_code) cont_pop ON continents.code = cont_pop.continent_code) sum_pop FULL JOIN (select record_year, SUM(total_population) all_refugees, continents.NAME continent FROM ((SELECT * FROM REFUGEE_STATS LEFT JOIN (SELECT cname, continent_code from COUNTRIES) co ON co.cname = REFUGEE_STATS.asylum_country WHERE REFUGEE_STATS.asylum_country != \\\'Various/Unknown\\\') refs INNER JOIN (SELECT * FROM YEARLY_POP_BY_COUNTRY_NUMERIC) pops ON pops.country_name = refs.asylum_country) LEFT JOIN continents ON continents.CODE = refs.continent_code GROUP BY record_year, continents.NAME) rest ON rest.continent = sum_pop.continent WHERE tot_pop > 0;"""')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); View(by_cont)

str(by_cont)

by_cont %>% mutate(normal_refs = ALL_REFUGEES / TOT_POP * 100) -> by_cont

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  facet_wrap(~CONTINENT) +
  labs(title='Yearly Normalized Refugee Intake By Continent') +
  labs(x=paste("YEAR"), y=paste("REFUGEES (% of Total Population)")) +
  layer(data=by_cont, 
        mapping=aes(x=RECORD_YEAR, y=normal_refs), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  )