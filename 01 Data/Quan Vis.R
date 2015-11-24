require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(tidyr)

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"""select ASYLUM_COUNTRY, RECORD_YEAR, REFUGEES, ORIGIN_COUNTRY from Refugee_Stats;"""')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); 

df <- df %>% filter (as.character(ORIGIN_COUNTRY) == "Viet Nam", as.numeric(as.character(REFUGEES)) != "null") %>% group_by(RECORD_YEAR) %>% arrange(desc(as.character(ASYLUM_COUNTRY)))

df2 <- df %>% group_by(RECORD_YEAR) %>% summarise(sum_refugees = sum(as.numeric(as.character(REFUGEES))))

p1 <- ggplot(df, aes(x=as.character(RECORD_YEAR), y = as.numeric(as.character(REFUGEES)), fill = as.character(ASYLUM_COUNTRY))) + 
  geom_bar(stat = "identity") + 
  labs(title='A Look at Migration Out of Vietnam') + 
  labs(x="Year", y="Refugees")

p1 <- p1  +
  geom_text(data = df2, 
            aes(y = sum_refugees, label = sum_refugees, fill = NULL), size = 4,
            vjust = -0.5) +
  geom_hline(data = df2, aes(yintercept = mean(as.numeric(as.character(sum_refugees))))) + 
  annotate("text", x = 1.5, y = 310000, label = 301027, size = 4)

p1
