df <- read.csv("C:/Users/prana/Desktop/Econ/R_codes/h1b_kaggle.csv")
cols(
  x1 = col_double(),
  CASE_STATUS = col_character(),
  EMPLOYER_NAME = col_character(),
  SOC_NAME = col_character(),
  JOB_TITLE = col_character(),
  FULL_TIME_POSITION = col_character(),
  PREVAILING_WAGE = col_double(),
  YEAR = col_double(),
  WORKSITE = col_character(),
  lon = col_double(),
  lat = col_double()
  
)

install.packages("dplyr")
install.packages("ggplot2")



update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'),
                 type = 'source')
library(knitr)
library(magrittr)
library(dplyr)
library("ggplot2")




df$state <- trimws(gsub("^.*,", "",df$WORKSITE))
head(df$state)


df$rlat = round(df$lat, 0)
df$rlon = round(df$lon, 0)
#head(df$rlat)

#length(df$lat)

#Case_status vs Applications mapping

df %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>% 
  summarise(nr = length(lat)) %>% ungroup() -> dc

ggplot(data = dc, aes(x = reorder(CASE_STATUS,nr), y = nr/1000)) +  
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Case status", y = "Number of applications (thousands)")



#year vs applications
df %>% filter(!is.na(CASE_STATUS)) %>% filter(!is.na(YEAR)) %>%
  group_by(CASE_STATUS,YEAR) %>% summarise(nr = length(CASE_STATUS)) %>% ungroup() ->dcy

ggplot(data = dcy, aes(x = YEAR, y = nr/1000, colour = CASE_STATUS)) +
  geom_line() + geom_point() + theme_bw() + theme(legend.position = "right") +
  labs(x="Year", y = "Applications(thousands)", colour = "Case Status", title = "Case Status(per year)")



dev.off() 