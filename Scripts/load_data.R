library(data.table)
library(tidyverse)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('../App')

source('../Scripts/util.R')

countries_cities <- load_countries_cities()

country_list <- c()
city_list <- c()

for(i in seq(1,length(countries_cities)-1,by=2) ){
  country_list <- append(country_list, countries_cities[i])
  city_list <- append(city_list, countries_cities[i+1])
}

df_countries_cities <- data.frame(country = country_list, city = city_list)

cities <- list()

for(c in country_list){
  filtered_city <- df_countries_cities %>% distinct(country, city) %>% filter(country == c)
  cities[c] <- list(filtered_city$city)
}


rm(df_countries_cities)
