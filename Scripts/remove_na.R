library(tidyverse)
library(stringr)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('../App')

source("../Scripts/util.R")

fill_na <- function(country, city){
  df_city <- load_data(city)
  data_dates_unique <- df_city %>%
    distinct(data_date) %>%
    as.list()
  
  df_summary <- df_city %>%
    group_by(id)  %>%
    summarise(fill_na_bedrooms = max(bedrooms,na.rm = TRUE),
              fill_na_beds = max(beds,na.rm = TRUE),
              fill_na_minimum_nights = max(minimum_nights,na.rm = TRUE),
              fill_na_maximum_nights = max(maximum_nights,na.rm = TRUE))
  df_city <- df_city %>%
    arrange(id) %>%
    left_join(df_summary, by = c("id")) %>%
    mutate(bedrooms = ifelse(is.na(bedrooms), fill_na_bedrooms, bedrooms)) %>%
    mutate(beds = ifelse(is.na(beds), fill_na_beds, beds)) %>%
    mutate(minimum_nights = ifelse(is.na(minimum_nights), fill_na_minimum_nights, minimum_nights)) %>%
    mutate(maximum_nights = ifelse(is.na(maximum_nights), fill_na_maximum_nights, maximum_nights)) %>%
    select(-c(fill_na_bedrooms,fill_na_beds,fill_na_minimum_nights,fill_na_maximum_nights))
  
  
  data_dates_unique <- data_dates_unique$data_date

  for(i in 1:length(data_dates_unique)){
    df_city_by_date <- df_city %>% filter(data_date == data_dates_unique[i])
    print(paste0("saving data into ", file.path("App", "data_cleansed", country, city, data_dates_unique[i], "listings.csv")))
    write.csv(df_city_by_date, file.path("data_cleansed", country, city, data_dates_unique[i], "listings.csv"), row.names=FALSE)
  }
}

countries_cities <- load_countries_cities()

for(i in seq(1,length(countries_cities)-1, by=2) ){
  fill_na(countries_cities[i],countries_cities[i+1])
}

