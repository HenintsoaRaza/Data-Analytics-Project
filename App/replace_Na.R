library(tidyverse)
library(data.table)

if(getwd()!= "C:/Users/henin/Desktop/ING5/1. Data Analytics/Project - Airbnb Listings Analysis/Data-Analytics-Project/App/data_cleansed3/italy/bologna")
  setwd('App/data_cleansed3/italy/bologna')

path1 <- file.path(getwd(),"2020-06-15", "listings.csv")
path2 <- file.path(getwd(),"2020-07-20", "listings.csv")
path3 <- file.path(getwd(),"2020-08-31", "listings.csv")

bologna_path <- c(path1,path2,path3)

df = lapply(bologna_path, read.csv)

df <- data.frame(Reduce(rbind, df))

df %>%
  arrange(id) %>%
  print()

bed_values <- df %>%
  select(-c(country, city, latitude, longitude, property_type, accommodates)) %>%
  arrange(id) %>%
  group_by(id) %>%
  summarise(max_beds = max(beds,na.rm = T))

df %>% 
  left_join(bed_values, by='id' ) %>%
  arrange(id) %>%
  mutate(beds = max_beds) %>%
  select(c(id, data_date, beds, bedrooms)) %>%
  head(100)

