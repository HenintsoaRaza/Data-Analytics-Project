library(tidyverse)
library(stringr)
library(ggplot2)
library(data.table)

#setwd("~/Dropbox/Data Analytics ECE/Airbnb")


# ##########################################################################################################
# 
# # a generic function to prepare data for a specific city, data_date
# prepare_data <- function(city, data_date)
# {
#     # Cleaning listings dataframe
# 
#     # suppose raw data is stored in data_raw/city/data_date/listings.csv.gz
#     #listings_url <- file.path("data_raw", city, data_date, "listings.csv.gz")
#     # suppose raw data is stored in data_raw/city/data_date/calendar.csv.gz
#     #calendar_url <- file.path("data_raw", city, data_date, "calendar.csv.gz")
# 
#     listings_url <- file.path("data_raw",city,  "listings.csv.gz")
#     calendar_url <- file.path("data_raw",city,  "calendar.csv.gz")
# 
#     print(paste0("reading data from ", listings_url))
#     listings <- read.csv(gzfile(listings_url))
#     print(paste0("reading data from ", calendar_url))
#     calendar <- read.csv(gzfile(calendar_url))
# 
#     ## Add Keys: columns city and day date
#     listings$city <- city
#     listings$data_date <- data_date
# 
#     ## Select interesting columns
#     ### Most columns don't contain interesting information
#     columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed",
#                           "latitude", "longitude",
#                           "property_type", "room_type", "accommodates", "bedrooms",
#                           "beds", "price", "minimum_nights",  "maximum_nights")
# 
#     listings <- listings %>%
#         select(columns_listings) %>%
#         arrange(id)
# 
# 
#     # Cleaning calendar dataframe
# 
#     ## arrange by id and date
#     calendar <- calendar %>%
#         arrange(listing_id, date)
# 
#     ## add day number (starting first day)
#     calendar <- calendar %>%
#         group_by(listing_id) %>%
#         mutate(day_nb = row_number()) %>%
#         ungroup()
# 
#     ## change available column to binary
#     calendar <- calendar %>%
#         mutate(available = ifelse(available=="t", 1, 0))
# 
#     ## clean price column and transform to numeric
#     calendar <- calendar %>%
#         mutate(price = str_replace(price, "\\$", ""),
#                adjusted_price = str_replace(adjusted_price, "\\$", ""))
#     calendar <- calendar %>%
#         mutate(price = str_replace(price, ",", ""),
#                adjusted_price = str_replace(adjusted_price, ",", ""))
#     calendar <- calendar %>%
#         mutate(price = as.numeric(price),
#                adjusted_price = as.numeric(adjusted_price))
# 
#     ## calculate estimated revenue for upcoming day
#     calendar <- calendar %>%
#         mutate(revenue = price*(1-available))
# 
#     ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
#     calendar <- calendar %>%
#         group_by(listing_id) %>%
#         summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
#                   #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
#                   #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
#                   #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
#                   price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
#                   #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
#                   #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
#                   #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
#                   revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
#                   #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
#                   #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
#                   #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)
#         )
# 
#     listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
# 
#     dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
# 
#     write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
#     print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
# 
# }
# 
# # Example: Reading data for malaga:
# # Preparing data
# 
# city <- "malaga"
# data_date <- "2020-06-30"
# prepare_data(city, data_date)
# 
# 
# # Example: Prepare data for multiple cities
# 
# cities <- c("malaga", "mallorca", "sevilla")
# data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")
# 
# for(i in 1:length(cities)){
#     city <- cities[i]
#     data_date <- data_dates[i]
#     print("-------------------------------------------------")
#     print(paste(c("Preparing data for", city, "compiled at", data_date), collapse = " "))
#     prepare_data(city, data_date)
# }
# 
# # Clean Environment
# rm(list=ls())
# 
# ## Once data for multiple cities are prepared
# ## We can read these data and concatenate them together into one dataframe
# 
# # Reading cleansed data
# cities <- c("malaga", "mallorca", "sevilla")
# data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")
# 
# # We are only interested in data between min_date and max_date
# min_date <- '2020-05-01'
# max_date <- '2020-11-01'
# 
# files_paths <- c()
# 
# # Read data in cities between min_date and max_date
# for(city in cities){
#     file_dir <- file.path(".", "data_cleansed", city)
#     file_subdirs <- list.dirs(file_dir)
#     file_subdirs <- file_subdirs[-1]
# 
#     for(file_subdir in file_subdirs){
#         if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
#             file_subdirs = file_subdirs[file_subdirs != file_subdir]
#     }
#     files_paths <- c(files_paths, file_subdirs)
# }
# files_paths <- file.path(files_paths, "listings.csv")
# listings <-
#     do.call(rbind,
#             lapply(files_paths, read.csv, row.names=1))
# 
# ## Preprocess
# listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
# 
# ##########################################################################################################

rm(list=ls())
start_time <- Sys.time()
# Number of last files we keep per countries (according to the date)
NB_KEEP_DATES <- 3
# We choose 3 countries among these ones : "france", "spain", "italy", "germany", "the-netherlands", "belgium"
selected_countries <- c("france", "germany", "belgium")

file_path <- file.path("..", "App", "all_data_urls.csv")
all_urls <- read.csv(file = file_path)

all_urls <- all_urls %>%
    mutate(country = str_split_fixed(listings_data_url, '/', n=10)[,4],
           city = str_split_fixed(listings_data_url, '/', n=10)[,6],
           data_date = str_split_fixed(listings_data_url, '/', n=10)[,7]
           ) %>%
    filter(country %in% selected_countries) %>%
    group_by(city) %>%
    top_n(n = NB_KEEP_DATES, wt = data_date) %>%
    ungroup() %>%
    as.data.frame() 

## Select interesting columns
### Most columns don't contain interesting information
columns_listings <- c("country","city", "data_date", "id", "neighbourhood_cleansed",
                      "latitude", "longitude",
                      "property_type", "room_type", "accommodates", "bedrooms",
                      "beds", "price", "minimum_nights",  "maximum_nights")

## Number of threads used for loading the data 
NB_THREADS <- 6

load_data_from_url <- function(listings_url){
    start_time2 <- Sys.time()
    
    print(listings_url)
    splitted_url <- str_split_fixed(listings_url, '/', n=10)
    country <- splitted_url[,4]
    city <- splitted_url[,6]
    data_date <- splitted_url[,7]
    calendar_url <- str_replace(listings_url, "listings", "calendar")
    
    # con1 <- gzcon(url( listings_url ))
    # txt1 <- readLines(con1)
    # df_list <- read.csv(textConnection(txt1))
    df_list <- fread(listings_url, header = T, sep = ',', nThread = NB_THREADS)
    
    ## Add Keys: columns country, city and data date 
    df_list$country <- country
    df_list$city <- city
    df_list$data_date <- data_date
    
    df_list <- df_list %>% 
        select(columns_listings) %>% 
        mutate(id = strtoi(id))
    
    # con2 <- gzcon(url( calendar_url ))
    # txt2 <- readLines(con2)
    # df_cal <- read.csv(textConnection(txt2))
    df_cal <- fread(calendar_url, header = T, sep = ',', nThread = NB_THREADS)
    
    
    ## add day number (starting first day)
    df_cal <- df_cal %>%
        group_by(listing_id) %>%
        arrange(date) %>%
        mutate(day_nb = row_number()) %>%
        ungroup() %>%
    
    ## change available column to binary
        mutate(available = ifelse(available=="t", 1, 0)) %>%
    
    ## clean price column and transform to numeric
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", "")) %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", "")) %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))  %>%
    
    ## calculate estimated revenue for upcoming day
        mutate(revenue = price*(1-available)) %>%
    
    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE)      
        )
    
    df <- df_list %>% left_join(df_cal, by = c("id" = "listing_id"))
    print(Sys.time() - start_time)

    return(df)
}

df_clean <- all_urls %>%
    select(listings_data_url) %>%
    apply(1, function(x) load_data_from_url(x))

df_clean <- data.frame(Reduce(rbind, df_clean))

# new_dir <- file.path("..","App", "data_raw2", country, city, data_date)
# dir.create(new_dir, recursive = TRUE)
# download.file(url = listings_url,
#               destfile = file.path(new_dir, "listings.csv.gz"),
#               method = 'curl')
# download.file(url = calendar_url,
#               destfile = file.path(new_dir, "calendar.csv.gz"),
#               method = 'curl')

end_time <- Sys.time()

print(end_time - start_time)