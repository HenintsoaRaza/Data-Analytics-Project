library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

#setwd("~/Dropbox/Data Analytics ECE/Airbnb")

# a generic function to prepare data for a specific city, data_date
prepare_data <- function(city, data_date)
{
    # Cleaning listings dataframe
    
    # suppose raw data is stored in data_raw/city/data_date/listings.csv.gz
    #listings_url <- file.path("data_raw", city, data_date, "listings.csv.gz")
    # suppose raw data is stored in data_raw/city/data_date/calendar.csv.gz
    #calendar_url <- file.path("data_raw", city, data_date, "calendar.csv.gz")
    
    listings_url <- file.path(city,  "listings.csv.gz")
    calendar_url <- file.path(city,  "calendar.csv.gz")
    
    print(paste0("reading data from ", listings_url))
    listings <- read.csv(gzfile(listings_url))
    print(paste0("reading data from ", calendar_url))
    calendar <- read.csv(gzfile(calendar_url))
    
    ## Add Keys: columns city and day date
    listings$city <- city
    listings$data_date <- data_date
    
    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed", 
                          "latitude", "longitude", 
                          "property_type", "room_type", "accommodates", "bedrooms", 
                          "beds", "price", "minimum_nights",  "maximum_nights")
    
    listings <- listings %>% 
        select(columns_listings) %>% 
        arrange(id)
    
    
    # Cleaning calendar dataframe
    
    ## arrange by id and date
    calendar <- calendar %>% 
        arrange(listing_id, date)
    
    ## add day number (starting first day)
    calendar <- calendar %>%
        group_by(listing_id) %>%
        mutate(day_nb = row_number()) %>%
        ungroup()
    
    ## change available column to binary
    calendar <- calendar %>%
        mutate(available = ifelse(available=="t", 1, 0))
    
    ## clean price column and transform to numeric
    calendar <- calendar %>%
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", ""))
    calendar <- calendar %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", ""))
    calendar <- calendar %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))
    
    ## calculate estimated revenue for upcoming day
    calendar <- calendar %>%
        mutate(revenue = price*(1-available))
    
    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
    calendar <- calendar %>%
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
                  #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
                  #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
                  #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
                  #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                  #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
                  #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
                  #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)           
        )
    
    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
    
    dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
    
    write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
    print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
    
}  

# Example: Reading data for malaga:
# Preparing data 

city <- "malaga"
data_date <- "2020-06-30"
prepare_data(city, data_date)


# Example: Prepare data for multiple cities

cities <- c("malaga", "mallorca", "sevilla")
data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")

for(i in 1:length(cities)){
    city <- cities[i]
    data_date <- data_dates[i]
    print("-------------------------------------------------")
    print(paste(c("Preparing data for", city, "compiled at", data_date), collapse = " "))
    prepare_data(city, data_date)
}

# Clean Environment
rm(list=ls())

## Once data for multiple cities are prepared
## We can read these data and concatenate them together into one dataframe

# Reading cleansed data
cities <- c("malaga", "mallorca", "sevilla")
data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29")

# We are only interested in data between min_date and max_date
min_date <- '2020-05-01'
max_date <- '2020-11-01'

files_paths <- c()

# Read data in cities between min_date and max_date
for(city in cities){
    file_dir <- file.path(".", "data_cleansed", city)
    file_subdirs <- list.dirs(file_dir)
    file_subdirs <- file_subdirs[-1]
    
    for(file_subdir in file_subdirs){
        if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
            file_subdirs = file_subdirs[file_subdirs != file_subdir]
    }
    files_paths <- c(files_paths, file_subdirs)
}
files_paths <- file.path(files_paths, "listings.csv")
listings <- 
    do.call(rbind,
            lapply(files_paths, read.csv, row.names=1))

## Preprocess
listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)


# Analysis 1
# Find the "average availability over 30 days" of listings per each city. 
listings %>%
    group_by(city) %>%
    summarise(Mean_availability_30 = mean(availability_30)) %>%

# Find the "average revenue of over 30 days" of listings per each city. 
listings %>%
    group_by(city) %>%
    summarise(Mean_revenue_30 = mean(revenue_30))


## Comparing the distribution of estimated availability for the next 30 days of listings
## per each city.
avgAvai30 <- ggplot(listings, aes(city, availability_30, fill=city))
avgAvai30 + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
    stat_summary(fun="mean") +
    scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T)) +
    scale_fill_brewer(palette="Set1") + 
    stat_summary(fun=mean, colour="black", geom="text",
                 vjust=-0.8, aes(label=round(..y.., digits=2)))

## Comparing the distribution of estimated revenue for the next 30 days of listings
## per each city.

avgRev30 <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30 + geom_boxplot(aes(colour = "red"),  outlier.shape = NA) +
    stat_summary(fun="mean") +
    scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
    scale_fill_brewer(palette="Set1") +
    stat_summary(fun=mean, colour="black", geom="text",
                 vjust=-0.8, aes(label=round(..y.., digits=2)))


# Compare the distribution of estimated revenue for the next 30 days of listings
# per each city & for each house size (# of bedrooms).

avgRev30_city_bed <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30_city_bed + geom_boxplot(aes(colour = bedrooms), lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
    scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
    scale_fill_brewer(palette="Set1") 
    

# Compare the distribution of estimated revenue for the next 30 days of listings
# per each city & for each room type (room_type).

avgRev30_city_roomType <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30_city_roomType + geom_boxplot(aes(colour = room_type),  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
    scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
    scale_fill_brewer(palette="Set1") 


# -----------------------------------------------------------------------------------------------------------------

# Analysis 2 : 
# What is the proportion of each room type?
proportion_RoomType <- ggplot(listings, aes(room_type))
proportion_RoomType + geom_bar() +
    geom_text(aes(label=stat(count)), stat='count', nudge_y=0.125, va='bottom')



# What is the proportion of each house size (# of bedroom)?



# What is the proportion of each neighborhood?



# What is the average availability over the next 30 days for each room type /
# house size / neighborhood?



# What is the average revenue over the next 30 days for each room type /
# house size / neighborhood?



# What is the distribution of availability over the next 30 days for each room type
# house size / neighborhood?



# What is the distribution of revenue over the next 30 days for each room type /
# house size / neighborhood?
