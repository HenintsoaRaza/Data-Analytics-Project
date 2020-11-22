library(tidyverse)
library(stringr)
library(ggplot2)
library(data.table)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('../')

rm(list=ls())
start_time <- Sys.time()
# Number of last files we keep per countries (according to the date)
NB_KEEP_DATES <- 3
# We choose 3 countries among these ones : "france", "spain", "italy", "germany", "the-netherlands", "belgium"
selected_countries <- c("france", "italy", "germany")

file_path <- file.path("App", "all_data_urls.csv")
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
    
    
    df_list <- fread(listings_url, header = T, sep = ',', nThread = NB_THREADS)
    df_cal <- fread(calendar_url, header = T, sep = ',', nThread = NB_THREADS)
    
    ## Add Keys: columns country, city and data date 
    df_list$country <- country
    df_list$city <- city
    df_list$data_date <- data_date
    
    missing_columns <- setdiff(columns_listings, colnames(df_list))
    
    print(paste("Number of missing columns: ",length(missing_columns)))
    
    # If there is no missing columns ...
    if( length(missing_columns)==0 ){
        
        ## add day number (starting first day)
        df_cal <- df_cal %>%
            group_by(listing_id) %>%
            arrange(date) %>%
            mutate(day_nb = row_number()) %>%
            ungroup() %>%
            
            ## change available column to binary
            mutate(available = ifelse(available=="t", 1, 0)) %>%
            
            ## clean price column and transform to numeric
            mutate(price = str_replace(price, "\\$", "")) %>%
            mutate(price = str_replace(price, ",", "")) %>%
            mutate(price = as.numeric(price))  %>%
            
            ## calculate estimated revenue for upcoming day
            mutate(revenue = price*(1-available)) %>%
            
            ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
            group_by(listing_id) %>%
            summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                      price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                      revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE)      
            ) %>%
            ungroup()
            
            df <- df_list %>% 
                select(columns_listings) %>%
                
                ## clean price column and transform to numeric
                mutate(price = str_replace(price, "\\$", "")) %>%
                mutate(price = str_replace(price, ",", "")) %>%
                mutate(price = as.numeric(price))  %>%
                
                mutate(id = strtoi(id)) %>% 
                left_join(df_cal, by = c("id" = "listing_id"))
            
            dir.create(file.path("App","data_cleansed3", country, city, data_date), recursive = TRUE)
            
            write.csv(df, file.path("App","data_cleansed3", country, city, data_date, "listings.csv"), row.names=FALSE)
            print(paste0("saving data into ", file.path("App", "data_cleansed3", country, city, data_date, "listings.csv")))
            
            print(Sys.time() - start_time)
            
            return(df)
            
    }
    else { # If there are some missing columns ...
        
        ## add day number (starting first day)
        df_cal <- df_cal %>%
            group_by(listing_id) %>%
            arrange(date) %>%
            mutate(day_nb = row_number()) %>%
            ungroup() %>%
            
            ## change available column to binary
            mutate(available = ifelse(available=="t", 1, 0)) %>%
            
            ## clean price column and transform to numeric
            mutate(price = str_replace(price, "\\$", "")) %>%
            mutate(price = str_replace(price, ",", "")) %>%
            mutate(price = as.numeric(price))  %>%
            
            ## calculate estimated revenue for upcoming day
            mutate(revenue = price*(1-available)) %>%
            
            ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
            group_by(listing_id) %>%
            mutate(price = mean(price), 
                   availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                   price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                   revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE)      
            ) %>%
            ungroup() %>%
            filter(day_nb<=30) %>%
            select( -c("date", "available", "adjusted_price") ) %>%
            rename(id = listing_id) %>%
            distinct(id, .keep_all = TRUE)
        
        
        df <- df_list %>% 
            select(-availability_30) %>%
            mutate(id = strtoi(id)) %>% 
            left_join(df_cal, by = "id" )

        df$bedrooms <- NA
        df$beds <- NA
        
        df <- df %>%
            select(c(columns_listings,"availability_30","price_30","revenue_30") )
        
        dir.create(file.path("App","data_cleansed3", country, city, data_date), recursive = TRUE)
        
        write.csv(df, file.path("App","data_cleansed3", country, city, data_date, "listings.csv"), row.names=FALSE)
        print(paste0("saving data into ", file.path("App", "data_cleansed3", country, city, data_date, "listings.csv")))
        
        print(Sys.time() - start_time)
        
        return(df)
        
    }
    

}

df_clean <- all_urls %>%
    select(listings_data_url) %>%
    apply(1, function(x) load_data_from_url(x))

df_clean <- data.frame(Reduce(rbind, df_clean))


end_time <- Sys.time()

print(end_time - start_time)