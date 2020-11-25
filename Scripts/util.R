path_depth <- function(path){
  nb <- lengths(str_split(path, '/'))
  return(nb)
}

load_data <- function(cities, start_date=-1, end_date=-1){
  
  list_dirs <- list.dirs('./data_cleansed')
  
  path_kept <- list_dirs[ path_depth(list_dirs)==5 ]
  
  keep_bool = c()
  
  for(p in path_kept){
    splitted <- str_split_fixed(p, '/', n = 6)
    path_city <- splitted[,4]
    path_date <- splitted[,5]
    
    conditions = 0
    if(start_date == -1 | end_date ==-1){
      conditions = any(grepl( path_city, cities, fixed = TRUE))
    }
    else conditions = any(grepl( path_city, cities, fixed = TRUE)) & start_date <= path_date & path_date <= end_date 
    
    if( conditions == TRUE){
      keep_bool <- append(keep_bool, TRUE)
    }
    else{
      keep_bool <- append(keep_bool, FALSE)
    }
  }
  
  path_kept <- path_kept[ keep_bool ]
  print(path_kept)
  
  if(!is.na(path_kept[1])){
    df <- fread(paste0(path_kept[1],'/listings.csv'), header = T, sep = ',', data.table = F)
    
    
    for(path in path_kept[-1]){
      file_path <- file.path(path,'listings.csv')
      df <- rbind(df,fread(file_path, header = T, sep = ',', data.table = F))
      
    }
    df$bedrooms <- ifelse(df$bedrooms >= 5, "5+", df$bedrooms)
    df$beds <- ifelse(df$beds >= 5, "5+", df$beds)
    df$accommodates <- ifelse(df$accommodates >= 10, "10+", df$accommodates)
    return(df)
  }
  else return(NULL)
  
}


load_countries_cities <- function(){
  list_dirs <- list.dirs('./data_cleansed')
  
  path_kept <- list_dirs[ path_depth(list_dirs) == 4 ]
  countries_cities <- c()
  for(p in path_kept){
    countries_cities <- append(countries_cities, str_split_fixed(p, '/', n = 5)[,3:4])
  }
  return(countries_cities)
}