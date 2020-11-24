#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(htmltools)

load_data <- function(){
    list_dirs <- list.dirs('./data_cleansed')
    
    nb_dirs <- function(path){
        nb <- lengths(str_split(path, '/'))
        return(nb)
    }
    
    path_kept <- list_dirs[ nb_dirs(list_dirs)==5 ]
    
    df <- read.csv(paste0(path_kept[1],'/listings.csv'))
    
    for(path in path_kept[2:39]){
        file_path <- file.path(path,'listings.csv')
        print(file_path)
        df <- rbind(df,read.csv(file_path))
    }
    df$bedrooms <- ifelse(df$bedrooms >= 5, "5+", df$bedrooms)
    df$beds <- ifelse(df$beds >= 5, "5+", df$beds)
    df$accommodates <- ifelse(df$accommodates >= 10, "10+", df$accommodates)
    return(df)
}


df <- load_data()

countries <- df %>%
    distinct(country) %>%
    as.list()

countries <- countries$country

cities <- df %>%
    distinct(city) %>%
    as.list()

cities <- cities$city


# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("darkly"),
        titlePanel("Airbnb"),
            tabsetPanel(type = "tabs",
                tabPanel("Home", 
                         br(), 
                         mainPanel(
                             h1("Airbnb Analysis App"),
                             br(),
                             p("Welcome to Airbnb Analysis App that allows you to get some interesting insights on Airbnb statistics."),
                             br(),
                             br(),
                             p("Our app allows you to perform two different types of analysis: "),
                             tags$ol(
                                 tags$li("Compare cities"), 
                                 tags$li("Deep dive into a selected city")
                             ),
                             br(),
                             br(),
                             h3("1) Compare cities"),
                             p("The first type of analysis allows you to display insightful graphics (Density, Boxplot and Histogram) 
                             concerning the cities of your choice. The main of this section is to compare statistics between cities, 
                               therefore it is granted to you to select multiple cities. Our graphs will automatically change themselves
                               with respect to your filters such as date, feature and dimension. Finally, you can also
                               display a table that stores the average and median of the selected feature. All these filters are
                               placed beautifully in a sidebar on the left of the panel."),
                             br(),
                             h3("2) Deep dive into a selected city"),
                             p("The second type of analysis allows you to display more information for a specific city. As in the first section,
                               the filters are the same but you can select only one city. This finer grained analysis gives you some information
                               such as average and median of the selected feature according to a dimension. Eventually, we provide you a map, so that
                               you may visualize easily and interactivelly the different accommodations on the map."),
                         )
                ),
                tabPanel("1) Compare cities",
                         br(),
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("selectCities", 
                                             h3("Filter by city"),
                                             multiple = TRUE,
                                             choices = cities,
                                             selected = cities[1]
                                 ),
                                 dateRangeInput("dates", 
                                                h3("Date range"), 
                                                format = "yyyy-mm-dd",
                                                start = "2020-01-01",
                                                end = "2020-12-12"),
                                 radioButtons("radioFeatures", 
                                              h3("Select the feature"),
                                              choices = list("availability over 30 days" = 'availability_30', 
                                                             "Average price over 30 days" = 'price_30',
                                                             "Expected revenue over 30 days" = 'revenue_30' )),
                                 selectInput("selectDimension", 
                                             h3("Add new dimension"),
                                             choices = c("neighbourhood_cleansed", 
                                                         "room_type", 
                                                         "accommodates",
                                                         "bedrooms",
                                                         "beds"),
                                             selected = "beds"
                                 ),
                                 checkboxGroupInput("checkBoxAggregation", 
                                                    h3("Select the aggregation type"),
                                                    choices = list("average" = "average", 
                                                                   "median" = "median"),
                                                    selected = "average"
                                 ),
                                 radioButtons("radioButtonsPlot", 
                                              h3("Select the plot type"),
                                              choices = list("histogram" = "histogram", 
                                                             "density" = "density",
                                                             "boxplot" = "boxplot"),
                                              selected = "histogram"
                                 ),
                                 width = 3
                             ),
                             mainPanel(
                                 plotOutput("Plot"),
                                 tableOutput('table'),
                                 width = 9
                             )
                             
                         )
                ),
                
                tabPanel("2) Deep dive into a selected city", 
                         br(), 
                         textOutput("out2"), 
                         sidebarPanel(
                             selectInput("selectCitiesMap", 
                                         h3("Filter by city"),
                                         multiple = FALSE,
                                         choices = cities
                             ),
                             dateRangeInput("datesMap", 
                                            h3("Date range"), 
                                            format = "yyyy-mm-dd",
                                            start = "2020-01-01",
                                            end = "2020-12-12"),
                             radioButtons("radioFeaturesMap", 
                                          h3("Select the feature"),
                                          choices = list("availability over 30 days" = 'availability_30', 
                                                         "Average price over 30 days" = 'price_30',
                                                         "Expected revenue over 30 days" = 'revenue_30' )),
                             selectInput("selectDimensionMap", 
                                         h3("Add new dimension"),
                                         choices = c("neighbourhood_cleansed", 
                                                     "room_type", 
                                                     "accommodates",
                                                     "bedrooms",
                                                     "beds"),
                                         selected = "beds"
                             ),
                             checkboxGroupInput("checkBoxAggregationMap", 
                                                h3("Select the aggregation type"),
                                                choices = list("average" = "average", 
                                                               "median" = "median"),
                                                selected = "average"
                             ),
                             radioButtons("radioButtonsPlotMap", 
                                          h3("Select the plot type"),
                                          choices = list("histogram" = "histogram", 
                                                         "density" = "density",
                                                         "boxplot" = "boxplot"),
                                          selected = "histogram"
                             ),
                             width = 3
                             
                        ),
                        mainPanel(
                            plotOutput("PlotMap"),
                            tableOutput('tableMap'),
                            leafletOutput("mymap"),
                            width = 9
                        )
                )
                
            )

    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    # Tab 1
    
    output$Plot <- renderPlot({
        df_city <- df 
        if (input$selectDimension == 'neighbourhood_cleansed') {
            top_10_neighborhood <- df_city %>%
                group_by(city) %>%
                filter(city %in% input$selectCities) %>%
                count(neighbourhood_cleansed) %>%
                arrange(city, desc(n)) %>%
                top_n(10, n)
            print(top_10_neighborhood)
            top_10_neighborhood$neighbourhood_cleansed <- factor(top_10_neighborhood$neighbourhood_cleansed, levels = top_10_neighborhood$neighbourhood_cleansed[order(top_10_neighborhood$n)])
            ggplot(top_10_neighborhood, aes(x = neighbourhood_cleansed, fill= city)) + 
                geom_bar(aes(x = neighbourhood_cleansed, y = n), stat="identity",  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
                coord_flip() + xlab("Neighborhoods") + ylab("Proportions") 
            
        }  
        else {
            df_city <- df_city %>%
                filter(city %in% input$selectCities & input$dates[1] <=  data_date & input$dates[2] >=  data_date)
            if (input$radioButtonsPlot == 'boxplot') {
                ggplot(df_city, aes_string('city', input$radioFeatures, fill='city')) +
                    geom_boxplot(aes_string(colour = input$selectDimension), outlier.shape = NA) +
                    scale_y_continuous(limits = quantile(df_city[input$radioFeatures], c(0.1, 0.9), na.rm = T)) +
                    scale_fill_brewer(palette="Set1")
            }
            else if (input$radioButtonsPlot == 'histogram') {
                ggplot(df_city, aes_string(input$selectDimension)) +
                    geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
                    geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                                  y= ((..count..)/sum(..count..))), stat="count",
                              vjust = -.25) + theme(legend.position = "none") + facet_wrap(~ city) +
                    ylab("Proportion")
            }
            else if (input$radioButtonsPlot == 'density') {
                ggplot(df_city[!is.na(df_city[input$radioFeatures]),], aes_string(input$radioFeatures, fill = 'city')) +
                    geom_density(alpha = 0.4) +
                    ylab("density")
            }
        }
        

    })


    output$table <- renderTable({
        df_city <- df %>%
            filter(city %in% input$selectCities &
                       input$dates[1] <=  data_date &
                       input$dates[2] >=  data_date &
                       !is.na(df[input$selectDimension]))
        if (input$checkBoxAggregation[1] == 'average' & length(input$checkBoxAggregation) == 1) {
            df_city %>%
                group_by(city) %>%
                summarise(average = mean(!!rlang::sym(input$radioFeatures)))%>%
                print()
        }
        else if  (input$checkBoxAggregation[1] == 'median' & length(input$checkBoxAggregation) == 1) {
            df_city %>%
                group_by(city) %>%
                summarise(median = median(!!rlang::sym(input$radioFeatures)))%>%
                print()
        }
        else if (length(input$checkBoxAggregation) == 2) {
            df_city %>%
                group_by(city) %>%
                summarise(average = mean(!!rlang::sym(input$radioFeatures)),
                          median = median(!!rlang::sym(input$radioFeatures)))%>%
                print()
        }
    })

    # Tab 2
    output$PlotMap <- renderPlot({
        df_city <- df 
        if (input$selectDimensionMap == 'neighbourhood_cleansed') {
            top_10_neighborhood <- df_city %>%
                group_by(city) %>%
                filter(city %in% input$selectCitiesMap) %>%
                count(neighbourhood_cleansed) %>%
                arrange(city, desc(n)) %>%
                top_n(10, n)
            print(top_10_neighborhood)
            top_10_neighborhood$neighbourhood_cleansed <- factor(top_10_neighborhood$neighbourhood_cleansed, levels = top_10_neighborhood$neighbourhood_cleansed[order(top_10_neighborhood$n)])
            ggplot(top_10_neighborhood, aes(x = neighbourhood_cleansed, fill= city)) + 
                geom_bar(aes(x = neighbourhood_cleansed, y = n), stat="identity",  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
                coord_flip() + xlab("Neighborhoods") + ylab("Proportions") 
            
        }  
        else {
            df_city <- df_city %>%
                filter(city %in% input$selectCitiesMap & input$datesMap[1] <=  data_date & input$datesMap[2] >=  data_date)
            if (input$radioButtonsPlotMap == 'boxplot') {
                ggplot(df_city, aes_string('city', input$radioFeaturesMap, fill='city')) +
                    geom_boxplot(aes_string(colour = input$selectDimensionMap), outlier.shape = NA) +
                    scale_y_continuous(limits = quantile(df_city[input$radioFeaturesMap], c(0.1, 0.9), na.rm = T)) +
                    scale_fill_brewer(palette="Set1")
            }
            else if (input$radioButtonsPlotMap == 'histogram') {
                ggplot(df_city, aes_string(input$selectDimensionMap)) +
                    geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
                    geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                                  y= ((..count..)/sum(..count..))), stat="count",
                              vjust = -.25) + theme(legend.position = "none") + facet_wrap(~ city) +
                    ylab("Proportion")
            }
            else if (input$radioButtonsPlotMap == 'density') {
                ggplot(df_city[!is.na(df_city[input$radioFeaturesMap]),], aes_string(input$radioFeaturesMap, fill = 'city')) +
                    geom_density(alpha = 0.4) +
                    ylab("density")
            }
        }
        
        
    })
    
    
    output$tableMap <- renderTable({
        df_city <- df %>%
            filter(city %in% input$selectCitiesMap &
                       input$datesMap[1] <=  data_date &
                       input$datesMap[2] >=  data_date &
                       !is.na(df[input$selectDimensionMap]))
        if (input$checkBoxAggregationMap[1] == 'average' & length(input$checkBoxAggregationMap) == 1) {
            df_city %>%
                group_by(city, !!rlang::sym(input$selectDimensionMap)) %>%
                summarise(average = mean(!!rlang::sym(input$radioFeaturesMap)))%>%
                print()
        }
        else if  (input$checkBoxAggregationMap[1] == 'median' & length(input$checkBoxAggregationMap) == 1) {
            df_city %>%
                group_by(city, !!rlang::sym(input$selectDimensionMap)) %>%
                summarise(median = median(!!rlang::sym(input$radioFeaturesMap)))%>%
                print()
        }
        else if (length(input$checkBoxAggregationMap) == 2) {
            df_city %>%
                group_by(city, !!rlang::sym(input$selectDimensionMap)) %>%
                summarise(average = mean(!!rlang::sym(input$radioFeaturesMap)),
                          median = median(!!rlang::sym(input$radioFeaturesMap)))%>%
                print()
        }
    })
    
    output$mymap <- renderLeaflet({
        df %>%
            filter(city %in% input$selectCitiesMap) %>%
            leaflet() %>%
            addTiles() %>%
            addMarkers(clusterOptions = markerClusterOptions(), popup = ~ paste0( "City: ",
                                                                                   city ,
                                                                                   "</br>",
                                                                                  "Neighbourhood: ",
                                                                                  neighbourhood_cleansed,
                                                                                  "</br>",
                                                                                  "Price: ",
                                                                                  price
            ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
