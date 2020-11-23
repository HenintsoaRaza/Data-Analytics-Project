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
                tabPanel("Tab 1",
                         br(),
                         sidebarLayout(
                             sidebarPanel(
                                 #checkboxGroupInput("checkBoxCountry", 
                                 #                   h3("Filter by country "),
                                 #                   choiceNames = countries,
                                 #                   choiceValues = countries
                                 #),
                                 selectInput("selectCities", 
                                             h3("Filter by city"),
                                             multiple = TRUE,
                                             choices = cities
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
                                                         "property_type", 
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
                                 textInput("box3", "Enter Tab 3 Text:", value = "Tab 3!"),
                                 width = 3
                             ),
                             mainPanel(
                                 textOutput("out1"),
                                 plotOutput("Plot"),
                                 width = 9
                             )
                             
                         )
                         
                ),
                
                
                tabPanel("Tab 2", 
                         br(), 
                         textOutput("out2"), 
                         sidebarPanel(
                             textInput("box2", "Enter Tab 2 Text:", value = "Tab 2!")
                         )
                )
            )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    # Tab 1
    #checkboxGroupInput Country :
    output$out1<- renderText({
        countrySelected <- paste(input$checkBoxCountry, collapse = ", ")
        paste("You chose", countrySelected)
    })

    output$Plot <- renderPlot({
        df_city <- df %>%
            filter(city %in% input$selectCities & input$dates[1] <=  data_date & input$dates[2] >=  data_date)
        if (input$radioButtonsPlot == 'boxplot') {
            ggplot(df_city, aes_string('city', input$radioFeatures, fill='city')) +
                geom_boxplot(aes_string(colour = input$selectDimension), outlier.shape = NA) +
                scale_y_continuous(limits = quantile(df_city[input$radioFeatures], c(0.1, 0.9), na.rm = T)) +
                scale_fill_brewer(palette="Set1")
        }
        else if (input$radioButtonsPlot == 'histogram') {
            ggplot(df_city, aes_string(input$selectDimension))+
                geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count") +
                geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),3)),
                              y= ((..count..)/sum(..count..))), stat="count",
                          vjust = -.25) + theme(legend.position = "none") + facet_wrap(~ city) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                ylab("Proportion")
        }

    })


    output$table <- renderTable({
        df_city <- df %>%
            filter(city %in% input$selectCities &
                       input$dates[1] <=  data_date &
                       input$dates[2] >=  data_date &
                       !is.na(df[input$selectDimension]))
        if (input$checkBoxAggregation == 'average' & length(input$checkBoxAggregation) == 1) {
            df_city %>%
                group_by(city) %>%
                summarise(average = mean(df_city[input$radioFeatures], na.rm = TRUE))%>%
                print()
        }
        else if  (input$checkBoxAggregation == 'median' & length(input$checkBoxAggregation) == 1) {
            df_city %>%
                group_by(city) %>%
                summarise(median = median(df_city[input$radioFeatures], na.rm = TRUE))%>%
                print()
        }
        else if (length(input$checkBoxAggregation) == 2) {
            df_city %>%
                group_by(city) %>%
                summarise(average = average(df_city[input$radioFeatures], na.rm = TRUE),
                          median = median(df_city[input$radioFeatures], na.rm = TRUE))%>%
                print()
        }
    })

    # Tab 2
    output$out2 <- renderText(input$box2)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
