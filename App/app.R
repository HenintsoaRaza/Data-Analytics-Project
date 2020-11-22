#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


l <- c("aaaaaaaa", "blaaaaaaaoblo")

load_data <- function(){
    list_dirs <- list.dirs('./data_cleansed3')
    
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
ui <- fluidPage(
        titlePanel("Airbnb"),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tab 1", 
                                 br(), 
                                 textOutput("out1"), 
                                 sidebarPanel(
                                     checkboxGroupInput("checkBoxCountry", 
                                                        h3("Filter by country "),
                                                        choiceNames = countries,
                                                        choiceValues = countries
                                     ),
                                     selectInput("selectCities", 
                                                 h3("Filter by city"),
                                                 multiple = TRUE,
                                                 choices = cities
                                     ),
                                     dateRangeInput("dates", h3("Date range"), format = "yyyy-mm-dd"),
                                     radioButtons("radioFeatures", 
                                                  h3("Select the feature"),
                                                  choices = list("Availabiliy over 30 days" = 'availabity_30', 
                                                                 "Average price over 30 days" = 'price_30',
                                                                 "Expected revenue over 30 days" = 'revenue_30' )),
                                     selectInput("selectDimension", 
                                                 h3("Add new dimension"),
                                                 choices = c("neighbourhood_cleansed", 
                                                             "property_type", 
                                                             "room_type", 
                                                             "accommodates",
                                                             "bedrooms",
                                                             "beds")
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
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$out1 <- renderText(input$box1)
    output$out2 <- renderText(input$box2)
}

# Run the application 
shinyApp(ui = ui, server = server)
