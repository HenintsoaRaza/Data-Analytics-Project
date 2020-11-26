library(shiny)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(htmltools)
library(DT)
library(plotly)
library(rsconnect)


setwd('../')

source("Scripts/load_data.R")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////                          /////////////////////////////////////////
# /////////////////////////////////////////         UI Object        //////////////////////////////////////////
# ////////////////////////////////////////                          ///////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Define UI for application that draws a histogram
ui <- fluidPage( 
        theme = shinytheme('simplex'),
        navbarPage(
            title = div( tags$img(src = "logo2.png",height="15%", width="15%"), style = "text-align:center;")  ,
            tabPanel(div("HOME", style = "font-size: 15px;"),
                     tags$img(src='background2.jpg', style = 'width: 100% ; height: 100%')
                     ),
            tabPanel(div("1. Compare multiple cities", style = "font-size: 15px"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("cities1", 
                                     h4("Filter by city"),
                                     multiple = TRUE,
                                     choices = cities
                         ),
                         dateRangeInput("dates1", 
                                        h4("Date range"), 
                                        format = "yyyy-mm-dd",
                                        start = "2020-01-01",
                                        end = "2020-12-12"),
                         radioButtons("feature1", 
                                      h4("Select the feature"),
                                      choices = list("Availability over 30 days" = 'availability_30', 
                                                     "Average price over 30 days" = 'price_30',
                                                     "Expected revenue over 30 days" = 'revenue_30' )),
                         selectInput("dimension1", 
                                     h4("Add new dimension"),
                                     choices = c("neighbourhood_cleansed", 
                                                 "room_type", 
                                                 "accommodates",
                                                 "bedrooms",
                                                 "beds"),
                                     selected = "beds"
                         ),
                         h4("Select the aggregation type"),
                         checkboxInput("average1", "Average", value = FALSE),
                         checkboxInput("median1", "Median", value = FALSE),
                         radioButtons("choicePlot1", 
                                      h4("Select the plot type"),
                                      choices = list("histogram" = "histogram", 
                                                     "density" = "density",
                                                     "boxplot" = "boxplot",
                                                     "proportion" = "proportion"),
                                      selected = "histogram"
                         ),
                         submitButton("Submit"),
                         width = 3
                     ),
                     mainPanel(
                         plotOutput("plot1"),br(),br(),
                         dataTableOutput('table1'),
                         width = 9
                     )
                 )
            ),
            
            tabPanel(div("2. Deep dive into a city", style = "font-size: 15px"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("cities2", 
                                     h4("Filter by city"),
                                     multiple = FALSE,
                                     choices = cities
                         ),
                         dateRangeInput("dates2", 
                                        h4("Date range"), 
                                        format = "yyyy-mm-dd",
                                        start = "2020-01-01",
                                        end = "2020-12-12"),
                         radioButtons("feature2", 
                                      h4("Select the feature"),
                                      choices = list("Availability over 30 days" = 'availability_30', 
                                                     "Average price over 30 days" = 'price_30',
                                                     "Expected revenue over 30 days" = 'revenue_30' )),
                         selectInput("dimension2", 
                                     h4("Add new dimension"),
                                     choices = c("neighbourhood_cleansed", 
                                                 "room_type", 
                                                 "accommodates",
                                                 "bedrooms",
                                                 "beds"),
                                     selected = "beds"
                         ), 
                         h4("Select the aggregation type"),
                         checkboxInput("average2", "Average", value = FALSE),
                         checkboxInput("median2", "Median", value = FALSE),
                         radioButtons("choicePlot2", 
                                      h4("Select the plot type"),
                                      choices = list("histogram" = "histogram", 
                                                     "density" = "density",
                                                     "boxplot" = "boxplot",
                                                     "proportion" = "proportion"),
                                      selected = "histogram"
                         ),
                         submitButton("Submit"),
                         width = 3
                         
                    ),
                    mainPanel(
                        plotlyOutput("plot2"),br(),br(),
                        dataTableOutput('table2'),
                        width = 9
                    )
                ),
                leafletOutput("mymap", height = 750)
            ),
            tabPanel(div("Documentation", style = "font-size: 15px"),
                     br(), 
                     mainPanel(
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                         ),
                         h1("Airbnb Analytics App"),
                         br(),
                         p("Welcome to our Airbnb analytics App that allows you to get some interesting insights on Airbnb statistics."),
                         br(),
                         br(),
                         p("Our app allows you to perform two different types of analysis: "),
                         tags$ol(
                             tags$li("Perform comparison between cities"), 
                             tags$li("Deep dive into a selected city")
                         ),br(),
                         h3("1) Compare multiple cities"),
                         p("The first type of analysis allows you to display insightful graphics (Density, Boxplot and Histogram) 
                     concerning the cities of your choice.",br(),br(),
                           "The main goal of this section is to compare statistics between cities, therefore it is granted to you 
                     to select multiple cities. ",br(),br(),
                           "Our graphs will automatically adapt themselves with respect to your filters 
                     such as date, feature and dimension.",br(),br(),
                           "Finally, you can also display a table that stores the average and median of the selected feature. 
                     All these filters are placed beautifully in a sidebar on the left of the panel."),
                         br(),
                         h3("2) Deep dive into a specific city"),
                         p("The second type of analysis allows you to display more information for a specific city.", br(),
                           " As in the first section, the filters are the same but you can select only one city.", br(),br(),
                           "This finer grained analysis gives you some information such as average and median of 
                     the selected feature according to a dimension you specify. ", br(),br(),
                           "Eventually, we provide you a map, so that you may picture yourself easily and interactivelly 
                     the different accommodations geographically")
                     )
                     )
        )
)

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////                          /////////////////////////////////////////
# /////////////////////////////////////////       Server Object      //////////////////////////////////////////
# ////////////////////////////////////////                          ///////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Define server logic required to draw a histogram
server <- function(input, output) {
    
# ******************************************************************************************
# ****************************************  TAB 1  *****************************************
# ******************************************************************************************
    df_filtered1 <- reactive({
        req(input$cities1)
        df <- load_data(input$cities1, input$dates1[1], input$dates1[2]) 
        return(df)
    })
    
    output$plot1 <- renderPlot({
        
        df_plot <- df_filtered1()
        df_plot <- df_plot %>% filter( !is.na(df_plot[input$feature1]) & (!is.na(df_plot[input$dimension1])  ) )

        
        if (input$dimension1 == 'neighbourhood_cleansed') {
            top_10_neighborhood <- df_plot %>%
                group_by(city) %>%
                count(neighbourhood_cleansed) %>%
                arrange(city, desc(n)) %>%
                top_n(10, n) 
            
            df_plot <- df_plot %>% 
                filter(neighbourhood_cleansed %in% top_10_neighborhood$neighbourhood_cleansed ) 
        }  
        coeff <- 1
        if(input$feature1 == 'availability_30') coeff <- 0.04
        else if(input$feature1 == 'price_30') coeff <- 0.5
        else if(input$feature1 == 'revenue_30') coeff <- 1
        if (input$choicePlot1 == 'boxplot') {
            ggplot(df_plot, aes_string(input$dimension1, input$feature1, fill='city')) +
                geom_boxplot( outlier.shape = NA) +
                scale_y_continuous(limits = quantile(df_plot[input$feature1], c(0.1, 0.9), na.rm = T)) + 
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
        }
        else if (input$choicePlot1 == 'histogram') {
            ggplot(df_plot, aes_string(x = input$dimension1, y = input$feature1, fill='city')) +
                geom_bar(stat='identity') + ylab(input$feature1) +  coord_flip()
        }
        else if (input$choicePlot1 == 'density') {
            ggplot(df_plot[!is.na(df_plot[input$feature1]),], aes_string(input$feature1, fill = 'city')) +
                geom_density(alpha = 0.4) + ylab("density") + xlim(0,1000*coeff)
        }
        else if (input$choicePlot1 == 'proportion'){
            ggplot(df_plot, aes_string(x = input$dimension1, fill= 'city')) +
                geom_bar(aes_string(x = input$dimension1), stat="count",  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
                coord_flip() + xlab(input$dimension1) + ylab("Proportions")
        }
        
        
    })


    output$table1 <- renderDataTable({
        opt = list(pageLength = 5, searching = F)
        
        df_table <- df_filtered1() 
        df_table <- df_table %>% filter( !is.na(df_table[input$feature1]) & (!is.na(df_table[input$dimension1]) ) )
        
        if (input$average1 & !input$median1) {
            df_table %>%
                group_by(city) %>%
                summarise(average = mean(!!rlang::sym(input$feature1))) %>%
                DT::datatable(options = opt)
        }
        else if  (!input$average1 & input$median1) {
            df_table %>%
                group_by(city) %>%
                summarise(median = median(!!rlang::sym(input$feature1)))%>%
                DT::datatable(options = opt)
        }
        else if (input$average1 & input$median1) {
            df_table %>%
                group_by(city) %>%
                summarise(average = mean(!!rlang::sym(input$feature1)),
                          median = median(!!rlang::sym(input$feature1)))%>%
                DT::datatable(options = opt)
        }
    })

    # ******************************************************************************************
    # ****************************************  TAB 2  *****************************************
    # ******************************************************************************************
    
    df_filtered2 <- reactive({
        df <- load_data(input$cities2, input$dates2[1], input$dates2[2] ) 
        return(df)
    })
    
    output$plot2 <- renderPlotly({
        
        df_plot <- df_filtered2() 
        df_plot <- df_plot %>% filter( !is.na(df_plot[input$feature2]) & (!is.na(df_plot[input$dimension2]) ) )

        
        if (input$dimension2 == 'neighbourhood_cleansed') {
            top_10_neighborhood <- df_plot %>%
                group_by(city) %>%
                count(neighbourhood_cleansed) %>%
                arrange(city, desc(n)) %>%
                top_n(10, n) 
            
            df_plot <- df_plot %>% 
                filter(neighbourhood_cleansed %in% top_10_neighborhood$neighbourhood_cleansed ) 
            
        }  
        coeff <- 1
        if(input$feature2 == 'availability_30') coeff <- 0.04
        else if(input$feature2 == 'price_30') coeff <- 0.5
        else if(input$feature2 == 'revenue_30') coeff <- 1
        if (input$choicePlot2 == 'boxplot') {
            ggplot(df_plot, aes_string(input$dimension2, input$feature2, fill='city')) +
                geom_boxplot( outlier.shape = NA) +
                scale_y_continuous(limits = quantile(df_plot[input$feature2], c(0.1, 0.9), na.rm = T)) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        }
        else if (input$choicePlot2 == 'histogram') {
            ggplot(df_plot, aes_string(x = input$dimension2, y = input$feature2, fill='city')) +
                geom_bar(stat='identity') + ylab(input$feature2) + coord_flip()
        }
        else if (input$choicePlot2 == 'density') {
            ggplot(df_plot[!is.na(df_plot[input$feature2]),], aes_string(input$feature2, fill = 'city')) +
                geom_density(alpha = 0.4) + ylab("density") + xlim(0,1000*coeff)
        }
        else if (input$choicePlot2 == 'proportion'){
            ggplot(df_plot, aes_string(x = input$dimension2, fill= 'city')) +
                geom_bar(aes_string(x = input$dimension2), stat="count",  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
                coord_flip() + xlab(input$dimension2) + ylab("Proportions")
        }
        
        
        
    })
    
    
    output$table2 <- renderDataTable({
        opt = list(pageLength = 5, searching = F)
        
        df_table <- df_filtered2()
        df_table <- df_table %>% filter( !is.na(df_table[input$feature2]) & (!is.na(df_table[input$dimension2]) ) )
        
        if (input$average2 & !input$median2) {
            df_table %>%
                group_by(city, !!rlang::sym(input$dimension2)) %>%
                summarise(average = mean(!!rlang::sym(input$feature2)))%>%
                DT::datatable(options = opt)
        }
        else if  (!input$average2 & input$median2) {
            df_table %>%
                group_by(city, !!rlang::sym(input$dimension2)) %>%
                summarise(median = median(!!rlang::sym(input$feature2)))%>%
                DT::datatable(options = opt)
        }
        else if (input$average2 & input$median2) {
            df_table %>%
                group_by(city, !!rlang::sym(input$dimension2)) %>%
                summarise(average = mean(!!rlang::sym(input$feature2)),
                          median = median(!!rlang::sym(input$feature2)))%>%
                DT::datatable(options = opt)
        }
    })
    
    output$mymap <- renderLeaflet({
        
        df_filtered2() %>%
            leaflet() %>%
            addTiles() %>%
            addMarkers(clusterOptions = markerClusterOptions(), 
                       popup = ~ paste0( tags$b("City: "), city , "</br>",
                                         tags$b("Neighbourhood: "), neighbourhood_cleansed, "</br>",
                                         tags$b("Price: "), price, " $", "</br>",
                                         tags$a(href = listing_url[1], "Link")
                                         ),
                       label = ~htmlEscape(neighbourhood_cleansed)
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
