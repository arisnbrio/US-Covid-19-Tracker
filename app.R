library(dplyr)
library(shinydashboard)
library(bigrquery)
library(tidyverse)
library(DBI)
library(wk)
library(shiny)
library(leaflet)
library(httr)
library(RColorBrewer)
library(pool)

# Establishing connection



con <- dbPool(
    drv = bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "covid19_open_data",
    billing = "covid19rshinyapp"
)



bq_auth(path = "/Users/aris/Desktop/Website/RShiny US COVID APP/covid19rshinyapp-c16e515235ee.json")



summary_dbl <- tbl(con,"covid19_open_data") %>% 
    filter(country_name == "United States of America",
           !is.na(subregion1_name)) %>%
    select(date, subregion1_name, subregion2_name,
           new_confirmed,new_deceased, new_tested, 
           latitude, longitude,
           cumulative_confirmed,cumulative_deceased, cumulative_tested) %>% 
    rename(state = subregion1_name, city_county = subregion2_name) %>%
    mutate(new_tested = abs(new_tested),
           new_confirmed = abs(new_confirmed),
           new_deceased = abs(new_deceased))




state_names_list <- summary_dbl %>% select(state) %>% 
    distinct(state) %>% 
    filter(!is.na(state)) %>%
    arrange(state) %>% 
    collect() %>% rename(State = state)



ui <- dashboardPage(
    dashboardHeader(title = "U.S COVID-19 Tracker"),
    dashboardSidebar(
        
        title = "Options",
        
        # first input
        dateRangeInput('date',
                       strong('Date Range'),
                       start = "2021-01-01",
                       min = "2020-01-22",
                       max = as.character(Sys.Date()),
                       end = "2021-01-29"),
        textOutput("DateRange"),
        
        # second input
        selectInput('state',
                    'State',
                    choices = c("U.S", state_names_list)),
        
        # third input
        sliderInput("radius", "Adjust Size of Radius:",
                    min = 1, max = 10, value = 5),
        
        # third input
        sliderInput("weight", "Adjust Weight of Circles:",
                    min = 1, max = 10, value = 1),
        
        radioButtons("newx",
                     "Track",
                     choices = c("New Confirmed Cases", "New Deaths", "New Tested"))
    ),
    dashboardBody(
        fluidRow(
            box(width = 12,
                leafletOutput("leafmap")
                
            )),
        fluidRow(
            valueBoxOutput("confirmedTot"),
            valueBoxOutput("deathsTot"),
            valueBoxOutput("testedTot")
        ),
        fluidRow(
            box(
                width = 500, solidHeader=T,
                collapsible = T,
                plotOutput("plotSumm")))
    )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    summary_df <- reactive({
        summary_dbl %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2],
                   !is.na(city_county)) %>% # removes state totals
            select(date, state, city_county, new_confirmed, new_deceased, new_tested, latitude, longitude) %>%
            group_by(city_county, state, latitude, longitude) %>% 
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% 
            collect() %>% mutate(popup_info = paste("<b>",state,"</b><br/>",
                                                    "<b>",city_county,"</b><br/>",
                                                    "New Confirmed:", new_confirmed, "<br/>",
                                                    "New Deaths:", new_deceased, "<br/>",
                                                    "New Tested:", new_tested, "<br/>"))
    })
    
    summary_state <- reactive({
        summary_dbl %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2],
                   is.na(city_county),
                   state == !!input$state) %>%
            group_by(state, latitude, longitude) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>%
            collect() %>% arrange(desc(new_confirmed)) %>% head(1)
        
    })
    summary_country <- reactive({
        tbl(con,"covid19_open_data") %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2],
                   country_name == "United States of America",
                   is.na(subregion1_name)) %>% 
            group_by(country_name) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()
    })
    plot_func_state <- reactive({
        summary_dbl %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2],
                   is.na(city_county),
                   state == !!input$state) %>%
            group_by(date, state) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()
    })
    plot_func_country <- reactive({
        tbl(con,"covid19_open_data") %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2],
                   country_name == "United States of America",
                   new_confirmed != 0,
                   is.na(subregion1_name)) %>% 
            group_by(date, country_name) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% 
            collect()
        
        
    })
    
    output$leafmap <- renderLeaflet({
        
        basemap <- leaflet() %>% addProviderTiles(providers$OpenStreetMap)
        
        case.colconf <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_confirmed,na.color = 'transparent')
        case.coldeath <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_deceased,na.color = 'transparent')
        case.coltest <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_tested,na.color = 'transparent')
        
        if (input$state == 'U.S' && input$newx == 'New Confirmed Cases'){
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_confirmed)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.colconf(summary_df()$new_confirmed)) %>%
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_confirmed,
                    pal = case.colconf,
                    title = "New Cases<br/>by County") %>% 
                setView(lng = -98.583, lat = 39.833, zoom = 4) 
        } else if(input$state == 'U.S' && input$newx == 'New Deaths'){
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_deceased)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.coldeath(summary_df()$new_deceased)) %>%
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_deceased,
                    pal = case.coldeath,
                    title = "New Deaths<br/>by County") %>% 
                setView(lng = -98.583, lat = 39.833, zoom = 4)
        } else if(input$state == 'U.S' && input$newx == 'New Tested'){
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_tested)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.coltest(summary_df()$new_tested)) %>% 
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_tested,
                    pal = case.coltest,
                    title = "New Tested<br/>by County") %>% 
                setView(lng = -98.583, lat = 39.833, zoom = 4)
        } else if(input$state != 'U.S' && input$newx == "New Confirmed Cases"){
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_confirmed)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.colconf(summary_df()$new_confirmed)) %>%
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_confirmed,
                    pal = case.colconf,
                    title = "New Cases<br/>by County") %>% 
                setView(lng = summary_state()$longitude, lat = summary_state()$latitude, zoom = 5) 
        } else if(input$state != 'U.S' && input$newx == 'New Deaths'){
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_deceased)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.coldeath(summary_df()$new_deceased)) %>%
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_deceased,
                    pal = case.coldeath,
                    title = "New Deaths<br/>by County") %>% 
                setView(lng = summary_state()$longitude, lat = summary_state()$latitude, zoom = 5)
        } else {
            basemap %>% 
                addCircleMarkers(data = summary_df(), 
                                 lat = ~latitude, 
                                 lng = ~longitude,
                                 weight = input$weight,
                                 radius = ~(new_tested)^(input$radius/25),
                                 popup = ~popup_info,
                                 color = ~case.coltest(summary_df()$new_tested)) %>%
                addLegend(
                    position = 'topright',
                    values = summary_df()$new_tested,
                    pal = case.coltest,
                    title = "New Tested<br/>by County") %>% 
                setView(lng = summary_state()$longitude, lat = summary_state()$latitude, zoom = 5)
        }
    }) 
    output$confirmedTot <- renderValueBox({
        valueBox(
            if(input$state == "U.S"){
                summary_country()$new_confirmed
            } else{
                summary_state()$new_confirmed   
            },
            "New Cases",
            icon = icon("check-circle"), color = "blue"
        )
        
        
    })
    output$deathsTot <- renderValueBox({
        valueBox(
            if(input$state == "U.S"){
                summary_country()$new_deceased
            } else{
                summary_state()$new_deceased
            },
            "New Deaths",
            icon = icon("skull-crossbones"), color = "red"
        )
        
        
    })
    output$testedTot <- renderValueBox({
        valueBox(
            if(input$state == "U.S"){
                summary_country()$new_tested
            } else{
                summary_state()$new_tested
            },
            "New Tested",
            icon = icon("syringe"), color = "teal"
        )
        
        
    })
    
    
    output$DateRange <- renderText({
        # make sure end date later than start date
        validate(
            need(input$date[2] > input$date[1], "  End date is earlier than start date"
            ))
    })
    
    output$plotSumm <- renderPlot({
        if(input$state == 'U.S'){
            ggplot(plot_func_country()) +
                geom_line(aes(x = date, y = new_deceased, color = "Deaths"), size = 1) +
                geom_line(aes(x = date, y = new_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_line(aes(x = date, y = new_tested, color = "Tested"), size = 1) +
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue",
                                              "Tested" = "cyan2")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = "New Confirmed/Deaths/Recovered Daily", x ="Date", y = "Count")
        } else{
            ggplot(plot_func_state()) +
                geom_line(aes(x = date, y = new_deceased, color = "Deaths"), size = 1) +
                geom_line(aes(x = date, y = new_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_line(aes(x = date, y = new_tested, color = "Tested"), size = 1) +
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue",
                                              "Tested" = "cyan2")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = "New Confirmed/Deaths/Recovered Daily", x ="Date", y = "Count")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)