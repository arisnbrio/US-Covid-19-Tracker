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

# reading in external files to get population/demographic data to join
index <- read.csv("https://storage.googleapis.com/covid19-open-data/v2/index.csv")
demo <- read.csv("https://storage.googleapis.com/covid19-open-data/v2/demographics.csv")

pop <- index %>% inner_join(demo, by = "key") %>% 
  filter(country_name == "United States of America",
         !is.na(subregion1_name)) %>%
  select(key, subregion1_name, subregion2_name, population) %>%
  rename(state = subregion1_name, 
         city_county = subregion2_name, 
         location_key = key) 
pop <- pop[!(is.na(pop$city_county) | pop$city_county ==""), ] # removed "" values

con <- dbPool(
    drv = bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "covid19_open_data",
    billing = "covid19rshinyapp"
) # connection to bigquery


# authenticate
bq_auth(path = "/Users/aris/Desktop/Website/RShiny US COVID APP/covid19rshinyapp-c16e515235ee.json")



summary_dbl <- tbl(con,"covid19_open_data") %>% 
    filter(country_name == "United States of America",
           !is.na(subregion1_name)) %>%
    select(location_key, date, subregion1_name, subregion2_name,
           new_confirmed,new_deceased, new_tested, 
           latitude, longitude,
           cumulative_confirmed,cumulative_deceased, cumulative_tested) %>% 
    rename(state = subregion1_name, city_county = subregion2_name) %>%
    mutate(new_tested = abs(new_tested),
           new_confirmed = abs(new_confirmed),
           new_deceased = abs(new_deceased))
# some data cleaning to focus on the U.S
# negative values in tested/confirmed/deceased fixed
# renaming into easier names



state_names_list <- summary_dbl %>% select(state) %>% 
    distinct(state) %>% 
    filter(!is.na(state)) %>%
    arrange(state) %>% 
    collect() %>% rename(State = state) # list of states in dataset alphabetically



ui <- dashboardPage(
    dashboardHeader(title = "U.S COVID-19 Tracker"),
    dashboardSidebar( # in sidebar tab
        
        title = "Options",
        
        # date input
        dateRangeInput('date',
                       strong('Date Range'),
                       start = "2021-01-01",
                       min = "2020-01-22",
                       max = as.character(Sys.Date()),
                       end = "2021-01-29"),
        textOutput("DateRange"), # for error
        
        #input for selecting state 
        selectInput('state',
                    'State',
                    choices = c("U.S", state_names_list)),
        
        # true/false to scale map
        checkboxInput("per100","Scale Map to per 100,000 people",value=FALSE),
        
        #  what to track
        radioButtons("newx",
                     "Track",
                     choices = c("New Confirmed Cases", "New Deaths", "New Tested")),

        # visual controls
        sliderInput("radius", "Adjust Size of Radius:",
                    min = 1, max = 10, value = 5),
        

        sliderInput("weight", "Adjust Weight of Circles:",
                    min = 1, max = 10, value = 1)
    ),
    dashboardBody(
        fluidRow(
            box(width = 12,
                leafletOutput("leafmap") # output leaflet map in main body
                
            )),
        fluidRow(
            valueBoxOutput("confirmedTot"), # boxes
            valueBoxOutput("deathsTot"),
            valueBoxOutput("testedTot")
        ),
        fluidRow(
            box(
                width = 500, solidHeader=T,
                collapsible = T,
                plotOutput("plotSumm"))) # plotting of date,cases
    )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    summary_df <- reactive({
      if(input$per100==TRUE){ # if scaled
          summary_dbl %>%
            filter(date >= !!input$date[1] && date <= !!input$date[2], # filter by input date
                   !is.na(city_county)) %>% # removes state totals
            group_by(city_county, location_key, latitude, longitude, state) %>% # grouping county together
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE), # sums
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>%
             collect() %>% inner_join(pop, key = "location_key") %>% # joining population data to scale
          mutate( # scale per 100,000 people (population)
            new_confirmed = round((new_confirmed/population*100000),2),
            new_deceased = round((new_deceased/population*100000),2),
            new_tested = round((new_tested/population*100000),2)) %>%
             mutate(popup_info = paste("<b>",state,"</b><br/>",
                                                    "<b>",city_county,"</b><br/>",
                                                    "New Confirmed:", new_confirmed, "<br/>",
                                                    "New Deaths:", new_deceased, "<br/>",
                                                    "New Tested:", new_tested, "<br/>"))
          # popup_info adds labels over the bubbles
        
    
      }else{ # if not scaled (raw case numbers)
        summary_dbl %>%
          filter(date >= !!input$date[1] && date <= !!input$date[2],
                 !is.na(city_county)) %>% # removes state totals
          group_by(city_county, location_key, latitude, longitude, state) %>%
          summarize(
            new_confirmed = sum(new_confirmed,na.rm=TRUE),
            new_deceased = sum(new_deceased,na.rm = TRUE),
            new_tested = sum(new_tested,na.rm =TRUE)) %>%
          collect() %>%
          mutate(popup_info = paste("<b>",state,"</b><br/>",
                                    "<b>",city_county,"</b><br/>",
                                    "New Confirmed:", new_confirmed, "<br/>",
                                    "New Deaths:", new_deceased, "<br/>",
                                    "New Tested:", new_tested, "<br/>"))
      }
    })


    # state specific info
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
    # country specific info
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
    # plot for state
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
    # plot for country
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
    # leafmap
    output$leafmap <- renderLeaflet({
        # change static map options here
        basemap <- leaflet() %>% addProviderTiles("OpenStreetMap") %>% 
            setMaxBounds( lng1 = -10
                          , lat1 = 10
                          , lng2 = -180
                          , lat2 = 90)
        # color palettes
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
                    title = "New Cases<br/>by County)") %>% 
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
    # box values
    output$confirmedTot <- renderValueBox({ 
        valueBox(
            if(input$state == "U.S"){
                summary_country()$new_confirmed
            } else{
                summary_state()$new_confirmed   
            },
            "New Cases (Total)",
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
            "New Deaths (Total)",
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
            "New Tested (Total)",
            icon = icon("syringe"), color = "teal"
        )
        
        
    })
    #error messagesfor date
    
    output$DateRange <- renderText({
        # make sure end date later than start date
        validate(
            need(input$date[2] > input$date[1], "  End date is earlier than start date"
            ))
    })
    # plot output 
    output$plotSumm <- renderPlot({
        if(input$state == 'U.S'){
            ggplot(plot_func_country()) +
                geom_smooth(aes(x = date, y = new_deceased, color = "Deaths"), size = 1) +
                geom_point(aes(x = date, y = new_deceased, color = "Deaths")) +
                geom_smooth(aes(x = date, y = new_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_point(aes(x = date, y = new_confirmed, color = "Confirmed Cases")) +
                geom_smooth(aes(x = date, y = new_tested, color = "Tested"), size = 1) +
                geom_point(aes(x = date, y = new_tested, color = "Tested")) + 
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue",
                                              "Tested" = "cyan2")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = "New Confirmed/Deaths/Recovered Daily \n in the U.S", x ="Date", y = "Count")
        } else{
            ggplot(plot_func_state()) +
                geom_smooth(aes(x = date, y = new_deceased, color = "Deaths"), size = 1) +
                geom_point(aes(x = date, y = new_deceased, color = "Deaths")) +
                geom_smooth(aes(x = date, y = new_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_point(aes(x = date, y = new_confirmed, color = "Confirmed Cases")) +
                geom_smooth(aes(x = date, y = new_tested, color = "Tested"), size = 1) +
                geom_point(aes(x = date, y = new_tested, color = "Tested")) + 
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue",
                                              "Tested" = "cyan2")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = "New Confirmed/Deaths/Recovered Daily \n by State", x ="Date", y = "Count")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)