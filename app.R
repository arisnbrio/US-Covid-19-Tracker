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
library(jsonlite)
library(lubridate)
library(ggplot2)

# Establishing connection

# reading in external files to get population per county/demographic data to join
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
bq_auth(path = "covid19rshinyapp-2417098f8315.json")


# APIs for latest country population,cases,death totals
latest_tot <- fromJSON("https://covid-api.mmediagroup.fr/v1/cases?country=US")

# APIs for latest vaccine numbers
drops <- c("_2nd_dose_allocations")
vacc_jnj <- fromJSON("https://data.cdc.gov/resource/w9zu-fywh.json?") # vaccines from j&j
vacc_moderna <- fromJSON("https://data.cdc.gov/resource/b7pe-5nws.json?") # vaccines from moderna
vacc_pfizer <- fromJSON("https://data.cdc.gov/resource/saz5-9hgg.json?") # vacc_pfizer

# dropping variables to match so that we can bind them
vacc_moderna <- vacc_moderna[ , !(names(vacc_moderna) %in% drops)] 
vacc_pfizer <- vacc_pfizer[ , !(names(vacc_pfizer) %in% drops)]

vacc_df <- rbind(vacc_jnj,vacc_moderna,vacc_pfizer) # combining the three types of vaccs
# renaming
names(vacc_df)[names(vacc_df) == "_1st_dose_allocations"] <- "dose"
names(vacc_df)[names(vacc_df) == "jurisdiction"] <- "state"
vacc_df$dose <- as.numeric(vacc_df$dose)

vacc_tot <- sum(vacc_df$dose)

# group for plot
vacc_group <- vacc_df %>% group_by(week_of_allocations) %>% summarize(vaccinated = sum(dose))

# removing time from date
vacc_group$week_of_allocations <- substr(vacc_group$week_of_allocations, 0, 10)
vacc_rate <- vacc_group %>% summarize(rate = mean(vaccinated))

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

cumulative_tested <- tbl(con,"covid19_open_data") %>%
    filter(country_name == "United States of America",
           is.na(subregion1_name)) %>% 
    group_by(country_name) %>%
    summarize(
        new_confirmed = sum(new_confirmed,na.rm=TRUE),
        new_deceased = sum(new_deceased,na.rm = TRUE),
        new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()

state_names_list <- summary_dbl %>% select(state) %>% 
    distinct(state) %>% 
    filter(!is.na(state)) %>%
    arrange(state) %>% 
    collect() %>% rename(State = state) # list of states in dataset alphabetically

ui <- dashboardPage(
    dashboardHeader(title = "U.S COVID-19 Tracker"),
    dashboardSidebar( # in sidebar tab
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
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
        checkboxInput("per100","Scale Map to per 100,000 Population",value=FALSE),
        
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
            valueBoxOutput("confirmedTot"), # boxes
            valueBoxOutput("deathsTot"),
            valueBoxOutput("testedTot")),
        fluidRow(
            box(width = 12,
                leafletOutput("leafmap"), # output leaflet map in main body
            actionButton("button","Reset Map View"))),
        fluidRow(
            valueBoxOutput("confirmedSum"), # boxes
            valueBoxOutput("deathsSum"),
            valueBoxOutput("testedSum")),
        fluidRow(
            box(
                width = 500, solidHeader=T,
                collapsible = T,
                plotOutput("plotSumm"))),
    fluidRow(
        valueBoxOutput("vaccTot"),
        valueBoxOutput("vaccPop"),
        valueBoxOutput("vaccRate")),
    fluidRow(
        box(
            width = 500, solidHeader=T,
            collapsible = T,
            plotOutput("plotVacc")))
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
        # change static map options here # default map
        leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% 
            setMaxBounds( lng1 = -10 # boundaries where map can be dragged to
                          , lat1 = 10
                          , lng2 = -180
                          , lat2 = 90) %>% 
            setView(lng = -98.583, lat = 39.833, zoom = 4) # view of US
    })
    observe({
        # color palettes
        case.colconf <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_confirmed,na.color = 'transparent')
        case.coldeath <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_deceased,na.color = 'transparent')
        case.coltest <- colorNumeric(rev(brewer.pal(5, name = "Spectral")),summary_df()$new_tested,na.color = 'transparent')
        
        if (input$state == 'U.S' && input$newx == 'New Confirmed Cases'){
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Cases per County<br/>by Date Range")
        } else if(input$state == 'U.S' && input$newx == 'New Deaths'){
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Deaths per County<br/>by Date Range")
        } else if(input$state == 'U.S' && input$newx == 'New Tested'){
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Tested per County<br/>by Date Range")
        } else if(input$state != 'U.S' && input$newx == "New Confirmed Cases"){
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Cases per County<br/>by Date Range") 
        } else if(input$state != 'U.S' && input$newx == 'New Deaths'){
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Deaths per County<br/>by Date Range") 
        } else {
            leafletProxy("leafmap") %>% clearMarkers() %>% clearControls() %>%
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
                    title = "New Tested per County<br/>by Date Range")
        }
    })
    
    observeEvent(input$button, { # press button to reset map to default
        
        leafletProxy("leafmap") %>% setView(lng = -98.583, lat = 39.833, zoom = 4) # view of US
        
    })
    
    observeEvent(input$state, { # sends camera to state
        
        if(input$state == "U.S"){
        leafletProxy("leafmap") %>% setView(lng = -98.583, lat = 39.833, zoom = 4) # view of US
        } else
        leafletProxy("leafmap") %>% setView(lng = summary_state()$longitude, lat = summary_state()$latitude, zoom = 5.5)
    })
    # box values
    output$confirmedTot <- renderValueBox({ 
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(latest_tot$All$confirmed)),
            "Cumulative Cases",
            icon = icon("check-circle"), color = "black"
        )
        
        
    })
    output$confirmedSum <- renderValueBox({ 
        valueBox(
            if(input$state == "U.S"){
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_country()$new_confirmed))
            } else {
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_state()$new_confirmed))
            },
            paste("Cumulative New Cases by Date Range(",input$state,")"),
            icon = icon("check-circle"), color = "blue"
        )
        
        
    })
    
    output$deathsTot <- renderValueBox({
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(latest_tot$All$deaths)),
            "Cumulative Deaths",
            icon = icon("skull-crossbones"), color = "black"
        )
        
        
    })
    
    output$deathsSum <- renderValueBox({ 
        valueBox(
            if(input$state == "U.S"){
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_country()$new_deceased))
            } else{
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_state()$new_deceased))
            },
            paste("Cumulative New Deaths by Date Range(",input$state,")"),
            icon = icon("skull-crossbones"), color = "red"
        )
        
        
    })
    output$vaccTot <- renderValueBox({
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(vacc_tot)),
                "Total Cumulative Vaccines Allocated \n (J&J, Pfizer, Moderna)",
                icon = icon("syringe"), color = "purple"
        )
        
        
    })
    
    output$testedSum <- renderValueBox({ 
        valueBox(
            if(input$state == "U.S"){
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_country()$new_tested))
            } else{
                scales::label_number_si(accuracy=0.1)(as.numeric(summary_state()$new_tested))
            },
            paste("Cumulative New Tests by Date Range(",input$state,")"),
            icon = icon("vials"), color = "teal"
        )
        
        
    })
    output$vaccPop <- renderValueBox({ # vaccination vs population progress
        valueBox(
            paste(round((vacc_tot/latest_tot$All$population * 100),2),"%"),
            "Vaccination of Population Progress",
            icon = icon("percent"), color = "purple"
        )
        
        
    })
    output$vaccRate<- renderValueBox({ # vaccination vs population progress
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(vacc_rate)),
            "Rate of Vaccines Allocated per Week",
            icon = icon("wave-square"), color = "purple"
        )
        
        
    })
    output$testedTot <- renderValueBox({ # tests total output
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(cumulative_tested$new_tested)),
            "Cumulative Tests",
            icon = icon("vials"), color = "black"
        )
        
        
    })
    #error messages for date
    
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
                labs(title = "Number of New Confirmed/Deaths/Tests Daily \n in the U.S", x ="Date", y = "Count")
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
                labs(title = paste("Number of New Confirmed/Deaths/Tests Daily \n in",input$state), x ="Date", y = "Count")
        }
    })
    output$plotVacc <- renderPlot({ # plot for vaccinations
        ggplot(vacc_group, aes(x = week_of_allocations, y = vaccinated)) + 
            geom_line(group = 1, color = "purple") + geom_point() +
            theme_minimal() +
            labs(title = "Number of Vaccinations Allocated \n in the U.S by Week", x ="Date", y = "Count") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)