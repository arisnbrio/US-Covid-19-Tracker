library(dplyr)
library(dbplyr)
library(shinydashboard)
library(bigrquery)
library(DBI)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(readr)

# Establishing connection

# reading in external files to get population per county/demographic data to join
index <- read.csv("https://storage.googleapis.com/covid19-open-data/v2/index.csv")
demo <- read.csv("https://storage.googleapis.com/covid19-open-data/v2/demographics.csv")

# population for each county
pop <- index %>% inner_join(demo, by = "key") %>% 
    dplyr::filter(country_name == "United States of America",
           !is.na(subregion1_name)) %>%
    select(key, subregion1_name, subregion2_name, population) %>%
    rename(state = subregion1_name, 
           city_county = subregion2_name, 
           location_key = key) 
pop_us <- index %>% inner_join(demo, by = "key") %>% 
    dplyr::filter(key == "US") %>%
    rename(state = subregion1_name, 
           city_county = subregion2_name, 
           location_key = key) %>% select(population)

pop <- pop[!(is.na(pop$city_county) | pop$city_county ==""), ] # removed "" values

con <- dbConnect(
    drv = bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "covid19_open_data",
    billing = "covid19rshinyapp"
) # connection to bigquery

# authenticate
#bq_auth(path = "covid19rshinyapp-c2fececae643.json")
bq_auth(path = "covid19rshinyapp-ce07a0c7376a.json")



summary_dbl <- tbl(con,"covid19_open_data") %>% 
    dplyr::filter(country_name == "United States of America",
           !is.na(subregion1_name)) %>%
    select(location_key, date, subregion1_name, subregion2_name,
           new_confirmed,new_deceased, new_tested, 
           latitude, longitude,
           cumulative_confirmed,cumulative_deceased, cumulative_tested) %>% 
    rename(state = subregion1_name, city_county = subregion2_name) %>%
    mutate(new_tested = 1,
           new_confirmed = abs(new_confirmed),
           new_deceased = abs(new_deceased)) %>% 
    mutate(new_tested = as.numeric(new_tested))
# some data cleaning to focus on the U.S
# negative values in tested/confirmed/deceased fixed
# renaming into easier names

# cumulative values for box values
cumulative_stats <- tbl(con,"covid19_open_data") %>%
    dplyr::filter(country_name == "United States of America",
           is.na(subregion1_name)) %>% 
    mutate(new_tested = 1,
           new_confirmed = abs(new_confirmed),
           new_deceased = abs(new_deceased)) %>% 
    mutate(new_tested = as.numeric(new_tested)) %>%
    group_by(country_name) %>%
    summarize(
        new_confirmed = sum(new_confirmed,na.rm=TRUE),
        new_deceased = sum(new_deceased,na.rm = TRUE),
        new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()

# input for states
state_names_list <- summary_dbl %>% select(state) %>% 
    distinct(state) %>% 
    dplyr::filter(!is.na(state)) %>%
    arrange(state) %>% 
    collect() %>% rename(State = state) # list of states in dataset alphabetically

ui <- dashboardPage(
    dashboardHeader(title = "U.S COVID-19 Tracker"),
    dashboardSidebar( # in sidebar tab
        title = "Options",
        
        # date input
        dateRangeInput('date',
                       strong('Date Range'),
                       start = Sys.Date()-21,
                       min = "2020-01-22",
                       max = Sys.Date(),
                       end = Sys.Date()),
        textOutput("DateRange"), # for error
        
        #input for selecting state 
        selectInput('state',
                    'State',
                    choices = c("U.S", state_names_list)),
        
        # true/false to scale map
        checkboxInput("per100","Scale Map to per 100,000 Population",value=TRUE),
        
        #  what to track
        radioButtons("newx",
                     "Track",
                     choices = c("New Confirmed Cases", "New Deaths")),
        
        # visual controls
        sliderInput("radius", "Adjust Size of Radius:",
                    min = 1, max = 10, value = 6),
        
        
        sliderInput("weight", "Adjust Weight of Circles:",
                    min = 1, max = 10, value = 2)
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("confirmedTot",width = 6), # boxes
            valueBoxOutput("deathsTot",width = 6)),
            #valueBoxOutput("testedTot")),
        fluidRow(
            box(width = 12,
                leafletOutput("leafmap"), # output leaflet map in main body
            actionButton("button","Reset Map View"))),
        fluidRow(
            valueBoxOutput("confirmedSum",width = 6), # boxes
            valueBoxOutput("deathsSum",width = 6)),
            #valueBoxOutput("testedSum")),
        fluidRow(
            box(
                radioButtons("newx2",
                             "Top 10 States by Date Range For: ",
                             choices = c("New Confirmed Cases", "New Deaths")),
                width = 500, solidHeader=T,
                collapsible = T,
                plotOutput("top10state"))
        ),
        fluidRow(
            box(
                width = 20, solidHeader=T,
                collapsible = T,
                plotOutput("plotSumm"))
    )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    summary_df <- reactive({
        if(input$per100==TRUE){ # if scaled
            summary_dbl %>%
                dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2], # dplyr::filter by input date
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
                                          "New Deaths:", new_deceased, "<br/>")) 
            # popup_info adds labels over the bubbles
            
            
        }else{ # if not scaled (raw case numbers)
            summary_dbl %>%
                dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2],
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
                                          "New Deaths:", new_deceased, "<br/>"))
        }
    })
    
    
    # state specific info
    summary_state <- reactive({
        summary_dbl %>%
            dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2],
                   is.na(city_county),
                   state == !!input$state) %>%
            mutate(new_tested = 1,
                   new_confirmed = abs(new_confirmed),
                   new_deceased = abs(new_deceased)) %>% 
            mutate(new_tested = as.numeric(new_tested)) %>%
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
            dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2],
                   country_name == "United States of America",
                   is.na(subregion1_name)) %>% 
            mutate(new_tested = 1,
                   new_confirmed = abs(new_confirmed),
                   new_deceased = abs(new_deceased)) %>% 
            mutate(new_tested = as.numeric(new_tested)) %>%
            group_by(country_name) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()
    })
    
    # plot for state
    plot_func_state <- reactive({
        summary_dbl %>%
            dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2],
                   is.na(city_county),
                   state == !!input$state) %>%
            mutate(new_tested = 1,
                   new_confirmed = abs(new_confirmed),
                   new_deceased = abs(new_deceased)) %>% 
            mutate(new_tested = as.numeric(new_tested)) %>%
            group_by(date, state) %>%
            summarize(
                new_confirmed = sum(new_confirmed,na.rm=TRUE),
                new_deceased = sum(new_deceased,na.rm = TRUE),
                new_tested = sum(new_tested,na.rm =TRUE)) %>% collect()
    })
    # plot for country
    plot_func_country <- reactive({
        tbl(con,"covid19_open_data") %>%
            dplyr::filter(date >= !!input$date[1] && date <= !!input$date[2],
                   country_name == "United States of America",
                   new_confirmed != 0,
                   is.na(subregion1_name)) %>% 
            mutate(new_tested = 1,
                   new_confirmed = abs(new_confirmed),
                   new_deceased = abs(new_deceased)) %>% 
            mutate(new_tested = as.numeric(new_tested)) %>%
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
        leaflet() %>% addProviderTiles(providers$CartoDB) %>% 
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
    output$confirmedTot <- renderValueBox({ # overall cumulative confirmed
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(cumulative_stats$new_confirmed)),
            "Cumulative Cases",
            icon = icon("check-circle"), color = "black"
        )
        
        
    })
    
    output$deathsTot <- renderValueBox({ # overall cumulative deaths
        valueBox(
            scales::label_number_si(accuracy=0.1)(as.numeric(cumulative_stats$new_deceased)),
            "Cumulative Deaths",
            icon = icon("skull-crossbones"), color = "black"
        )
        
        
    })
    
#    output$testedTot <- renderValueBox({ # overall cumulative tests
#        valueBox(
#            scales::label_number_si(accuracy=0.1)(as.numeric(cumulative_stats$new_tested)),
#            "Cumulative Tests",
#            icon = icon("vials"), color = "black"
#        )
        
        
#    })
    
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
    
    #output$testedSum <- renderValueBox({ 
        #valueBox(
            #if(input$state == "U.S"){
                #scales::label_number_si(accuracy=0.1)(as.numeric(summary_country()$new_tested))
          #  } else{
           #     scales::label_number_si(accuracy=0.1)(as.numeric(summary_state()$new_tested))
           # },
            #paste("Cumulative New Tests by Date Range(",input$state,")"),
            #icon = icon("vials"), color = "teal"
        #)
        
        
    #})

    #error messages for date
    
    output$DateRange <- renderText({
        # make sure end date later than start date
        shiny::validate(
            need(input$date[2] > input$date[1], "  Error: End date is earlier than start date"
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
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = "Number of New Confirmed/Deaths \n in the U.S by Date Range", x ="Date", y = "Count")
        } else{
            ggplot(plot_func_state()) +
                geom_smooth(aes(x = date, y = new_deceased, color = "Deaths"), size = 1) +
                geom_point(aes(x = date, y = new_deceased, color = "Deaths")) +
                geom_smooth(aes(x = date, y = new_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_point(aes(x = date, y = new_confirmed, color = "Confirmed Cases")) +
                scale_color_manual(name = "Legend", 
                                   values = c("Deaths" = "red", 
                                              "Confirmed Cases" = "blue")) +
                theme_minimal() +
                scale_y_log10() +
                labs(title = paste("Number of New Confirmed/Deaths \n in",input$state, "by Date Range"), x ="Date", y = "Count")
        }
    })
    output$top10state <- renderPlot({
        if(input$newx2 == "New Confirmed Cases" && input$per100 == FALSE){
        ggplot(summary_df() %>% group_by(state) %>%
                   summarize(
                       new_confirmed = sum(new_confirmed,na.rm=TRUE), # sums
                       new_deceased = sum(new_deceased,na.rm = TRUE)) %>% 
                   arrange(desc(new_confirmed)) %>% head(10), 
               aes(x = reorder(state, -new_confirmed), y = new_confirmed,fill=state)) +
            geom_bar(stat='identity') + theme_minimal() + 
            labs(title = "Top 10 States with Most New Confirmed Cases", x = "State", y = "Confirmed Cases")
        } else if(input$newx2 == "New Confirmed Cases" && input$per100 == TRUE){
            ggplot(summary_df() %>% group_by(state) %>%
                       summarize(
                           new_confirmed = sum(new_confirmed,na.rm=TRUE), # sums
                           new_deceased = sum(new_deceased,na.rm = TRUE)) %>% 
                       arrange(desc(new_confirmed)) %>% head(10), 
                   aes(x = reorder(state, -new_confirmed), y = new_confirmed,fill=state)) +
                geom_bar(stat='identity') + theme_minimal() + 
                labs(title = "Top 10 States with Most New Confirmed Cases (Per 100,000 Population)", x = "State", y = "Confirmed Cases")
            
        } else if(input$newx2 == "New Deaths" && input$per100 == TRUE){
            ggplot(summary_df() %>% group_by(state) %>%
                       summarize(
                           new_confirmed = sum(new_confirmed,na.rm=TRUE), # sums
                           new_deceased = sum(new_deceased,na.rm = TRUE)) %>% 
                       arrange(desc(new_confirmed)) %>% head(10), 
                   aes(x = reorder(state, -new_deceased), y = new_deceased, fill=state)) +
                geom_bar(stat='identity') + theme_minimal() + 
                labs(title = "Top 10 States with Most New Deaths (Per 100,000 Population)", x = "State", y = "New Deaths")
        } else {
            ggplot(summary_df() %>% group_by(state) %>%
                       summarize(
                           new_confirmed = sum(new_confirmed,na.rm=TRUE), # sums
                           new_deceased = sum(new_deceased,na.rm = TRUE)) %>% 
                       arrange(desc(new_confirmed)) %>% head(10), 
                   aes(x = reorder(state, -new_deceased), y = new_deceased, fill=state)) +
                geom_bar(stat='identity') + theme_minimal() + 
                labs(title = "Top 10 States with Most New Deaths", x = "State", y = "New Deaths")  
        }
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
