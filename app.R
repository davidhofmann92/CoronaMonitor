############################################################
#### Corona Virus Shiny App
############################################################
# Clear R's brain
rm(list = ls())

# Load packages
library(raster)
library(tidyverse)
library(lubridate)
library(rworldmap)
library(tmap)
library(rgeos)
library(shiny)
library(shinydashboard)
library(RCurl)
library(DT)
library(countrycode)
library(leaflet)
library(scales)

############################################################
#### Data Handling
############################################################
# Specify the Github links to the different files
links <- list(
    Infections  = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  , Deaths      = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  , Recoveries  = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
)

# Load the data and do some cleaning
dat <- links %>%
  lapply(., read_csv) %>%
  bind_rows(., .id = "Case") %>%
  gather(., key = "Date", value = "ConfirmedCases", 6:ncol(.)) %>%
  rename(., Country = "Country/Region", Region = "Province/State") %>%
  mutate(., CountryRegion = paste(Country, Region, sep = ", \n")) %>%
  mutate(., CountryRegion = gsub(", \nNA", "", CountryRegion)) %>%
  mutate(., Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(., logConfirmedCases = log(ConfirmedCases)) %>%
  mutate(., Date = update(Date, year = 2020)) %>%
  mutate(., CountryCode = countrycode(Country
    , origin = "country.name"
    , destination = "iso3c")
  ) %>%
  arrange(., CountryRegion)

# Those countries without a valid country code are all "Other" in the data-base
dat$CountryCode[is.na(dat$CountryCode)] <- "Other"

# Make the data spatial
coordinates(dat) <- c("Long", "Lat")
crs(dat) <- CRS("+init=epsg:4326")

# Identify total cases (will be displayed on top of the dashboard)
totalCases <- dat@data %>%
  group_by(., Date, Case) %>%
  summarise(., TotalCases = sum(ConfirmedCases)) %>%
  subset(., TotalCases > 0 & Date == max(.[["Date"]]))

# Identify the extent of the data (will be the default plot area on the map)
extent <- extent(dat)
extent <- as(extent, "SpatialPolygons")
crs(extent) <- crs(dat)

############################################################
#### User Interface
############################################################
ui <- dashboardPage(

  # Add title of the App
  dashboardHeader(title = "Corona Monitor"),

  # Add sidebar with several entries
  dashboardSidebar(
    sidebarMenu(

      # Add an entry for the worldwide view
      menuItem("Spatial Spread"
        , tabName = "spatial"
        , icon    = icon("globe-africa")
      ),

      # Add an entry for the country specific view
      menuItem("Time Series"
        , tabName = "timeseries"
        , icon    = icon("chart-line")
      )
    )
  ),

  # Add the main panel
  dashboardBody(

    # We want to have multiple tabs
    tabItems(

      # Add a tab for the worldwide view
      tabItem(tabName = "spatial",

        # A nice title
        fluidRow(
          column(12, align = "center", h1("COVID 19 Spatial Spread"))
          ),

        # Some overview numbers
        fluidRow(

          # Infobox about infections
          infoBox("Confirmed Infections"
            , totalCases$TotalCases[2]
            , icon = icon("bug")
            , col = "black"),

          # Infobox about deaths
          infoBox("Confirmed Deaths"
            , totalCases$TotalCases[1]
            , icon = icon("skull")
            , col = "black"),

          # Infobox about recoveries
          infoBox("Confirmed Recoveries"
            , totalCases$TotalCases[3]
            , icon = icon("heart")
            , col = "black")
        ),

        # Add a spatial visualization of the virus-spread
        fluidRow(
          column(12, leafletOutput("map"))
        ),

        # Selectors for the plot to be displayed
        fluidRow(

          # Selector to select the case
          column(6, align = "center", selectInput(
              inputId   = "selectedCase"
            , label     = "Case"
            , choices   = as.list(sort(unique(dat$Case)))
            , selected  = "Infections"
          )),

          # Slider to select the date (with option to animate)
          column(3, align = "center", sliderInput(
              inputId = "selectedDate"
            , label   = "Date"
            , min     = min(dat$Date)
            , max     = max(dat$Date)
            , value   = max(dat$Date)
            , animate = animationOptions(interval = 400, loop = F)
          ))
        ),

        # Display some text that explains the source of the data
        fluidRow(
          column(12, align = "center"
            , h1("Data")
          )
        ),

        # Display the data as subsetted by the user
        fluidRow(
          column(3),
          column(6, align = "center", DT::dataTableOutput("confirmedCases")),
          column(3)
        ),
        
        # Github link to the shiny app
        fluidRow(
          column(12, align = "center",
            h4(
                "Data was sourced from the"
              , a("John Hopkins University. "
                  , href = "https://github.com/CSSEGISandData/COVID-19"
                  , target = "_blank")
              , "Click "
              , a("here"
                  , href = "https://github.com/davidhofmann92/CoronaMonitor"
                  , target = "_blank")
              , "to get the r code for the shiny app."
            )
          )
        )
      ),

      # Add a tab for the country specific view
      tabItem(tabName = "timeseries",

        # A nice title
        fluidRow(
          column(12, align = "center", h1("Cases Worldwide"))
          ),
        
        # Option to switch to log scale
        fluidRow(
          column(12, align = "center", checkboxInput(
              inputId      = "logscale1"
            , label        = "Log Scale"
            , value        = FALSE
          ))
          ),

        # Add a timeline of the cases worldwide
        fluidRow(
          column(12, plotOutput("worldwidePlot"))
        ),

        # A nice title
        fluidRow(
          column(12, align = "center", h1("Cases by Country"))
          ),
        
        # Country selector
        fluidRow(
          column(12, align = "center", selectInput(
              inputId   = "selectedCountry"
            , label     = "Country"
            , choices   = as.list(sort(unique(dat$Country)))
            , selected  = "Mainland China"))
        ),
        
        # Option to switch to log scale
        fluidRow(
          column(12, align = "center", checkboxInput(
              inputId      = "logscale2"
            , label        = "Log Scale"
            , value        = FALSE
          ))
          ),

        # Add a timeline for the cases by country
        fluidRow(
          column(12, plotOutput("countryPlot"))
        )
      )
    )
  )
)

############################################################
#### Server
############################################################
server <- function(input, output){
  
  # Subset data according to user input and return reactive data
  circles <- reactive({
    
    # Subset to the desired data
    circles <- subset(dat
      , Date == input$selectedDate
      & Case == input$selectedCase
      & ConfirmedCases > 0
    )

    # Identify a reasonable circle width
    circles$Width <- log(circles$ConfirmedCases) * 50000

    # Get some additional information
    information <- dat@data %>%
      subset(., Date == input$selectedDate) %>%
      group_by(., CountryRegion, Case) %>%
      summarise(., TotalCases = sum(ConfirmedCases)) %>%
      spread(., Case, TotalCases)

    # Join the information to the circles
    circles@data <- left_join(circles@data, information, by = "CountryRegion")
    
    # Add an Id column
    circles$Id <- 1:nrow(circles)

    # Remove unnecessary data
    circles@data <- select(circles@data
      , Id
      , Width
      , CountryRegion
      , Infections
      , Deaths
      , Recoveries
    )
    
    # Return the circles
    return(circles)
  })

  # Render a baseplot
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas
        , group = "Esri World Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery
        , group = "Esri World Imagery") %>%
      addProviderTiles(providers$OpenStreetMap
        , group = "Open Street Maps") %>%
      addProviderTiles(providers$Stamen.Toner
        , group = "Stamen Toner") %>%
      addLayersControl(baseGroups = c(
          "Esri World Grey"
        , "Esri World Imagery"
        , "Open Street Maps"
        , "Stamen Toner"), position = "topleft"
      ) %>%
      fitBounds(
          lng1 = xmin(extent(dat))
        , lng2 = xmax(extent(dat))
        , lat1 = ymin(extent(dat))
        , lat2 = ymax(extent(dat))
      )
  })
  
  # Add shapes, depending on user input
  observe({

    # Depending on the selected case the color changes
    if (input$selectedCase == "Deaths"){
      color <- "darkred"
    } else if (input$selectedCase == "Infections"){
      color <- "blue"
    } else {
      color <- "darkgreen"
    }

    # Add the circles to the plot
    leafletProxy("map", data = circles()) %>%
      clearShapes() %>%
      addCircles(
          data        = circles()
        , layerId     = as.character(circles()$Id)
        , weight      = 2
        , color       = color
        , fillColor   = color
        , fillOpacity = 0.5
        , radius      = ~Width
        , popup       = ~paste(
            "<b>"
          , CountryRegion
          , "</b>"
          , "<br>"
          , "Infections: "
          , Infections
          , "<br>"
          , "Deaths: "
          , Deaths
          , "<br>"
          , "Recoveries: "
          , Recoveries
        )
      )
  })
  
  # Keep track of previously selected circles (create a new reactive value)
  prev_s <- reactiveVal()
  
  # Highlight selected circles
  observeEvent(input$confirmedCases_rows_selected, {
    
    # First we need to check which row is selected
    s <- input$confirmedCases_rows_selected
    
    # Then we subset the data accordingly
    selectedCircle <- circles()[s, ]
    
    # Create proxy
    proxy <- leafletProxy("map")
    
    # Print the selected circle
    print(selectedCircle)
    
    # Add the circle of the selected row to the plot
    proxy %>%
      addCircles(
          data        = selectedCircle
        , layerId     = as.character(selectedCircle$Id)
        , weight      = 4
        , color       = "purple"
        , opacity     = 0.8
        , fillColor   = "purple"
        , fillOpacity = 0.8
        , radius      = ~Width
        , popup       = ~paste(
            "<b>"
          , CountryRegion
          , "</b>"
          , "<br>"
          , "Infections: "
          , Infections
          , "<br>"
          , "Deaths: "
          , Deaths
          , "<br>"
          , "Recoveries: "
          , Recoveries
          )
      ) %>%
      flyTo(.
        , lng = coordinates(selectedCircle)[, "Long"] %>% as.vector()
        , lat = coordinates(selectedCircle)[, "Lat"] %>% as.vector()
        , zoom = 3
      )
    
    # If there were previous selected markers, remove them
    if (!is.null(prev_s())){
      proxy %>%
        removeShape(layerId = as.character(prev_s()$Id))
    }
    
    # Update the previous selection holder
    prev_s(selectedCircle)
  })
  
  # When animating the slider the depicted data table will refresh every
  # iteration and cause a flickering. To prevent this, we include a debounce
  # Which will prevent the data from updating until the animation settled. Since
  # The animation speed is set to 400ms, a delay of 500ms will suffice to
  # prevent flickering.
  circles_debounced <- circles %>% debounce(., 500)

  # Render the data by country
  output$confirmedCases <- DT::renderDataTable({
    circles_debounced()@data %>%
      select(., -c(Id, Width)) %>%
      datatable(., selection = "single", options = list(stateSave = TRUE))
  }, digits = 0)
  
  # Plot the time series data worldwide
  output$worldwidePlot <- renderPlot({
    p <-dat@data %>%
      group_by(., Date, Case) %>%
      summarise(TotalCases = sum(ConfirmedCases)) %>%
      mutate(Case = factor(
        Case, levels = c("Infections", "Deaths", "Recoveries")
      )) %>%
      ggplot(data = ., aes(x = Date, y = TotalCases, colour = Case)) +
        geom_line(size = 1)  +
        geom_point(size = 3)  +
        scale_color_manual(values = c("blue", "red", "green")) +
        ylab("Total Cases") +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "grey14")
          , panel.grid.minor = element_line(colour = "grey10")
          , panel.grid.major = element_line(colour = "grey10")
          )
    if (input$logscale1){
      p <- p + scale_y_log10() + ylab("log(Total Cases)")
      p
    } else {
      p
    }
  })

  # Plot the time series data by country
  output$countryPlot <- renderPlot({
    p <- dat@data %>%
      group_by(., Date, Country, Case) %>%
      summarise(TotalCases = sum(ConfirmedCases)) %>%
      mutate(Case = factor(
        Case, levels = c("Infections", "Deaths", "Recoveries")
      )) %>%
      subset(Country == input$selectedCountry) %>%
      ggplot(data = ., aes(x = Date, y = TotalCases, colour = Case)) +
        geom_line(size = 1)  +
        geom_point(size = 3)  +
        scale_color_manual(values = c("blue", "red", "green")) +
        ylab("Total Cases") +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "grey14")
          , panel.grid.minor = element_line(colour = "grey10")
          , panel.grid.major = element_line(colour = "grey10")
          )
    if (input$logscale2){
      p <- p + scale_y_log10() + ylab("log(Total Cases)")
      p
    } else {
      p
    }
  })
}

############################################################
#### Run App
############################################################
shinyApp(ui = ui, server = server)
