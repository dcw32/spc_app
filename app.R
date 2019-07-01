# Global
library(shiny)
library(markdown)
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
#
data_available <- readr::read_csv("data/data.csv")
latest <- data_available[nrow(data_available),]

dates <- data_available$Date_Time


#
ui <- fluidPage(includeCSS("www/dwade.min.css"),
  navbarPage("SPC Data",
    tabPanel("View Forecasts",
      sidebarLayout(
        sidebarPanel(
          shiny::dateInput(inputId = "calendar",label = "Calendar"),
          selectInput("forecast","Forecast",choices = "")
        ),
        mainPanel(
          leafletOutput("summary_map")
        )
      )
    ),
    tabPanel("Upload Exposures",
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      )
    ),
    tabPanel("Create Reports",
      sidebarLayout(
        sidebarPanel(),
        mainPanel(
          
        )
      )
    ),
    navbarMenu("More",
      tabPanel("Old Data",
        DT::dataTableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  # Combine the selected variables into a new data frame
  observe({
    input$calendar
    lubridate::as_date(dates) %in% lubridate::date(input$calendar)
    opts <- dates[lubridate::as_date(dates) %in% lubridate::date(input$calendar)]
    if (length(opts)>0){
      updateSelectInput(session,"forecast", choices = opts)
    } else {
      updateSelectInput(session,"forecast", choices = "")
    }
  })
  
  observe({
    input$forecast
    isolate({
      x <- data_available[data_available$Date_Time==as_datetime(input$forecast),]
      if(nrow(x)>1){
        x <- x[1,]
      }
      if(!is.na(x$Folder)){
        folder <- paste0(x$Folder,"/")
        
        otlk <- st_read(paste0(folder,"day1otlk_cat.shp"))
        otlk_prj <- st_transform(otlk, 4326)
        
        wind <- st_read(paste0(folder,"day1otlk_wind.shp"))
        wind_prj <- st_transform(wind, 4326)
        
        hail <- st_read(paste0(folder,"day1otlk_hail.shp"))
        hail_prj <- st_transform(hail, 4326)
        
        torn <- st_read(paste0(folder,"day1otlk_torn.shp"))
        torn_prj <- st_transform(torn, 4326)
        
        
        otlk_map <- data.frame(
          `DN` = 1:5,
          `Description` = c("Marginal","Slight","Enhanced","Moderate","Severe"),
          stringsAsFactors = F
        )
        torn_map <- data.frame(
          `DN` = c(2,5,10,15,30,45,60),
          `Description` = c("2%","5%","10%","15%","30%","45%","60%"),
          stringsAsFactors = F
        )
        hail_map <- data.frame(
          `DN` = c(5,15,30,45,60),
          `Description` = c("5%","15%","30%","45%","60%"),
          stringsAsFactors = F
        )
        wind_map <- data.frame(
          `DN` = c(5,15,30,45,60),
          `Description` = c("5%","15%","30%","45%","60%"),
          stringsAsFactors = F
        )
        
        
        bins_otlk <- colorFactor("YlOrRd", domain = otlk_map$DN)
        bins_wind <- colorFactor("Blues", domain = wind_map$DN)
        bins_torn <- colorFactor("Greens", domain = torn_map$DN)
        bins_hail <- colorFactor("BuPu", domain = hail_map$DN)
        
        output$summary_map <- renderLeaflet(leaflet() %>%
                                              addTiles() %>%
                                              addPolygons(data=otlk_prj,
                                                          fillColor= ~bins_otlk(otlk_prj$DN),
                                                          weight = 0,fillOpacity = 0.5,
                                                          group = "otlk"
                                              ) %>%
                                              addPolygons(data=wind_prj,
                                                          fillColor= ~bins_wind(wind_prj$DN),
                                                          weight = 0,fillOpacity = 0.5,
                                                          group = "wind"
                                              ) %>%
                                              addPolygons(data=torn_prj,
                                                          fillColor= ~bins_torn(torn_prj$DN),
                                                          weight = 0,fillOpacity = 0.5,
                                                          group = "torn"
                                              ) %>%
                                              addPolygons(data=hail_prj,
                                                          fillColor= ~bins_hail(hail_prj$DN),
                                                          weight = 0,fillOpacity = 0.5,
                                                          group = "hail"
                                              ) %>%
                                              leaflet::addLayersControl(
                                                overlayGroups = c("otlk","hail","torn","wind")
                                              ) %>%
                                              addLegend(group = "otlk",
                                                        title = "Convective Outlook",
                                                        colors= bins_otlk(otlk_map$DN),
                                                        labels = otlk_map$Description,
                                                        labFormat = labelFormat()
                                              ) %>%
                                              addLegend(group = "torn",
                                                        title = "Tornado Risk",
                                                        colors= bins_torn(torn_map$DN),
                                                        labels = torn_map$Description,
                                                        values= torn_map$DN
                                              ) %>%
                                              addLegend(group = "wind",
                                                        title = "Wind Risk",
                                                        colors= bins_wind(wind_map$DN),
                                                        labels = wind_map$Description,
                                                        values= wind_map$DN
                                              ) %>%
                                              addLegend(group = "hail",
                                                        title = "Hail Risk",
                                                        colors= bins_hail(hail_map$DN),
                                                        labels = hail_map$Description,
                                                        values= hail_map$DN
                                              ) %>%
                                              
                                              leaflet::hideGroup(c("hail","torn","wind"))
                                            
        )
        
      } else {
        output$summary_map <- renderLeaflet(leaflet() %>%
                                              addTiles)
      }
      
    })
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
  filedata <- reactive({
    df <- read_csv(input$file1$datapath)
    return(df)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    df <- filedata()
    print("DONE")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
