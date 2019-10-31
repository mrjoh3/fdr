
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

library(tidyRSS)
library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(glue)
library(bomrang)

ui <- shinyUI(fluidPage(
  title = 'CFA FDR and Weather for Central Region',
  responsive = TRUE,
  theme = shinytheme("superhero"),
  header = NULL,
  useShinydashboard(),
  h1('CFA FDR and Weather for Central Region'),
  fluidRow( 
    uiOutput('days')
  ),
  fluidRow(
    h3('Fire Danger Ratings'),
    column(6,
           h5('Forest Fire'),
           tags$img(src = 'http://www.bom.gov.au/fwo/IDV65406.png', width = '95%')),
    column(6,
           h5('Grass Fire'),
           tags$img(src = 'http://www.bom.gov.au/fwo/IDV65426.png', width = '95%'))
  )
))

server <- function(input, output, session) {
  
  url <- 'https://www.cfa.vic.gov.au/documents/50956/50964/central-firedistrict_rss.xml'
  
  df <- tidyfeed(url) %>%
    mutate(date = as.Date(gsub('Today, |Tomorrow, ', '', item_title), '%a, %d %b %Y'),
           title = str_extract(item_description, 'LOW-MODERATE|HIGH|VERY HIGH|SEVERE|EXTREME|CODE RED'),
           start = date,
           end = date + days(1),
           day = wday(start, label = TRUE),
           week = isoweek(start),
           rendering = 'background',
           color = case_when(
             title == 'CODE RED' ~ '#710d08', # should be same as extreme but with black cross hatch
             title == 'EXTREME' ~ 'red', #ee2e24',
             title == 'SEVERE' ~ 'orange', #f89829',
             title == 'VERY HIGH' ~ 'yellow', #fff002',
             title == 'HIGH' ~ 'blue', #00adef',
             title == 'LOW-MODERATE' ~ 'green' #79c141'
           )) %>%
    filter(!is.na(date)) %>%
    distinct()
  
  
  # get BOM data
  #wth <- get_current_weather('MELBOURNE AIRPORT')
  fc <- get_precis_forecast('VIC') %>%
    filter(town == 'Melbourne',
           !is.na(minimum_temperature)) %>%
    mutate(date = as.Date(start_time_local)) %>%
    select(date, minimum_temperature:probability_of_precipitation)
  
  # merge fdr and forecast
  df <- left_join(df, fc, by = 'date')
  
  # wind
  wurl <- 'http://www.bom.gov.au/places/vic/st-andrews/forecast/detailed/'
  wind <- read_html(wurl) %>%
    html_table() %>%
    .[seq(5, length(.), 5)]%>%
    setNames(seq(Sys.Date(), length.out = length(.), by = "1 days")) %>%
    bind_rows(.id = 'date') %>% 
    gather('time', 'value', -date, -At)
  
  render_days <- lapply(1:nrow(df), function(n){
    r <- df[n,]
    box(
      title = r$item_title,
      width = 12,
      solidHeader = TRUE,
      background = r$color, 
      fluidRow(
        column(
          width = 6,
          h2(r$title),
          tags$a(href = r$item_link, 'view on CFA page')
          #h1(r$item_title)
          #HTML(r$item_description)
        ),
        column(
          width = 6,
          infoBox(
            title = r$precis, 
            value = glue('{r$probability_of_precipitation}% for {r$lower_precipitation_limit} - {r$upper_precipitation_limit} mm'),
            #color = r$color,
            icon = icon('cloud'),
            width = 12,
            fill = TRUE 
          ),
          infoBox(
            title = "Temperature", 
            value = glue('{r$minimum_temperature} - {r$maximum_temperature} degrees C'),
            #color = r$color,
            icon = icon('thermometer-half'),
            width = 12,
            fill = TRUE 
          )))
    )
  })
  
  output$days <- shiny::renderUI({
    tagList(render_days)
  })
  
}




shinyApp(ui = ui, server = server)

