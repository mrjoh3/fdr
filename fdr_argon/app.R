
library(shiny)
library(argonDash)
library(argonR)

library(tidyRSS)
library(dplyr)
library(lubridate)
library(stringr)
library(glue)
library(bomrang)

ui <- argonDashPage(
  header = NULL,
  body = argonDashBody(
    #setShadow(class = "dropdown-menu")
    uiOutput('days')
  ),
  sidebar = NULL,
  title = "DashboardPage"
)

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
  
  render_days <- lapply(1:nrow(df), function(n){
    r <- df[n,]
    argonCard(
      #title = r$item_title,
      width = 12,
      icon = icon('fire'),
      background_color = r$color, 
      icon_background = 'info',
      gradient = FALSE,
      #src = r$item_link,
      fluidRow(
        column(
          width = 6,
          h1(r$title),
          h1(r$item_title)
          #HTML(r$item_description)
          ),
        column(
          width = 6,
          argonInfoCard(
              title = r$precis, 
              value = glue('{r$probability_of_precipitation}% for {r$lower_precipitation_limit} - {r$upper_precipitation_limit} mm'),
              icon_background = r$color,
              icon = icon('cloud'),
              width = 12
            ),
          argonInfoCard(
              title = "Temperature", 
              value = glue('{r$minimum_temperature} - {r$maximum_temperature} degrees C'),
              icon_background = r$color,
              icon = icon('thermometer-half'),
              width = 12
            )))
    )
  })
  
  output$days <- shiny::renderUI({
    tagList(render_days)
  })
  
}
   



shinyApp(ui = ui, server = server)

