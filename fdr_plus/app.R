
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(tidyRSS)
library(dplyr)
library(lubridate)
library(stringr)
library(glue)
library(bomrang)

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(),
  body = dashboardBody(
    #setShadow(class = "dropdown-menu")
    uiOutput('days')
  ),
  sidebar = dashboardSidebar(),
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
             title == 'EXTREME' ~ '#ee2e24',
             title == 'SEVERE' ~ '#f89829',
             title == 'VERY HIGH' ~ '#fff002',
             title == 'HIGH' ~ 'Info', #'#00c0ef', #'#00adef',
             title == 'LOW-MODERATE' ~ '#00a65a' #'#79c141'
           )) %>%
    filter(!is.na(date))
  
  
  # get BOM data
  #wth <- get_current_weather('MELBOURNE AIRPORT')
  fc <- get_precis_forecast('VIC') %>%
    filter(town == 'Melbourne') %>%
    mutate(date = as.Date(start_time_local)) %>%
    select(date, minimum_temperature:probability_of_precipitation)
  
  # merge fdr and forecast
  df <- left_join(df, fc, by = 'date')
  
  render_days <- lapply(1:nrow(df), function(n){
    r <- df[n,]
    gradientBox(
      title = r$item_title,
      width = 12,
      icon = "fa fa-fire",
      gradientColor = r$color, 
      boxToolSize = "xs", 
      closable = FALSE,
      footer = fluidRow(
        column(
          width = 6,
          boxPad(color = r$color,
            descriptionBlock(
            header = r$title#, 
            #text = HTML(r$item_description)
          ))
        ),
        column(
          width = 6,
          boxPad(
            color = "white",
            descriptionBlock(
              header = r$precis, 
              text = glue('Rainfain prob: {r$probability_of_precipitation}%'), 
              right_border = FALSE,
              margin_bottom = TRUE
            ),
            descriptionBlock(
              header = "Rainfall range", 
              text = glue("{r$lower_precipitation_limit} - {r$upper_precipitation_limit} mm"), 
              right_border = FALSE,
              margin_bottom = FALSE
            ),
            descriptionBlock(
              header = "Temperature", 
              text = glue('{r$minimum_temperature} - {r$maximum_temperature} degrees C'), 
              right_border = FALSE,
              margin_bottom = TRUE
            )
          )
        )),
      tags$a(href = r$item_link)
    )
  })
  
  output$days <- shiny::renderUI({
    tagList(render_days)
  })
  
}
   



shinyApp(ui = ui, server = server)

