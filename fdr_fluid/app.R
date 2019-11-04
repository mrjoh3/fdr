
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
library(ggplot2)
library(emojifont)
library(c3)


source('utils.R')




ui <- shinyUI(fluidPage(
  title = 'CFA FDR and Weather for Central Region',
  responsive = TRUE,
  theme = shinytheme("superhero"),
  header = NULL,
  useShinydashboard(),
  mobileDetect('isMobile'),
  tags$div(style = 'text-align: center;',
    h1('CFA Fire Danger Rating and Weather'),
    h5('for Central Region')
    ),
  includeCSS('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'),
  includeCSS('www/css/styles.css'),
  tags$style(HTML(".c3-chart-text .c3-text {
                      font-family: 'FontAwesome';
                      font-size: 22px;
                    }
                    .c3-target-value text {
                       fill: orange !important;
                    }")),
  fluidRow( 
    uiOutput('days')
  ),
  fluidRow(
    div(style = 'padding: 30px;',
    h3('Fire Danger Ratings'),
    tabsetPanel(
      tabPanel(h5('Forest'),
               tags$img(src = 'http://www.bom.gov.au/fwo/IDV65406.png', width = '95%')),
      tabPanel(h5('Grassland'),
               tags$img(src = 'http://www.bom.gov.au/fwo/IDV65426.png', width = '95%'))
    )
  )),
  fluidRow(
    textOutput('isItMobile'),
    c3Output('test')
  )
))

server <- function(input, output, session) {
  
  # check for mobile device
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "You are on a mobile device", "You are not on a mobile device")
  })
  
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
    #.[seq(5, length(.), 5)]%>%
    #setNames(seq(Sys.Date(), length.out = length(.), by = "1 days")) %>%
    lapply(., function(df){
      rename_at(df, vars(1), ~ sub('At|From','meas', .)) %>%
      mutate_all(as.character)
    }) %>%
    setNames(
      sort(rep(seq(Sys.Date(), length.out = length(.) / 5, by = "1 days"), 5))
      ) %>%
    bind_rows(.id = 'date') %>% 
    gather('time', 'value', -date, -meas) %>%
    filter(meas %in% c('Relative humidity (%)',
                       'Air temperature (°C)',
                       'Wind speed  km/hknots',
                       'Forest fuel dryness factor',
                       'Thunderstorms',
                       'Rain',
                       'Wind direction')) %>%
    spread(meas, value) %>%
    gather('meas', 'value', -date, -time, -Thunderstorms, -Rain, -`Wind direction`) %>%
    # pivot_wide for all pivot_long for numeric
    mutate(date = as.Date(date),
           time = lubridate::ymd_hm(glue('{date} {time}', tz = 'AET')),
           value = ifelse(value == '–', NA, value),
           value = ifelse(!is.na(value) & meas == 'Wind speed  km/hknots', 
                          substr(value, 1, ceiling(nchar(value)/2)), # need to check this works for higher wind speeds
                          value),
           meas = gsub('knots', '', meas),
           `Wind direction` = ifelse(meas != 'Wind speed  km/h', NA, `Wind direction`),
           Thunderstorms = ifelse(meas == 'Air temperature (°C)' & Thunderstorms == 'Yes', 'fa-bolt', NA),
           Rain = ifelse(meas == 'Relative humidity (%)' & Rain == 'Yes', 'fa-tint', NA),
           y_max = case_when(
             meas == 'Relative humidity (%)' ~ 100,
             meas == 'Air temperature (°C)' ~ 50,
             meas == 'Wind speed  km/h' ~ 60,
             meas == 'Forest fuel dryness factor' ~ 10
           ))
  
  # add ggplot elements uses facet_wrap 
  # Map(function(n){
  #   output[[glue('plot{n}')]] <- renderPlot({
  #     d <- df[['date']][n]
  #     wind %>%
  #       filter(date == d,
  #              meas %in% c('Relative humidity (%)',
  #                          'Air temperature (°C)',
  #                          'Wind speed  km/h',
  #                          'Forest fuel dryness factor')) %>%
  #       mutate(value = as.numeric(value)) %>%
  #       ggplot(aes(x = time, y = as.numeric(value))) +
  #       geom_line() +
  #       geom_text(aes(label = `Wind direction`), #color = 'red'), 
  #                 vjust = 0, nudge_y = 3, size = 3) +
  #       geom_text(aes(label = fontawesome(Thunderstorms)), 
  #                 family = 'fontawesome-webfont', size = 4,
  #                 vjust = 0, nudge_y = 3) +
  #       geom_text(aes(label = fontawesome(Rain)), 
  #                 family = 'fontawesome-webfont', size = 4,
  #                 vjust = 0, nudge_y = 4) +
  #       facet_wrap(~ meas, 
  #                  ncol = ifelse(input$isMobile, 1, 2), 
  #                  scales = 'free_y') +
  #       geom_blank(aes(y = 0)) +
  #       geom_blank(aes(y = y_max)) +
  #       scale_x_datetime(date_breaks = '8 hour', date_labels = "%H:%M") +
  #       theme_minimal() +
  #       theme(
  #         panel.background = element_rect(fill = "transparent", colour = NA),
  #         plot.background = element_rect(fill = "transparent", colour = NA),
  #         panel.grid.minor.x=element_blank(),
  #         panel.grid.major.x=element_blank(),
  #         panel.grid.minor.y=element_blank()
  #       ) +
  #       labs(x = '', y = '')
  #   }, height = 350, bg = "transparent")
  # }, 1:nrow(df))
  
  # add c3 elements need 4 plots per row
  Map(function(n){
    label <- df[['date']][n]
    cats <- unique(wind$meas)
    c3_grid_server(input, output, session, wind, label, cats)
  }, 1:nrow(df))
  
  
  render_days <- lapply(1:nrow(df), function(n){
    r <- df[n,]
    pbox <- box(
      title = tags$a(href = r$item_link, 'view on CFA page'),
      width = 12,
      solidHeader = TRUE,
      background = r$color, 
      fluidRow(
        column(
          width = 6,
          h2(r$title),
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
          )
          #h1(r$item_title)
          #HTML(r$item_description)
        ),
        column(
          width = 6,
          #plotOutput(glue('plot{n}'))
          c3_grid_UI(r$date, unique(wind$meas))
          ))
    )
    
    tags$div(class="panel panel-default",
             tags$div(class="panel-heading", role="tab", id=glue("heading{n}"), #header div
                      style = glue('background-color: {r$color};'),
                      tags$h4(class="panel-title",
                              tags$a(role="button", `data-toggle`="collapse", `data-parent`="#accordion", href=glue("#collapse{n}"), `aria-expanded`="false", `aria-controls`=glue("collapse{n}"),
                                     fluidRow(
                                       column(9, r$item_title),
                                       column(3, r$title)
                                       )
                                     )
                              )), 
             tags$div(id=glue("collapse{n}"), class="panel-collapse collapse", role="tabpanel", `aria-labelledby`=glue("heading{n}"), #content div
                      tags$div(class="panel-body",
                        pbox
                      ))  
             )
  })
  
  # c3 label icon test
  output$test <- renderC3({
    #select(tmp, value, Thunderstorms) %>% c3(y = 'value', labels = list(format = list(value = htmlwidgets::JS('function(v,id,i,j){if (Thunderstorms.v === 1) return "\uf1ec"}'))))
    d <- df[['date']][1]
    c3df <- wind %>%
      filter(date == d,
             meas == 'Air temperature (°C)') %>%
      mutate(Thunderstorms = ifelse(is.na(Thunderstorms), 0 , 1),
             time = as.character(time))
    
    select(c3df, time, value) %>% 
      arrange(time) %>%
      c3(x = 'time', y = 'value', xFormat = '%Y-%m-%d %H:%M:%S', labels = list(
        format = list(
          value = htmlwidgets::JS('function(v,id,i,j){',
                                  sprintf('var a = [%s];', paste(c3df[['Thunderstorms']], collapse = ',')),
                                  'return (a[i] === 1) ? "\uf0e7" : ""}')
          )
        )) %>% 
      xAxis(type = 'timeseries') %>% 
      tickAxis('y', count = 4, format = htmlwidgets::JS('d3.format(".2n")')) %>% 
      tickAxis('x', culling = list(max = 3)) %>%
      legend(hide = TRUE)
  })
  
  output$days <- shiny::renderUI({
    tagList(tags$div(class="panel-group", id="accordion", role="tablist", `aria-multiselectable`="true",
                     render_days))
  })
  
}




shinyApp(ui = ui, server = server)

