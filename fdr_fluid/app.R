
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

library(tidyRSS)
library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(glue)
library(bomrang)
library(ggplot2)
library(ggdark)
library(colortools)
library(emojifont)
library(c3)


source('utils.R')
towns <- readRDS('towns.rds')

cfa__lu <- gsub(' ', '', tolower(unique(towns$cfa_tfb))) %>% as.list() %>%
  setNames(unique(towns$cfa_tfb))
town_lu <- towns$town_val %>% as.list() %>%
  setNames(towns$town_name)

fontawesomeDep <- htmltools::htmlDependency("fontawesome", "5.1.0",
                                          src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.1.0/"),
                                          script = "js/fontawesome.js", stylesheet = "css/fontawesome.css"
)

#precis_w <- get_precis_forecast('VIC')

ui <- shinyUI(fluidPage(
  title = 'FDR and Weather',
  responsive = TRUE,
  theme = shinytheme("superhero"),
  header = NULL,
  useShinydashboard(),
  mobileDetect('isMobile'),
  tags$div(style = 'text-align: center;',
    h1('FDR and Weather', icon('fire',class = 'orange')),
    h5(textOutput('subtitle')),
    tags$hr()
    ),
  fontawesomeDep,
  useShinyalert(),
  includeCSS('www/css/styles.css'),
  tags$style(HTML(".c3-chart-text .c3-text {
                      font-family: 'FontAwesome';
                      font-size: 22px;
                    }
                    .c3-target-value text {
                       fill: orange !important;
                    }
                    .orange {
                       color: orange;
                    }
                  ")),
  fluidRow( 
    uiOutput('days'), tags$hr()
  ),
  fluidRow(
    column(4#,
           #tags$img(src = 'www/fdr-scale.gif', width = '95%')
    ),
    column(4,
           selectizeInput('location', 'Nearest Town', 
                          choices = town_lu, 
                          selected = 'st-andrews')
           ),
    column(4)
  ),
  fluidRow(
    div(style = 'padding: 30px;',
        tags$hr(),
        h3('Fire Danger Ratings'),
        tabsetPanel(
          tabPanel(h5('Forest'),
                   tags$img(src = 'http://www.bom.gov.au/fwo/IDV65406.png', width = '95%')),
          tabPanel(h5('Grassland'),
                   tags$img(src = 'http://www.bom.gov.au/fwo/IDV65426.png', width = '95%'))
        )
  )),
  fluidRow(
    column(12,
           tags$hr(),
           uiOutput('sources'),
           tags$hr()
           )
    #textOutput('isItMobile'),
    #c3Output('test')
  )
))

server <- function(input, output, session) {
  
  # check for mobile device
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "You are on a mobile device", "You are not on a mobile device")
  })

  dat <- reactiveValues(df = data.frame(),
                        wind = data.frame(),
                        fc = data.frame(),
                        furl = as.character(),
                        wurl = as.character(),
                        surl = as.character(),
                        load = TRUE)
  
  observe({    # check for url parameters
      
      print(1)
      
      location <- input$location
      query <- parseQueryString(session$clientData$url_search)
      
      if (!is.null(query[['location']]) & dat$load) {
        location <- query[['location']]
        updateSelectizeInput(session, "location", 
                             choices = town_lu, 
                             selected = query[['location']])
        print('into if')
        isolate({dat$load <- FALSE})
      }
      
      isolate({dat$location = location})
      
    }, priority = 2)
  
  observe({
    
    print(2)
    
    cfa_region <- filter(towns, town_val == dat$location) %>% 
      pull(cfa_tfb) %>%
      tolower() %>%
      gsub(' ', '', .)
    isolate({dat$cfa = cfa_region})
    
  }, priority = 1)
  
  observe({
    print(5)
    output$subtitle <- renderText({
      #print(dat$location)
      glue('{tools::toTitleCase(gsub("-"," ", dat$location))} is in the {tools::toTitleCase(dat$cfa)} Total Fire Ban District')
    })

  }, priority = 3)
  
  # watch button to update qry and get new location
  observeEvent(input$location, {
    print(4)
    isolate({dat$location = input$location})
  }, ignoreInit = TRUE, priority = 3)
  

  
  # get data 
  observe({
    
    print(3)
    
    withProgress(message = 'Loading', value = 0, {
      
      # define and check urls exist
      wurl <- glue('http://www.bom.gov.au/places/vic/{dat$location}/forecast/detailed/') # 3hr detailed forecast
      surl <- glue('http://www.bom.gov.au/places/vic/{dat$location}/forecast/') # simple forecast
      furl <- glue('https://www.cfa.vic.gov.au/documents/50956/50964/{dat$cfa}-firedistrict_rss.xml') # cfa tfb district forecast
      
      # test location available
      if (RCurl::url.exists(wurl)) { #  & RCurl::url.exists(furl)
        
        incProgress(.20, 'Precis Weather Forecast')
        
        # get BOM forecast data
        fc <- get_precis(surl)
        
        # get latest weather
        latest <- get_latest(fc$obs_url[1])
        
        incProgress(.20, 'CFA Fire Danger')
        
        # cfa fire danger ratings forecast
        df <- get_fdr(furl)
        
        # merge fdr and forecast
        df <- left_join(df, fc, by = 'date')
        
        incProgress(.20, '3hr Weather Forecast')
        
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
                 time = lubridate::ymd_hm(glue('{date} {time}', tz = 'AEST')),
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
                 ),
                 value = as.numeric(value))
        
        wind_agg <- wind %>%
          filter(meas %in% c('Relative humidity (%)',
                             'Air temperature (°C)',
                             'Wind speed  km/h',
                             'Forest fuel dryness factor')) %>%
          mutate(meas = tolower(word(meas, 1))) %>%
          group_by(date, meas) %>%
          summarise(max = max(as.numeric(value), na.rm = TRUE),
                    min = min(as.numeric(value), na.rm = TRUE)) %>% 
          pivot_longer(c(max,min)) %>%
          pivot_wider(date, c(meas, name))
        
        df <- df %>% 
          left_join(wind_agg)
        
        # add fdr colors to plotting data then bind latest observations
        wind <- wind %>%
          left_join(select(df, date, fdr_color), by = 'date') %>%
          bind_rows(latest)
        
        isolate({dat$df = df})
        isolate({dat$wind = wind})
        isolate({dat$fc = fc})
        isolate({dat$furl = furl})
        isolate({dat$wurl = wurl})
        isolate({dat$surl = surl})
        
        
        
      } else {
        
        incProgress(1, 'Something Went Wrong')
        
        print('location not found')
        print(dat$location)
        shinyalert(title = 'Error', 
                   text = 'The entered location could not be found',
                   type = 'error'
        )
        
      }
      
      
    })
      
    }, priority = 0)

  
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
  


}




shinyApp(ui = ui, server = server)

