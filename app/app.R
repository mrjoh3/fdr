
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

library(tidyRSS)
library(rvest)
library(dplyr)
library(tidyr)
library(sf)
library(lwgeom)
library(units)
library(knitr)
library(lubridate)
library(stringr)
library(glue)
library(bomrang)
library(ggplot2)
library(ggdark)
library(colortools)
library(emojifont)
library(c3)
library(leaflet)


source('utils.R')
towns <- readRDS('towns.rds')
print('start')
TOWN_DEFAULT <- 'st-andrews'
APP_TITLE <- 'Fire Weather'

cfa__lu <- gsub(' ', '', tolower(unique(towns$cfa_tfb))) %>% as.list() %>%
  setNames(unique(towns$cfa_tfb))
town_lu <- towns$town_val %>% as.list() %>%
  setNames(towns$town_name)

# get current situation
statewide <- st_read('https://www.emergency.vic.gov.au/public/osom-geojson.json', stringsAsFactors = FALSE) %>% 
  st_transform(3111) %>%
  mutate(sizeFmt = as.character(sizeFmt),
         sizeFmt = ifelse(sizeFmt == 'character(0)', '', sizeFmt)) %>%
  filter(!is.na(st_is_valid(.))) %>%
  st_make_valid() %>%
  select(sourceId,
         feedType,
         sourceTitle,
         cap,
         category1, 
         category2,
         status,
         location,
         incidentFeatures,
         webHeadline,
         url,
         resources, 
         sizeFmt)

fontawesomeDep <- htmltools::htmlDependency("fontawesome", "5.9.0",
                                          src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.9.0/"),
                                          script = "js/fontawesome.min.js", stylesheet = "css/fontawesome.min.css"
)
# fontawesomeDep <- htmltools::htmlDependency("fontawesome", "5.11.2",
#                                             src = c(href = "www/"),
#                                             script = "js/fontawesome.js", stylesheet = "css/fontawesome.css"
# )

#precis_w <- get_precis_forecast('VIC')

ui <- shinyUI(fluidPage(
  title = APP_TITLE,
  #responsive = TRUE,
  theme = shinytheme("superhero"),
  header = NULL,
  useShinydashboard(),
  mobileDetect('isMobile'),
  tags$div(style = 'text-align: center;',
    h1(APP_TITLE, icon('fire', class = 'orange')),
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
  div(id = 'pagewrap', # does not appear to work for now
    fluidRow( 
      uiOutput('days'), tags$hr()
    ),
    fluidRow(
      column(7,
             # https://emergencyprepare.com.au/wp-content/uploads/2018/10/fire-danger-preview-2.png
             tags$img(src = 'fire-danger-preview-2.png', height = '160px', class = 'center')
      ),
      column(5,
             div(class = 'center', style = 'padding-top: 30px;',
                 selectInput('location', 'Nearest Town', selectize = T, width = '100%',
                             choices = town_lu, 
                             selected = TOWN_DEFAULT
                             )
                 )
             )
    ),
    fluidRow(div(
      style = 'padding: 10px;',
      tags$hr(),
      h3("Daily Variation"),
      plotOutput('daily', height = 100)
    )),
    fluidRow(
        uiOutput('fdr_images')
      ),
    fluidRow(
      div(style = 'padding: 30px;',
          tags$hr(),
          h3(textOutput('current_title')),
          sliderInput('buffer', label = 'Distance', value = 20, min = 10, max = 50, step = 10, width = '100%'),
          uiOutput('current_incidents'))
    ),
    fluidRow(
      div(style = 'padding: 30px;',
             tags$hr(),
             uiOutput('sources')
             )
      #textOutput('isItMobile'),
      #c3Output('test')
    ),
    fluidRow(
      div(style = 'padding: 30px;',
          tags$hr(),
          includeMarkdown('about.md')
      )
    ),
    fluidRow(
      div(style = 'padding: 30px;',
        tags$hr(),
        box(solidHeader = TRUE, background = 'black', width = 12,
          h3('Warning'),
          p('This application ', tags$b('DOES NOT'), 
            ' replace the official ',
            tags$a(href = 'https://www.cfa.vic.gov.au/warnings-restrictions/total-fire-bans-and-ratings', 'CFA'),
            ' and ',
            tags$a(href = 'https://www.emergency.vic.gov.au/respond/', 'VIC Emergency'),
            ' websites. These sites should remain your primary sources of information. ',
            'This application merely gives a rapid overview of current information.')
        ),
        tags$hr()
      )
    )
  )
))

server <- function(input, output, session) {
  
  # check for mobile device
  # output$isItMobile <- renderText({
  #   ifelse(input$isMobile, "You are on a mobile device", "You are not on a mobile device")
  # })
  observe({
    if (!(input$isMobile)) {
      includeCSS('www/css/page_style.css')
    }
  })

  dat <- reactiveValues(df = data.frame(),
                        wind = data.frame(),
                        fc = data.frame(),
                        furl = as.character(),
                        wurl = as.character(),
                        surl = as.character(),
                        load = TRUE)
  
  location <- eventReactive(input$location, {    # check for url parameters
      
      print('location(1)')
      
      query <- parseQueryString(session$clientData$url_search)
      
      if (!is.null(query[['location']]) & dat$load) {
        loc <- query[['location']]
        isolate(
        updateSelectInput(session, "location",
                          choices = town_lu,
                          selected = query[['location']])  # might cause run twice TEST
        )
        print('into if')
        isolate({dat$load <- FALSE})
      } else {
        loc = input$location
      }
      
      return(loc)
      
    }, ignoreNULL = FALSE)
  
  
  # set cfa region
  observe({
    
    print('cfa_region(1)')
    
    cfa_region <- filter(towns, town_val == location()) %>% 
      pull(cfa_tfb) %>%
      tolower() %>%
      gsub(' ', '', .)
    isolate({dat$cfa = cfa_region})
    
  }, priority = 1)
  
  # set subtitle content
  observe({
    
    print('subtitle(3)')
    
    output$subtitle <- renderText({
      #print(location())
      glue('{tools::toTitleCase(gsub("-"," ", location()))} is in the {tools::toTitleCase(dat$cfa)} Total Fire Ban District')
    })

  }, priority = 3)
  
  # watch button to update qry and get new location
  # observeEvent(input$location, {
  #   print(4)
  #   isolate({location() = input$location})
  # }, ignoreInit = TRUE, priority = 3)
  

  
  # get data 
  observe({
    
    print('data(0)')
    
    withProgress(message = 'Loading', value = 0.1, {
      
      # define and check urls exist
      wurl <- glue('http://www.bom.gov.au/places/vic/{location()}/forecast/detailed/') # 3hr detailed forecast
      surl <- glue('http://www.bom.gov.au/places/vic/{location()}/forecast/') # simple forecast
      furl <- glue('https://www.cfa.vic.gov.au/documents/50956/50964/{dat$cfa}-firedistrict_rss.xml') # cfa tfb district forecast
      
      # test location available
      if (RCurl::url.exists(wurl)) { #  & RCurl::url.exists(furl)
        
        incProgress(.30, 'Precis Weather Forecast')
        
        # get BOM forecast data
        fc <- get_precis(surl)
        
        # get latest weather
        latest <- get_latest(fc$obs_url[1])
        
        incProgress(.30, 'CFA Fire Danger')
        
        # cfa fire danger ratings forecast
        df <- get_fdr(furl)
        
        # merge fdr and forecast (TODO: change to full_join as in non fdp fdr only for current day, then need to fill required missing fields)
        df <- left_join(df, fc, by = 'date')
        
        incProgress(.30, '3hr Weather Forecast')
        
        wind <- get_detailed(wurl)
        
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
        print(location())
        shinyalert(title = 'Error', 
                   text = 'The entered location could not be found',
                   type = 'error'
        )
        
      }
      
      
    })
      
    }, priority = 0)

  
  # render charts and panels
  observe({
    
    print('charts(-1)')
    
    withProgress(message = 'Rendering', value = 0.1, {
      
      
      incProgress(.30, '3hr Weather plotting')
      
      # add c3 elements need 4 plots per row
      Map(function(n){
        label <- dat$df[['date']][n]
        cats <- unique(dat$wind$meas)
        c3_grid_server(input, output, session, dat$wind, label, location(), cats)
      }, 1:nrow(dat$df))
      
      incProgress(.50, 'Render Charts')
      
      # render UI for dropdown panels
      render_days <- lapply(1:nrow(dat$df), function(n){
        r <- dat$df[n,]
        pbox <- box(
          #title = r$tfb,
          width = 12,
          solidHeader = TRUE,
          background = r$color, 
          fluidRow(
            tags$head(tags$style(HTML('.info-box {min-height: 45px; filter: brightness(0.95);} ',
                                      '.info-box-icon {height: 45px; line-height: 45px; width: 50px; padding-bottom: 10px; font-size: 15px;} ',
                                      '.info-box-icon i {font-size: 30px; padding-top: 7px;} ',
                                      '.info-box-content {padding-top: 0px; padding-bottom: 0px; margin-left: 50px;} ',
                                      '.info-box-content span {font-size: 14px; padding-top: 2px; filter: brightness(2);}'))),
            column(
              width = 6,
              fluidRow(column(6,
                              infoBox(
                                title = 'Rain', 
                                value = ifelse(all(!is.na(c(r$lower_precipitation_limit, r$upper_precipitation_limit))), 
                                               glue('{r$lower_precipitation_limit} - {r$upper_precipitation_limit} mm ({r$probability_of_precipitation})'), 
                                               ifelse(all(is.na(c(r$lower_precipitation_limit, r$upper_precipitation_limit))), 
                                                      glue('0 mm ({r$probability_of_precipitation})'), 
                                                      ifelse(is.na(r$lower_precipitation_limit) & !is.na(r$upper_precipitation_limit), 
                                                             glue('max {r$upper_precipitation_limit} mm ({r$probability_of_precipitation})'), 
                                                             ' - '
                                                      )
                                               )
                                ),
                                color = r$color,
                                icon = icon('burn'),
                                width = 12,
                                fill = TRUE 
                              ),
                              infoBox(
                                title = "Temperature", 
                                #value = glue('{r$minimum_temperature} - {r$maximum_temperature} °C'),
                                value = ifelse(all(!is.na(c(r$minimum_temperature, r$maximum_temperature))), 
                                               glue('{r$minimum_temperature} - {r$maximum_temperature} °C'), 
                                               ifelse(is.na(r$minimum_temperature) & !is.na(r$maximum_temperature), 
                                                      glue('max {r$maximum_temperature} °C'), 
                                                      ' - '
                                               )
                                ),
                                color = r$color,
                                icon = icon('thermometer-half'),
                                width = 12,
                                fill = TRUE 
                              ),
                              infoBox(
                                title = "Fuel Dryness", 
                                value = ifelse(r$forest_min == r$forest_max, 
                                               as.character(r$forest_max), 
                                               glue('{r$forest_min} - {r$forest_max}')),
                                color = r$color,
                                icon = icon('tree'),
                                width = 12,
                                fill = TRUE 
                              )),
                       column(6,
                              infoBox(
                                title = 'Wind', 
                                value = glue('{r$wind_min} - {r$wind_max} km/h'),
                                color = r$color,
                                icon = icon('flag'),
                                width = 12,
                                fill = TRUE 
                              ),
                              infoBox(
                                title = "Humidity", 
                                value = glue('{r$relative_min} - {r$relative_max} %'),
                                color = r$color,
                                icon = icon('cloud'),
                                width = 12,
                                fill = TRUE 
                              ),
                              infoBox(
                                title = "UV", 
                                value = ifelse(is.na(r$uv), ' - ', as.character(r$uv)),
                                color = r$color,
                                icon = icon('sun'),
                                width = 12,
                                fill = TRUE 
                              ))
              ),
              tags$div(tags$img(style = 'float: left; padding: 20px;',
                                src = r$image, alt = 'weather icon'),
                       tags$p(r$precis)
                       )
              #h1(r$item_title)
              #HTML(r$item_description)
            ),
            column(
              width = 6,
              #plotOutput(glue('plot{n}'))
              c3_grid_UI(r$date, location(), unique(dat$wind$meas))
            ))
        )
        
        tags$div(class=glue("{str_replace_all(tolower(r$title), ' ', '-')} panel panel-default"),
                 tags$div(class=glue("{str_replace_all(tolower(r$title), ' ', '-')} panel-heading"), role="tab", id=glue("heading{n}"), #header div
                          #style = glue('background-color: {r$fdr_color};'),
                          tags$h5(class=glue("panel-title {str_replace_all(tolower(r$title), ' ', '-')}"),
                                  #style=glue("color: {sequential(splitComp(r$fdr_color, plot = FALSE)[2], plot = FALSE, percentage = 10, what = 'value')[6]};"),
                                  tags$a(role="button", `data-toggle`="collapse", `data-parent`="#accordion", href=glue("#collapse{n}"), `aria-expanded`="false", `aria-controls`=glue("collapse{n}"),
                                         fluidRow(
                                           column(6, r$item_title),
                                           column(3, r$tfb, if (r$tfb != '') {icon('ban')} else {''}),
                                           column(3, 
                                                  div(style = ifelse(input$isMobile,
                                                                     'text-align: right; margin: 0; position: relative; top: -19px;',
                                                                     'text-align: right; margin: 0;'),
                                                      tags$b(r$title)
                                                  )
                                           )
                                         )
                                  )
                          )), 
                 tags$div(id=glue("collapse{n}"), class="panel-collapse collapse", role="tabpanel", `aria-labelledby`=glue("heading{n}"), #content div
                          tags$div(class="panel-body",
                                   pbox,
                                   tags$a(href = r$item_link, 'view on CFA page')
                          ))  
        )
      })
      
      output$days <- shiny::renderUI({
        tagList(tags$div(class="panel-group", id="accordion", role="tablist", `aria-multiselectable`="true",
                         render_days))
      })
      
      output$daily <- renderPlot({
        calc_fdi(dat$wind, mobile = input$isMobile)
      }, height = 100, bg = 'transparent')
      
      output$fdr_images <- renderUI({
        fdr_images(input$isMobile)
      })
      
      incProgress(.05, 'Current Situation')
      
      output$current_incidents <- renderUI({
        
        render_current(statewide, towns, location(), buffer = input$buffer)
        
      })
      
      output$current_title <- renderText({
        glue('Current Situation {input$buffer} km')
      })
      
      incProgress(.05, 'Data Sources')
      
      output$sources <- renderUI({
        
        tagList(
          h3('Data Sources'),
          p('All data is sourced when you first load the page via a combination of RSS feeds and webscraping. ',
            'The links here are the reliable sources, this page merely groups useful information from these sources.'),
          
          h4('Vic Emergency'),
          h5(tags$a(href = 'http://emergency.vic.gov.au/respond/', # explore adding bounding box later for zoom to area
                    'Warnings, Incidents and Burnt Area')),
          
          h4('CFA'),
          h5(tags$a(href = glue('https://www.cfa.vic.gov.au/warnings-restrictions/{dat$cfa}-fire-district'),
                    'Fire Danger Ratings')),
          h5(tags$a(href = dat$furl, 'Fire Danger Ratings (RSS)')),
          
          h4('BOM'),
          h5(tags$a(href = dat$surl, 'Extended Forecast (7 day)')),
          h5(tags$a(href = dat$wurl, 'Detailed 3-hourly Forecast')),
          h5(tags$a(href = dat$fc$obs_url[1], 'Current and Past Weather'))
          
        )
        
      })
      
      
    })
    
    
  }, priority = -1)
  

}




shinyApp(ui = ui, server = server)

