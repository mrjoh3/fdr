
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


ui <- shinyUI(fluidPage(
  title = 'CFA FDR and Weather for Central Region',
  responsive = TRUE,
  theme = shinytheme("superhero"),
  header = NULL,
  useShinydashboard(),
  mobileDetect('isMobile'),
  tags$div(style = 'text-align: center;',
    h1('CFA Fire Danger Rating and Weather', icon('fire',class = 'orange')),
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
          # selectizeInput('cfa', 'CFA Region',
          #                choices = NULL)
    ),
    column(4,
           selectizeInput('location', 'Nearest Town', 
                          choices = town_lu, 
                          selected = 'st-andrews')
           ),
    column(4,
           style = 'padding-top: 20px;',
           shinyWidgets::actionBttn(
             'new', 'New Location', icon('crosshairs'), 
             style = 'material-flat', block = TRUE, color = 'primary'
           ))
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
    textOutput('isItMobile'),
    c3Output('test')
  )
))

server <- function(input, output, session) {
  
  # check for mobile device
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "You are on a mobile device", "You are not on a mobile device")
  })

  dat <- reactiveValues(df = data.frame(),
                        wind = data.frame())
  
  # get nearest forcast town
  # bomrang::sweep_for_forecast_towns(c(-38, 147))
  
  # observeEvent(input$location, {
  #   cfa_region <- filter(towns, town_name == input$location) %>% 
  #     pull(cfa_tfb) %>%
  #     tolower() %>% 
  #     gsub(' ', '', .)
  #   updateSelectizeInput(session, "cfa", 
  #                        choices = cfa_regions, 
  #                        selected = cfa_region[1],
  #                        server = TRUE)
  # })
  
  observe({    # check for url parameters
    
      query <- parseQueryString(session$clientData$url_search)
      
      # cfa <- input$cfa # default
      location <- input$location # default
      
      # if (!is.null(query[['cfa']])) {
      #   cfa <- query[['cfa']]
      #   updateSelectizeInput(session, "cfa", selected = query[['cfa']])
      # }
      # isolate({dat$cfa = cfa})
      
      if (!is.null(query[['location']])) {
        location <- query[['location']]
        updateSelectizeInput(session, "location", 
                             choices = town_lu, 
                             selected = query[['location']],
                             server = TRUE)
      }
      
      isolate({dat$location = location})
      
      cfa_region <- filter(towns, town_val == location) %>% 
            pull(cfa_tfb) %>%
            tolower() %>%
            gsub(' ', '', .)
      isolate({dat$cfa = cfa_region})
      
    })
  
  # watch button to update qry and get new location
  observeEvent(input$new, {
    updateQueryString(glue('?location={dat$location}'), mode = "push")
  })
  
  observe({
    output$subtitle <- renderText({
      glue('{dat$location} is in the {dat$cfa} Total Fire Ban District')
    })
  })
  
  # get data 
  observe({

    # define and check urls exist
    wurl <- glue('http://www.bom.gov.au/places/vic/{dat$location}/forecast/detailed/')
    furl <- glue('https://www.cfa.vic.gov.au/documents/50956/50964/{dat$cfa}-firedistrict_rss.xml')

    # test location available
    if (RCurl::url.exists(wurl) & RCurl::url.exists(furl)) {
      
      # get BOM forecast data
      #wth <- get_current_weather('MELBOURNE AIRPORT')
      fc <- get_precis_forecast('VIC') %>%
        filter(town == 'Melbourne',
               !is.na(minimum_temperature)) %>%
        mutate(date = as.Date(start_time_local)) %>%
        select(date, minimum_temperature:probability_of_precipitation)
      
      df <- furl %>%
        tidyfeed(.) %>%
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
      
      # merge fdr and forecast
      df <- left_join(df, fc, by = 'date')
      
      isolate({dat$df = df})
      
      
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
      
      isolate({dat$wind = wind})
      
      # add c3 elements need 4 plots per row
      Map(function(n){
        label <- dat$df[['date']][n]
        cats <- unique(dat$wind$meas)
        c3_grid_server(input, output, session, dat$wind, label, cats)
      }, 1:nrow(dat$df))
      
      # render UI for dropdown panels
        render_days <- lapply(1:nrow(dat$df), function(n){
          r <- dat$df[n,]
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
                c3_grid_UI(r$date, unique(dat$wind$meas))
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
        
        output$days <- shiny::renderUI({
          tagList(tags$div(class="panel-group", id="accordion", role="tablist", `aria-multiselectable`="true",
                           render_days))
        })
        
      
    } else {
      
      print('location not found')
      shinyalert(title = 'Error', 
                 text = 'The entered location could not be found',
                 type = 'error'
                 )
      
    }

    
  })
  
  
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

