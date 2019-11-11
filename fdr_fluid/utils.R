


mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


# 4 plots in a box UI (rendered inside server , renderUI)
c3_grid_UI <- function(label, cats){
  
  cats_L <- tolower(word(unique(cats), 1))
  
  tagList(
    fluidRow(column(6,
                    h5(cats[1]),
                    c3Output(glue("{cats_L[1]}{gsub('-', '', label)}", height = 80)),
                    h5(cats[2]),
                    c3Output(glue("{cats_L[2]}{gsub('-', '', label)}", height = 80))),
             column(6,
                    h5(cats[3]),
                    c3Output(glue("{cats_L[3]}{gsub('-', '', label)}", height = 80)),
                    h5(cats[4]),
                    c3Output(glue("{cats_L[4]}{gsub('-', '', label)}", height = 80)))
             )
  )
  
}




c3_grid_server <- function(input, output, session, wind, label, cats){
  
  cats_L <- tolower(word(unique(cats), 1))
  
  Map(function(pl){
    output[[glue("{cats_L[pl]}{gsub('-', '', label)}")]] <- renderC3({
      
      c3df <- wind %>%
        filter(date == label,
               meas == cats[pl]) %>%
        arrange(time) %>%
        mutate(value = as.numeric(value),
               time = as.character(time),
               Thunderstorms = ifelse(is.na(Thunderstorms), 0 , 1),
               Rain = ifelse(is.na(Rain), 0 , 1)) %>%
        fill(fdr_color, y_max, .direction = 'downup')

      if (cats[pl] != 'Forest fuel dryness factor') {
        c3d4 <- c3df %>% filter(!is.na(value))
      }
      
      labels <- list()
      
      if (cats[pl] == 'Air temperature (°C)') {
        labels <- list(
        format = list(
          value = htmlwidgets::JS('function(v,id,i,j){',
                                  sprintf('var a = [%s];', paste(c3df[['Thunderstorms']], collapse = ',')),
                                  'return (a[i] === 1) ? "\uf0e7" : ""}')))
        }
      if (cats[pl] == 'Relative humidity (%)') {
        labels <- list(
          format = list(
            value = htmlwidgets::JS('function(v,id,i,j){',
                                    sprintf('var a = [%s];', paste(c3df[['Rain']], collapse = ',')),
                                    'return (a[i] === 1) ? "\uf043" : ""}')))
      }
      
      #print(colnames(c3df))
      
      chart <- select(c3df, time, value) %>% 
        c3(x = 'time', xFormat = '%Y-%m-%d %H:%M:%S', 
           y = 'value', 
           labels = labels, height = 80) %>% 
        c3_chart_size(height = 110) %>% 
        xAxis(type = 'timeseries') %>%
        #yAxis(max = c3df$y_max[1]) %>%
        yAxis(max = max(c3df$value) * 1.2) %>% # give space for icons
        tickAxis('x', culling = list(max = 3)) %>% 
        tickAxis('y', count = 4, format = htmlwidgets::JS('d3.format(".0f")')) %>% 
        legend(hide = TRUE) %>%
        c3_colour(c(colortools::opposite(c3df$fdr_color[1], plot = FALSE)[2],
                    'black'))
      
      if (nrow(c3df) > 10) {
        chart <- chart %>%
          grid('x', 
               show = F, 
               lines = data.frame(value = as.character(c3df$now[1]), 
                                  text = 'Now'))
      }
        
      chart
    })
  }, 1:length(cats))
  
}


get_precis <- function(url){
  
  doc <- read_html(url) 
  
  days <- doc %>%
    html_nodes('.day')
  
  dates <- seq(Sys.Date(), length.out = length(days), by = 1)
  
  obs_url <- doc %>% html_nodes('.obs a') %>% html_attr('href')
  
  df <- lapply(1:length(days), function(n){

    dd <- days[[n]] %>%
      html_nodes('dd')
    
    img <- dd %>% html_node('img') %>% html_attr('src')
    area <- days[[n]] %>% html_node('h3') %>% html_text()
    precis <- days[[n]]  %>% html_node('p') %>% html_text()
    uv <- days[[n]]  %>% html_node('p.alert') %>% html_text()
    names <- dd %>% html_attr('class') 
    
    names[is.na(names)] <- 'X'
    
    dl <- dd %>% 
      html_text() %>%
      setNames(names) %>% # sometimes there are extra fields to drop
      as.list()
    
    dl[['X']] <- NULL
      
    dl %>%
      as_tibble() %>%
      mutate(image = glue('http://www.bom.gov.au/images/meteye/weather_icons/large/{basename(img[1])}'),
             area = area,
             precis = precis,
             uv = uv, 
             date = dates[n]) %>%
      separate(amt, c('lower_precipitation_limit', 'upper_precipitation_limit'), sep = ' to ', remove = TRUE) %>%
      mutate(upper_precipitation_limit = gsub(' mm', '', upper_precipitation_limit),
             uv = unlist(str_extract_all(uv, '[1-9]')) %>% .[length(.)]) %>%
      mutate_at(c('lower_precipitation_limit', 'upper_precipitation_limit', 'uv'), as.numeric) 
    
  }) %>% bind_rows() %>%
    select(date,
           minimum_temperature = min,
           maximum_temperature = max,
           lower_precipitation_limit,
           upper_precipitation_limit,
           probability_of_precipitation = pop,
           precis,
           uv,
           area,
           image) %>%
    mutate_at(c("minimum_temperature", "maximum_temperature"), ~ as.numeric(gsub(' °C', '', .))) %>% # done here as min sometimes missing
    mutate(obs_url = glue('http://www.bom.gov.au{obs_url}'))
    
  return(df)
  
}


get_latest <- function(url){
  
  doc <- read_html(url)
  
  dt <- doc %>% html_nodes('h2.pointer span') %>% html_text() %>%
    as.POSIXct(., format = 'at %I:%M%p, %a %d %b %Y.')
  
  tbls <- doc %>% html_table()
  tdy <- tbls[[3]]
  
  # check that time at top of table is current
  
  # add date
  tdy %>%
    mutate(date = as.Date(dt),
           time = as.POSIXct(glue('{date} {`Time (AEDT)`}'), format = '%Y-%m-%d %I:%M %p', tz = 'AEDT'),
           now = dt) %>%
    select(date, time, now,
           `Air temperature (°C)` = `Temp (°C)`,
           `Relative humidity (%)` = `Humidity(%)`,
           `Wind speed  km/h` = `Wind Speed (km/h) (knots)`) %>%
    mutate(wind = as.character(`Wind speed  km/h`),
           wind = substr(wind, 1, ceiling(nchar(wind)/2)),
           `Wind speed  km/h` = as.numeric(wind)) %>%
    select(-wind) %>%
    pivot_longer(c(`Air temperature (°C)`,
                   `Relative humidity (%)`,
                   `Wind speed  km/h`),
                 "meas")
  
}


get_fdr <- function(url){
  
  url %>%
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
           ),
           fdr_color = case_when(
             title == 'CODE RED' ~ '#710d08', # should be same as extreme but with black cross hatch
             title == 'EXTREME' ~ '#ee2e24',
             title == 'SEVERE' ~ '#f89829',
             title == 'VERY HIGH' ~ '#fff002',
             title == 'HIGH' ~ '#00adef',
             title == 'LOW-MODERATE' ~ '#79c141'
           )) %>%
    filter(!is.na(date)) %>%
    rowwise() %>%
    mutate(tfb = read_html(item_description) %>% html_node('strong') %>% html_text(),
           tfb = ifelse(tfb == 'not', '', tfb)) %>%
    ungroup() %>%
    distinct()
  
}