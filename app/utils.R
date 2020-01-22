


mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


# 4 plots in a box UI (rendered inside server , renderUI)
c3_grid_UI <- function(label, location, cats){
  
  cats_L <- tolower(word(unique(cats), 1))
  
  tagList(
    fluidRow(column(6,
                    h5(cats[1]),
                    c3Output(glue("{gsub('-', '', location)}{cats_L[1]}{gsub('-', '', label)}", height = 80)),
                    h5(cats[2]),
                    c3Output(glue("{gsub('-', '', location)}{cats_L[2]}{gsub('-', '', label)}", height = 80))),
             column(6,
                    h5(cats[3]),
                    c3Output(glue("{gsub('-', '', location)}{cats_L[3]}{gsub('-', '', label)}", height = 80)),
                    h5(cats[4]),
                    c3Output(glue("{gsub('-', '', location)}{cats_L[4]}{gsub('-', '', label)}", height = 80)))
             )
  )
  
}


c3_grid_server <- function(input, output, session, wind, label, location, cats){
  
  cats_L <- tolower(word(unique(cats), 1))
  
  Map(function(pl){
    output[[glue("{gsub('-', '', location)}{cats_L[pl]}{gsub('-', '', label)}")]] <- renderC3({
      
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
  
  forcast_datetime <- doc %>% html_node('.date') %>% html_text() %>%
    as.POSIXct(., format = "Forecast issued at %I:%M %p AEDT on %a %d %b %Y", tz = 'AEDT')
  forcast_date <- as.Date(forcast_datetime)
  
  dates <- seq(forcast_date, length.out = length(days), by = 1)
  
  obs_url <- doc %>% html_nodes('.obs a') %>% html_attr('href')
  
  df <- lapply(1:length(days), function(n){

    dd <- days[[n]] %>%
      html_nodes('dd')
    
    img <- dd %>% html_node('img') %>% html_attr('src')
    area <- days[[n]] %>% html_node('h3') %>% html_text()
    precis <- days[[n]]  %>% html_node('p') %>% html_text()
    precis_summary <- days[[n]]  %>% html_node('dd.summary') %>% html_text()
    uv <- days[[n]]  %>% html_node('p.alert') %>% html_text()
    names <- dd %>% html_attr('class') 
    #hd <- days[[n]] %>% html_node('h2') %>% html_text() %>% as.Date(., format = '%a %d %b %Y')
    
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
             precis_summary = precis_summary, # present at MIldura but can't find elsewhere
             uv = uv, 
             date = dates[n],
             forcast_datetime = forcast_datetime) %>%
      separate(amt, c('lower_precipitation_limit', 'upper_precipitation_limit'), sep = ' to ', remove = TRUE) %>%
      mutate(upper_precipitation_limit = gsub(' mm', '', upper_precipitation_limit),
             pop = str_trim(pop, 'both'),
             uv = unlist(str_extract_all(uv, '[1-9]')) %>% .[length(.)]) %>%
      mutate_at(c('lower_precipitation_limit', 'upper_precipitation_limit', 'uv'), as.numeric) 
    
  }) %>% bind_rows() %>%
    select(date,
           forcast_datetime,
           minimum_temperature = min,
           maximum_temperature = max,
           lower_precipitation_limit,
           upper_precipitation_limit,
           probability_of_precipitation = pop,
           precis,
           precis_summary,
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
    as.POSIXct(., format = 'at %I:%M%p, %a %d %b %Y.', tz = 'AEDT')
  
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


get_detailed <- function(url){
  
  tbls <- read_html(url) %>%
    html_table() 

  n_tbls <- ifelse(length(tbls)%%5 == 0, 5, 6)
  
  tbls %>%
    lapply(., function(df){
      if (colnames(df)[1] == '') {colnames(df)[1] <- 'wave'}
      rename_at(df, vars(1), ~ sub('At|From|wave','meas', .)) %>%
        #df %>%
        mutate_all(as.character)
    }) %>%
    setNames(
      sort(rep(seq(Sys.Date(), length.out = length(.) / n_tbls, by = "1 days"), n_tbls))
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
  
}


get_fdr <- function(url){
  
  url %>%
    tidyfeed(.) %>%
    mutate(item_title = gsub('Today, |Tomorrow, ', '', item_title),
           date = as.Date(item_title, '%a, %d %b %Y'),
           title = str_extract(item_description, 'LOW-MODERATE|HIGH|VERY HIGH|SEVERE|EXTREME|CODE RED'),
           start = date,
           end = date + days(1),
           day = wday(start, label = TRUE),
           week = isoweek(start),
           rendering = 'background',
           color = case_when(
             title == 'CODE RED' ~ 'black', # should be same as extreme but with black cross hatch
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

fdr_colour <- c('LOW-MODERATE' = '#79c141',
                'HIGH' = '#00adef',
                'VERY HIGH' = '#fff002',
                'SEVERE' = '#f89829',
                'EXTREME' = '#ee2e24',
                'CODE RED' = '#710d08')


calc_fdi <- function(df, mobile = FALSE){
  
  daily <- df %>% 
    filter(meas %in% c('Relative humidity (%)',
                       'Air temperature (°C)',
                       'Wind speed  km/h',
                       'Forest fuel dryness factor')) %>%
    mutate(meas = tolower(word(meas, 1))) %>%
    select(time, 
           now,
           meas,
           value) %>%
    pivot_wider(names_from = meas, values_from = value) %>%
    arrange(time) %>%
    fill(forest, .direction = 'updown') %>%
    mutate(fdi = 2 * exp(-0.45 + (0.987 * log(forest)) + (0.0338 * air) - (0.0345 * relative) + (0.0234 * wind)),
           fdr = case_when(
             fdi > 94 ~ 'CODE RED', 
             fdi > 74 ~ 'EXTREME',
             fdi > 49 ~ 'SEVERE',
             fdi > 24 ~ 'VERY HIGH',
             fdi > 11 ~ 'HIGH',
             fdi > 0  ~ 'LOW-MODERATE'
           )) %>% 
    filter(!(is.na(now) & is.na(fdi))) %>% 
    mutate(cnk = ifelse(fdr != lead(fdr, 1), time, NA)) %>%
    fill(cnk, .direction = 'up') %>% 
    mutate(cnk = ifelse(is.na(cnk), max(cnk, na.rm = T) + 999, cnk))  # last cnk always NA if long too much missing data
      # ggplot(data = ., aes(x = t2, ymax = fdi, ymin = 0, group = cnk, fill = fdr)) +
      #   geom_ribbon()
      # ggplot(data=dat3, aes(x=date, ymax=count, ymin=0, group=df, fill=month)) + geom_ribbon()
    
  daily %>%
    group_by(cnk, fdr) %>% 
    summarise(xmin = min(time),
              xmax = max(time),
              fdi = mean(fdi)) %>% 
    ungroup() %>% 
    mutate(xmax = lead(xmin, 1), 
           xmax = if_else(is.na(xmax),
                          #as.POSIXct(glue('{as.Date(xmin) + days(1)} 00:00"))'), tz = 'AEDT'), 
                          as.POSIXct(max(df$time), tz = 'AEDT'), 
                          xmax), # catch mising last value
           fdr = factor(fdr, levels = rev(names(fdr_colour)))) %>%
    ggplot() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 150, fill = fdr)) +
      geom_area(data = daily, stat = 'identity', aes(x = time, y = fdi), alpha = .2) +
      scale_color_manual(values = fdr_colour,
                         aesthetics = c("colour", "fill")) +
      #scale_y_sqrt() +
      #xlim(min(daily$time), as.POSIXct('2020-01-15 00:00:00')) + # breaks geom rects so backgound colour drops
      theme_minimal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_text(colour = "lightgrey", 
                                   size = ifelse(mobile, 30, 14)),
          axis.text.y=element_text(colour = "lightgrey",
                                   size = ifelse(mobile, 14, 8)),
          axis.ticks.y=element_blank(),
          #axis.ticks.x = element_line(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
    
}


fdr_images <- function(mobile){
  
  if (mobile) {
    
    tl <- div(style = 'padding: 30px;',
        tags$hr(),
        h3('Fire Danger Ratings'),
        tabsetPanel(
          tabPanel(h5('Forest'),
                   tags$img(src = 'http://www.bom.gov.au/fwo/IDV65406.png', width = '95%')),
          tabPanel(h5('Grassland'),
                   tags$img(src = 'http://www.bom.gov.au/fwo/IDV65426.png', width = '95%'))
        )
    )
    
  } else {
    
    tl <- div(style = 'padding: 30px;',
              tags$hr(),
              h3('Fire Danger Ratings'),
              column(6,
                h5('Forest'),
                tags$img(src = 'http://www.bom.gov.au/fwo/IDV65406.png', width = '95%')),
              column(6,
                h5('Grassland'),
                tags$img(src = 'http://www.bom.gov.au/fwo/IDV65426.png', width = '95%'))
              
    )
    
  }
    
  return(tl)  
  
}


render_current <- function(statewide, towns, location, buffer = 40) {
    
  sel <- towns %>% filter(town_val == location) %>% 
    st_transform(3111)
  
  splt <- c('Sheep Grazier Warning',
            'Strong Wind',
            #'Total Fire Ban',
            'Weather',
            'Met')
  
  # miscelaneous warnings
  sheep_wind <- statewide %>%
    filter(category2 %in% splt) # need way to use these
  
  statewide <- statewide %>%
    filter(!(category2 %in% splt)) # have overlapping geometry might need to separate before join (could be same for wind warning???)
  

  
  # current situation within 20km
  cur = sel %>% st_buffer(dist = buffer * 1000) %>% 
    st_intersection(statewide) %>%
    mutate(dist = st_distance(., sel),
           dist = round(units::set_units(dist, km), 1)) %>%
    arrange(as.numeric(dist))
  
  warnBox <- box(solidHeader = TRUE, background = 'black', width = 12,
                 p('WARNING: If you are likely to be impacted by an incident see original source data at ', 
                   tags$a(href = 'http://emergency.vic.gov.au/respond/', 'http://emergency.vic.gov.au/respond/')))
  
  if (nrow(cur) == 0) {
    return(
      tagList(div(style = 'padding: 30px;',
                  tags$hr(),
                  h3('Current Situation'),
                  p(glue('There are currently no incidents or warnings within {buffer} km of your selected location. '),
                    tags$br(),
                    warnBox))
                    
      )
    )
  } else {
    
    # burn area
    ba <- cur %>% 
      filter(feedType == 'burn-area') %>%
      mutate(area = st_area(.),
             area = round(units::set_units(area, ha), 2)) %>% 
      select(status, location, dist, area)
    
    # warnings
    wn <- cur %>% 
      filter(feedType == 'warning' | status %in% c('warning','Warning')) %>% 
      select(status, type = sourceTitle, location, dist)
    
    # incidents
    inc <- cur %>% 
      filter(feedType %in% c('incident','earthquake'), 
             category2 != 'Total Fire Ban',
             category1 != 'Met',
             status != 'warning') %>% 
      select(status, location, category = category2, category1, resources, size = sizeFmt, dist)
    
    # tfb
    tfb <- cur %>% filter(feedType == 'incident', category2 == 'Total Fire Ban')
    
    # combine in list
    comb = list(tfb = tfb,
                warnings = wn,
                incidents = inc,
                burnarea = ba)
    
    # drop ellements with no rows returned
    comb[sapply(comb, nrow) == 0]  <- NULL
    
    # create leaflet map
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = 'Default') %>%
      addProviderTiles("Esri.WorldImagery", group = 'Aerial') %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = 'Street') %>%
      addMarkers(data = st_transform(sel, 4236),
                 popup = ~glue('<strong>{town_name}</strong>')) %>%
      addPolylines(data = st_transform(st_buffer(sel, buffer * 1000), 4236),
                   color = 'red',
                   weight = 1) %>%
      addLayersControl(
        baseGroups = c("Default", "Aerial", "Street"),
        overlayGroups = c('Incidents', 'Burnt Area'),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    inc_icon <- awesomeIconList(
      Fire = makeAwesomeIcon(icon= 'fire', markerColor = 'red', iconColor = '#FFFFFF', library = "fa"),
      `Planned Burn` = makeAwesomeIcon(icon= 'fire', markerColor = 'darkred', iconColor = '#FFFFFF', library = "fa"),
      `Building Damage` = makeAwesomeIcon(icon= 'home', markerColor = 'lightgray', iconColor = '#FFFFFF', library = "fa"),
      `Accident / Rescue` = makeAwesomeIcon(icon= 'car', markerColor = 'orange', iconColor = '#FFFFFF', library = "fa"),
      `Tree Down` = makeAwesomeIcon(icon= 'tree', markerColor = 'green', iconColor = '#FFFFFF', library = "fa"),
      `Hazardous Material` = makeAwesomeIcon(icon= 'nuclear', markerColor = 'black', iconColor = '#FFFFFF', library = "ion"),
      Rescue = makeAwesomeIcon(icon= 'first-aid', markerColor = 'darkgreen', iconColor = '#FFFFFF', library = "fa"),
      Flooding = makeAwesomeIcon(icon= 'tiny', markerColor = 'darkblue', iconColor = '#FFFFFF', library = "fa"),
      Earthquake = makeAwesomeIcon(icon= 'podium', markerColor = 'darkblue', iconColor = '#FFFFFF', library = "ion"),
      Other = makeAwesomeIcon(icon= 'question-circle', markerColor = 'purple', iconColor = '#FFFFFF', library = "fa")
    )
    
    if (nrow(inc) > 0) {
      map <- map %>% 
        addAwesomeMarkers(data = st_transform(inc, 4236) %>% st_cast("POINT"), 
                          icon = ~inc_icon[category1],
                          popup = ~paste(sep = '<br>',
                                        glue('<strong>{status}</strong>'),
                                        ifelse(category1 == category,
                                               glue('Category: {category1}'),
                                               glue('Category: {category1} - {category}')),
                                        glue('Location: {location}'),
                                        glue('Resources: {resources}'),
                                        glue('Size: {size}')),
                          group = 'Incident')
    }
    if (nrow(ba) > 0) {
      map <- map %>% 
        addPolygons(data = st_transform(ba, 4236), 
                    weight = 1,
                    color = 'red',
                    fillColor = 'black',
                    popup = ~paste(sep = '<br>',
                                   glue('<strong>Status: {status}</strong>'),
                                   glue('Location: {location}'),
                                   glue('Burnt Area (Ha): {area}')),
                    group = 'Burnt Area')
      }
    
    cs <- lapply(1:length(comb), function(x){
      tnm <- names(comb)[x] # table name
      df <- comb[[x]]
      st_geometry(df) <- NULL
      
      if (tnm == 'tfb') {
        return(tabPanel(
          h5('Total Fire Ban'),
          p(df[['webHeadline']][1],
            'For more details see ',
            tags$a(href = df[['url']][1], df[['url']][1]))
        ))
      } else if (tnm == 'incidents') {
        return(
          tabPanel(
            h5('Incidents'),
            #p(glue('All current Incidents within {buffer} km')),
            HTML(knitr::kable(df %>% select(-category1), format = 'html', table.attr = "class=\"current\""))
          )
        )
      } else if (tnm == 'burnarea') {
        return(
          tabPanel(
            h5('Burnt Area'),
            #p(glue('Burnt Area within {buffer} km')),
            HTML(knitr::kable(df, format = 'html', table.attr = "class=\"current\""))
          )
        )
      } else if (tnm == 'warnings') {
        return(
          tabPanel(
            h5('Warnings'),
            #p(glue('All Warnings within {buffer} km')),
            HTML(knitr::kable(df, format = 'html', table.attr = "class=\"current\""))
          )
        )
      }
    })
    
    return(
      tagList(
            if (nrow(inc) > 0 | nrow(ba) > 0) {map} else {tags$br()},
            tags$br(),
            do.call(tabsetPanel, cs),
            tags$br(),
            warnBox
      )
    )
    
  }

  

  
}