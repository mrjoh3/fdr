


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
                    h4(cats[1]),
                    c3Output(glue("{cats_L[1]}{gsub('-', '', label)}", height = 80)),
                    h4(cats[2]),
                    c3Output(glue("{cats_L[2]}{gsub('-', '', label)}", height = 80))),
             column(6,
                    h4(cats[3]),
                    c3Output(glue("{cats_L[3]}{gsub('-', '', label)}", height = 80)),
                    h4(cats[4]),
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
        mutate(value = as.numeric(value),
               time = as.character(time),
               Thunderstorms = ifelse(is.na(Thunderstorms), 0 , 1),
               Rain = ifelse(is.na(Rain), 0 , 1)) %>%
        arrange(time)
      
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
      
      #print(c3df$value)
      
      select(c3df, time, value) %>% 
        c3(x = 'time', xFormat = '%Y-%m-%d %H:%M:%S', 
           y = 'value', 
           labels = labels, height = 80) %>% 
        c3_chart_size(height = 120) %>% 
        xAxis(type = 'timeseries') %>%
        #yAxis(max = c3df$y_max[1]) %>%
        yAxis(max = max(c3df$value) * 1.2) %>% # give space for icons
        tickAxis('x', culling = list(max = 3)) %>% 
        tickAxis('y', count = 4, format = htmlwidgets::JS('d3.format(".2n")')) %>% 
        legend(hide = TRUE)
    })
  }, 1:length(cats))
  
}


get_precis <- function(location){
  
  url <- glue('http://www.bom.gov.au/places/vic/{location}/forecast/')
  
  doc <- read_html(url) 
  
  days <- doc %>%
    html_nodes('.day')
  
  dates <- seq(Sys.Date(), length.out = length(days), by = 1)
  
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
      mutate(image = glue('http://www.bom.gov.au/{img[1]}'),
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
    mutate_at(c("minimum_temperature", "maximum_temperature"), ~ as.numeric(gsub(' °C', '', .))) # done here as min sometimes missing
  
  return(df)
  
}


contrast <- function(col){
  
  r <- col2rgb(col) %>% as.vector(.)
  
  cont <- (Math.round(rgb[1] * 299) + Math.round(rgb[2] * 587) + Math.round(rgb[3] * 114)) / 1000
  rn <- rgb(255 - r[1],
            255 - r[2],
            255 - r[3], 0)
} 