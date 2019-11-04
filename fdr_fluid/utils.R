


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
        c3_chart_size(height = 100) %>% 
        xAxis(type = 'timeseries') %>% 
        tickAxis('x', culling = list(max = 3)) %>% 
        tickAxis('y', count = 4, format = htmlwidgets::JS('d3.format(".2n")')) %>% 
        legend(hide = TRUE)
    })
  }, 1:length(cats))
  
}


