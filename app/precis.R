

pc <- 'http://www.bom.gov.au/places/vic/st-andrews/forecast/' %>%
  read_html() %>% 
  html_nodes('.day')

d1 <- pc[[1]] %>%
  html_nodes('dd') %>%
  html_attr('class')
