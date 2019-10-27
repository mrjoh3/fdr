
library(tidyRSS)
library(dplyr)
library(lubridate)

url <- 'https://www.cfa.vic.gov.au/documents/50956/50964/central-firedistrict_rss.xml'

df <- tidyfeed(url) %>%
  mutate(date = as.Date(gsub('Today, |Tomorrow, ', '', item_title), '%a, %d %b %Y'),
         title = str_extract(item_description, 'LOW-MODERATE|HIGH|VERY HIGH|SEVERE|EXTREME|CODE RED'),
         start = date,
         end = date + days(1),
         color = case_when(
           title == 'CODE RED' ~ '#C80815',
           title == 'EXTREME' ~ '#FF4040',
           title == 'SEVERE' ~ 'orange',
           title == 'VERY HIGH' ~ '#FCF75E',
           title == 'HIGH' ~ 'skyblue',
           title == 'LOW-MODERATE' ~ '#ADDFAD'
         )) %>%
  filter(!is.na(date))


# calender setup
# https://github.com/rasmusab/fullcalendar
#devtools::install_github("rasmusab/fullcalendar")
library(fullcalendar)

fullcalendar(df)
