
library(tidyRSS)
library(dplyr)
library(lubridate)
library(stringr)

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

fullcalendar(df, settings = list(header = list(center = 'dayGridMonth,timeGridFourDay'),
                                 views = list(
                                   timeGridFourDay = list(
                                     type = 'timeGrid',
                                     duration = list(days = 4 ),
                                     buttonText = '4 day'
                                   )
                                 )))

# ggplot 
library(ggplot2)

ggplot(df, aes(x = week, y = day, fill = color)) +
  geom_tile(color = 'white') +
  geom_text(aes(label=start)) +
  scale_fill_manual(values=df$color) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(df$day))) +
  theme_minimal() + 
  theme(legend.position="none")

