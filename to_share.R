library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(gganimate)

df <- read_csv('~/downloads/statedatabyseason56_55.csv') # from https://gis.cdc.gov/grasp/fluview/main.html
df <- select(df, STATENAME, `ACTIVITY LEVEL`, WEEK, SEASON)
df <- filter(df, WEEK >= 41 | WEEK <= 7)

df$`Activity Level` <- as.numeric(stringr::str_extract(df$`ACTIVITY LEVEL`, "\\(?[0-9,.]+\\)?"))

df <- select(df, -`ACTIVITY LEVEL`)

df <- df %>% 
    spread(SEASON, `Activity Level`) %>% 
    mutate(`Difference Between Years` = `2016-17` - `2015-16`) %>% 
    arrange(STATENAME)

df$WEEK_f <- factor(df$WEEK,
                    levels = c(c(41:52), c(1:8)))

# for plot 

states <- map_data("state")

df$region<- tolower(df$STATENAME)
df <- df[df$region %in% states$region, ]
df$WEEK <- paste0("Week ", df$WEEK_f, sep = "")
df$WEEK <- factor(df$WEEK,
                  levels = c(paste0("Week ", c(41:52)), paste0("Week ", c(1:8))))

gg <- ggplot() +
    geom_map(data = states, map = states,
             aes(x = long, y = lat, map_id = region),
             color= "white" , size = 0.1) +
    geom_map(data = df, map = states,
             aes(fill = `Difference Between Years`, map_id = region, frame = WEEK),
             color="white", size = 0.1) +
    coord_map(proj = "bonne", param = 45) +
    scale_fill_gradient("", high = '#e9a3c9', low = '#a1d76a',
                        breaks = c(9, 5, 0, -5, -6),
                        labels = c("Higher in 16-17",5, 0, -5,"Higher in 15-16")) +
    theme_map() +
    ggtitle("Comparison of Flu Activity Levels") +
    labs(subtitle = "Data from: https://gis.cdc.gov/grasp/fluview/main.html. Flu activity is measured on a 1-10 scale.",
         caption = "Created by: http://jrosen48.github.io.") +
    theme()

gg

animation::ani.options(ani.width=600, ani.height=420)
gganimate(filename = "final_draft.mp4")