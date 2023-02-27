library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(glue)
library(ggtext)


# time course for number lynchings
# * across the US
# * by state
#what states had most lynchings?
# how widespread?

table(lynchings_map_data$n)

state_lookup <- tibble(abb = state.abb,
                       state = state.name)

lynchings_per_state_county <- read_delim("Data.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE) %>% 
        filter(race == "Black") %>% 
        count(state, county) %>% 
        inner_join(., state_lookup, by = c("state" = "abb")) %>% 
        select(state, county, n)

state_county <- map_data("county") %>% 
        select(region, subregion) %>% 
        distinct()

anti_join(lynchings_per_state_county, state_county, by = c("county" = "subregion", "state" = "region"))    


county_map <- lynchings_per_state_county %>% 
        distinct(state) %>% 
        inner_join(., map_data("county"), by = c("state"="region")) %>% 
        distinct()

lynchings_map_data <- right_join(lynchings_per_state_county, county_map,
           by = c("state" = "state", "county" = "subregion"))

table(lynchings_map_data$n)                      
class(n)
lynchings_map_data$n <- as.numeric(lynchings_map_data$n)
class(lynchings_per_state_county$n)
sum(is.na(lynchings_map_data$n))

lynchings_map_data %>% 
        mutate(n = if_else(is.na(n), 0, n)) %>% 
        ggplot(aes(x=long, y=lat, fill=n, group=group))+
        geom_polygon(color = "black")+
        coord_quickmap()

ggsave("lynchings_map.pdf", height =5,  width = 4)