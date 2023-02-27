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

state_lookup <- tibble(abb = state.abb,
       name = state.name)

data2 <- read_delim("Data.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
View(data2)

lynchings_per_state_per_year <- data2 %>% 
        filter(race == "Black") %>% 
        count(year, state) %>% 
        inner_join(., state_lookup, by = c("state" = "abb")) %>% 
        view()


lynchings_per_state <- lynchings_per_state_per_year %>% 
        group_by(name) %>% 
        summarize(N = sum(n)) %>% 
        mutate(state = glue("{name} ({N})"))


lynchings_per_state_per_year %>% 
        inner_join(., lynchings_per_state, by = "name") %>%
        arrange(N) %>% 
        ggplot(aes(x=year, fill=n, y=name))+
        geom_tile()+
        scale_fill_gradient(name = "Number of\nLynchings", low = "#FFFFFF", high = "#FF0000",
                            limits=c(0, NA))+
        labs(x="Year", y=NULL, title = glue("Between 1883-1941 Texas had the most lynchings"))+
        theme_classic()+
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_textbox_simple(size = 20, face = "bold",
                                                   margin = margin(t=10, 0, b=15, 0)),
        plot.title.position = "plot")
