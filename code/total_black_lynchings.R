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

data <- read_delim("Data.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
View(data)

lynchings_per_year <- data %>% 
        filter(race == "Black") %>% 
        count(year) %>% 
        view()

year_range <- lynchings_per_year %>% 
        summarize(early = min(year), late = max(year)) %>% 
        mutate(range = glue("{early} and {late}")) %>% 
        pull(range)

peak_year <- lynchings_per_year %>% 
        top_n(n,n=1) %>% 
        pull(year)

total_lynchings <- lynchings_per_year %>% 
        summarize(N = sum(n))


lynchings_per_year %>% 
        ggplot(aes(x=year, y=n))+
        geom_line()+
        labs(x="Year", y="Number of Lynchings",
             title = glue("Between {year_range} there were {total_lynchings} Black individuals lynchings with the peak accuring in {peak_year}"))+
        theme_classic()+
        theme(plot.title = element_textbox_simple(size = 20, face = "bold",
                                                  margin = margin(t=10, 0, b=15, 0)),
              plot.title.position = "plot")
