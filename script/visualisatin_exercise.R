library(tidyverse)

gapminder1 <- read_csv("data/gapminder.csv")
gapminder2 <- read_csv("data/gapminder_2012.csv")
gapminder <- bind_rows(gapminder1, gapminder2)
data1977 <- filter(gapminder, year==19977)
