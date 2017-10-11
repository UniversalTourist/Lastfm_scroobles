# What have I listened since 2010?

install.packages('readxl')
install.packages('dplyr')
install.packages('plyr')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('tidyverse')
install.packages("wesanderson")

library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(tidyverse)
library(wesanderson)

myPlays <- read_excel("../lastfmFiles/samcohen.xlsx")
as.POSIXct(myPlays$date, format="%d/%m/%Y %H:%M:%S", tz="CET")
allMonths <- myPlays %>% expand(year(date), month(date)) %>% filter('year(date)' >= 2010)


monthlyPlays <- myPlays %>% group_by(year(date), month(date)) %>% 
  right_join(allMonths) %>%
  tally() %>% 
  mutate(month = as.Date(paste(`year(date)`, `month(date)`, "01", sep = "-"))) 

ggplot(monthlyPlays, aes(x = month, y = n, color = "red" )) + 
  geom_line(size = 1) + labs(x = "Month", y = "Number of Scrobbles") +
  ggtitle("My Lastfm Scrobbles") 

# top ten artist
myTopten <- myPlays %>%
  count(artist, sort = TRUE) %>%
  top_n(n = 10, wt = n)

ggplot(myTopten, aes(x = reorder(artist,n), y = n, fill = artist)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_gray() +
    labs(y = "Scrobbles", x = "")


# florence + the machine scrobbles
Flo_plays <- myPlays %>% filter(artist == "Florence + the Machine", year(date) >= 2010) 
Flo_plays %>% group_by(year(date), artist) %>% tally() %>% 
  top_n(1, wt = n) %>% 
  rename(year = `year(date)`) %>% ggplot(aes(x = year, y = n, fill = year)) +
  geom_bar(stat = "identity", width = 0.7) + ggtitle("Florence + Machine Scrobbles by year") +
  labs(y = "Scrobbles", x = "Year")

# My Top 10 artists scrobbles by month
myPlays %>% filter(artist %in% myTopten$artist) %>% group_by(year(date), month(date), artist) %>%
  tally() %>% 
  mutate(artist = factor(artist, levels = myTopten$artist)) %>%
  mutate(month = as.Date(paste(`year(date)`, `month(date)`, "01",
                               sep = "-"))) %>%
  ggplot(aes(x = month, y = n, color = artist)) +
  geom_line() + ggtitle("My top 10 artist Scrobbles") +
  facet_wrap(~ artist, ncol = 2) +
  theme(strip.text.x = element_text(size = 10)) +
  labs(x = "Month", y = "Scrobbles")

# My Top 10 songs
myTopten_songs <- myPlays %>%
  count(song, sort = TRUE) %>%
  top_n(n = 10, wt = n)

myPlays %>% filter(song %in% myTopten_songs$song) %>% 
  group_by(song) %>% mutate(count = n()) %>%
  ggplot(aes(x = year(date), color = song)) +   geom_step(aes(len = count, y = ..y.. * len), stat = "ecdf", size = 1.5) +
  labs(x = "Year", y = "Cumulative scrobbles", color = "Song")


  
















