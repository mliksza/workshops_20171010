#install.packages(c("tidyverse", "glmnet", "shiny", "splitstackshape"))

#https://www.tidyverse.org/packages/
library(tidyverse)
library(glmnet)
library(shiny)
library(splitstackshape)

## readr usage ## 
film_rates <- read_tsv("films_rates_last.tsv")

# data.frame view
data.frame(film_rates)

## tibble usage ## 
as.tibble(film_rates)  # tibble view

# rename columns
names(film_rates) <- sub(".*_last.", "", names(film_rates))

summary(film_rates)

table(film_rates$titletype)  
film_rates$titletype <- as.factor(film_rates$titletype) # make factor

table(film_rates$isadult)    
film_rates$isadult <- as.logical(film_rates$isadult)  # make logical

table(film_rates$genres)    
# split into a few columns
film_rates_splitted <- cSplit(film_rates, "genres", sep=",") # from splitstackshape package

summary(film_rates_splitted)
table(film_rates_splitted$titletype)

## NA values?
## dplyr usage ##
film_rates_rem_na <- film_rates_splitted %>% 
  filter(!is.na(averagerating)) %>%  # remove records with NA values in averagerating
  select(-processing_dttm) %>%
  filter(titletype == 'movie') # only movies


## use cases ##
top10_ranking <- film_rates_rem_na %>%
  select(primarytitle, averagerating) %>%
  arrange(desc(averagerating)) %>%
  top_n(n = 10, wt = averagerating) 

hist(film_rates_rem_na$numvotes)
summary(film_rates_rem_na$numvotes)

# more reliable
top10_ranking_include_num_votes <- film_rates_rem_na %>%
  select(primarytitle, averagerating, numvotes) %>%
  filter(numvotes >= 400) %>%  # at least 400 votes 
  top_n(n = 10, wt = averagerating) %>%
  arrange(desc(averagerating)) 


# group by genre 
top3_per_genre <- film_rates_rem_na %>%
  select(primarytitle, averagerating, numvotes, genres_1) %>%
  filter(numvotes >= 400) %>%  # at least 400 votes 
  group_by(genres_1) %>%
  top_n(n = 3, wt = averagerating) %>%
  arrange(genres_1, desc(averagerating)) 

table(top3_per_genre$numvotes)
# only movies with votes <= 10000 for better visualization 
top3_per_genre_under_10000_votes <- top3_per_genre %>% 
  filter(numvotes <= 10000)

# visualization 
ggplot(top3_per_genre_under_10000_votes, aes(x = numvotes, y = averagerating)) +
  geom_point(aes(colour = genres_1)) 


# the best movies for adults 
top5_adults <- film_rates_rem_na %>%
  filter(isadult == 'TRUE') %>%
  select(primarytitle, averagerating, numvotes) %>%
  filter(numvotes >= 400) %>%  # at least 400 votes 
  top_n(n = 5, wt = averagerating) %>%
  arrange(desc(averagerating)) 


# Zadanie 1
#Dokonać własnej analizy, w oparciu o wykorzystane wyżej zmienne, bądź wybrać jakąś nową. 

# Zadanie 2
#Dodać nową funkcjonalność do isteniejącej aplikacji.

# Zadanie do domu 
#Dokonać analizy zbioru imdbprincflat_names.tsv oraz zbudować interaktywną aplikację 
#w oparciu o ten zbiór. 

