# SCOOBYDOO --------------------------------------------------------------------

#LIBRARYS ----------------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggthemes)

# RAW DATA ---------------------------------------------------------------------

scoobydoo <- read.csv("C:/Users/Martin/Downloads/scoobydoo.csv")
tail(scoobydoo, 2)


# Number of Episodes per Scooby-Doo Show ------------------------------------------------

# DATA
number_of_episodes_per_serie <- scoobydoo %>% 
  group_by(series_name) %>%
  summarise(number_of_episodes = n())

number_of_series_more_than_3_episode <- number_of_episodes_per_serie %>%
  filter(number_of_episodes > 3)
number_of_series_more_than_3_episode

# VISUALISATION

# COLUMN PLOT
ggplot(number_of_series_more_than_3_episode, aes(y = series_name, x  = number_of_episodes)) +
  geom_col(col = "black", fill = "lightgreen") +
  labs(
    x = " ",
    y = "Show Name") + 
  ggtitle("Number of Episodes per Scooby-Doo Show") 

# BARPLOT
ggplot(number_of_series_more_than_3_episode, aes(y = series_name, fill  = number_of_episodes)) +
  geom_bar(col = "black") +
  labs(
    x = " ",
    fill = "Number of Episodes",
    y = "Show Name") + 
  ggtitle("Number of Episodes per Scooby-Doo Show") + 
  scale_fill_gradient(high = "white", low = "#421869")




# Most common Monsters ---------------------------------------------------------

# DATA 

monsters <- scoobydoo
monsters <- monsters %>% group_by(monster_type) %>% summarise(count = n())
monsters_top5 <- monsters %>% filter(count > 25, monster_type != "NULL")
monsters

# VISUALISATION

ggplot(monsters_top5, aes(x = monster_type, y = count)) +
  geom_col(col = 'black', fill = 'purple') + 
  labs(
    x = "Type of Monster",
    y = "Number of Monsters") + 
  ggtitle("The 5 Monsters mostacured in Scooby-Doo Series") +
  geom_text(aes(label = count), vjust = -0.5, size = 5 ) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme_pander()


# Most common Motive -----------------------------------------------------------

# DATA 

data_motives <- scoobydoo %>% 
  group_by(motive) %>%
  summarise(count = n()) %>% 
  filter(motive != "NULL") %>%
  arrange(desc(count))

data_motives

# VISUALISATION

ggplot(head(data_motives,10), aes(x = motive, y = count)) + 
  geom_col(col = "black", fill = "lightgreen") + 
  labs(y = "Count",
       x = "Motive") + 
  ggtitle("Most common Motive for Crime in Scooby-Doo") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label = count), vjust = 1, size = 5)








# hero - unmasking --------------------------------------------------------------

# DATA
names_heros <- c("Daphnie", "Fred", "Scooby", "Shaggy", "Velma")
scoobydoo <- scoobydoo

data_caught <- scoobydoo %>%  
  select(index, caught_fred, caught_velma, caught_daphnie, caught_shaggy, caught_scooby) %>%
  pivot_longer(!index, names_to = "Hero", values_to = "Caught") %>% filter(Caught != 'NULL')

data_caught$Caught <- factor(data_caught$Caught, levels = c("FALSE", "TRUE"), labels = c(0, 1))
data_caught$Hero <- factor(data_caught$Hero)
data_caught <- data_caught %>% group_by(Hero) %>% summarise(score = n()) 
data_caught

# VISUALISATION
ggplot(data_caught, aes(y = Hero, fill = score)) + 
  geom_bar() +
  labs(
    y = "Hero",
    x = "Number of Monsters Caught",
    fill = "Count") + 
  ggtitle('kur')  +   
  scale_fill_gradient(high = "lightblue", low = "blue") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))


# IMDB rating of Scooby-Doo series thru the Years ------------------------------

# DATA

imdb <- scoobydoo %>% 
  select(index, date_aired, imdb)

imdb$date_aired <- as.Date(imdb$date_aired)
head(imdb, 2)

# VISUALISATION

ggplot(imdb, aes(x = date_aired, y = imdb)) + 
  geom_line(col = "brown", size = 0.5) + 
  geom_point(col = "darkgreen", size = 0.5) + 
  labs(
    x = "Year", 
    y = "IMDB") +
  ggtitle("IMDB rating of Scooby-Doo series thru the Years") + 
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))+ 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 60))













 
