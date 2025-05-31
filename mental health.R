# Librarys ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(tidyr)
library(stringr)

# Data: MentalHealthSurvey.csv -------------------------------------------------

mental_health <- read.csv("C:/Users/Martin/Downloads/MentalHealthSurvey.csv")
head(mental_health, 2)

#Average Financial Concern by Major --------------------------------------------


# DATA
avg_concern <- mental_health %>% 
  group_by(degree_major) %>%
  summarise(avg_finance_concern = mean(financial_concerns, na.rm = TRUE)) 
avg_concern


#VISUALISATION 
ggplot(avg_concern, aes(x = degree_major, y = avg_finance_concern)) + 
  geom_col(col = "#7E191B", fill = "#960018") + 
  labs(
    x = "Major",
    y = "Financial Concern",
    title = "Average Financial Concern by Major"
  ) + theme_tufte() + 
  geom_text(aes(label = round(avg_finance_concern, 2)), 
            vjust = 3, size = 5.5) + 
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = 'bold'))


#  Average sleep in diferent years in University -------------------------------

#DATA - avg_sleep_in_academic_years_grouped

avg_sleep_in_academic_years <- mental_health
 
avg_sleep_in_academic_years$average_sleep <- factor(avg_sleep_in_academic_years$average_sleep, 
                                                    levels = c("2-4 hrs", "4-6 hrs", "7-8 hrs"),
                                                    labels = c(3, 5, 7.5))

color_code1 <- c("3" = "red", "5" = "yellow", "7.5" = "green")
# VISUALISATION 

ggplot(avg_sleep_in_academic_years, aes(x = academic_year, fill = average_sleep)) +
  geom_bar(col = "black") +
  scale_fill_manual(values = color_code1) +
  labs(x = "Year of study",
       y = " ",
       fill = "Average sleep") + 
  ggtitle("Average sleep in diferent years in University") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, face = "bold"))


# Psychological Factors vs Sleep Duration --------------------------------------

# DATA
fears <- mental_health
head(fears, 2)

fears_long <- fears %>%
  select(average_sleep, depression, anxiety, isolation, future_insecurity) %>%
  pivot_longer(cols = -average_sleep, names_to = "factor", values_to = "score") %>%
  group_by(average_sleep, factor) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")
fears_long


#VISUALISATION

ggplot(fears_long, aes(x = average_sleep , y = mean_score, colour = factor, group = factor)) + 
  geom_point() +
  geom_line(size = 1.2) +
  labs(
    x = "Hours of sleep",
    y = " ",
    colour = "Mental problems") + 
  ggtitle("Psychological Factors vs Sleep Duration")
















