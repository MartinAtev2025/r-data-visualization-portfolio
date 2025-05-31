
# LIBRARYS ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(tidyr)
library(stringr)
library(lubridate)

# # value - dates, years 

# DATA

covid_effects <- read.csv("C:/Users/Martin/Downloads/
                          effects-of-covid-19-on-trade-at-15-december-2021-provisional.csv")

head(covid_effects,2)
l


covid_efect_short <- covid_effects %>% filter(Direction == "Exports", Measure == "$", Country == "All") %>% select(Direction, Year, Date, Measure, Value)
covid_efect_short$Date <- as.Date(covid_efect_short$Date)

covid_monthly <- covid_efect_short %>%
  mutate(month = floor_date(Date, unit = "month")) %>%
  group_by(month) %>%
  summarise(total_exports = sum(Value, na.rm = TRUE))
covid_monthly

# VISUALISATION

ggplot(covid_monthly, aes(x = month, y = total_exports)) + geom_line(col ="darkblue") + 
  labs(
    x = "Month",
    y = "Total Export") + ggtitle("Total export for all Countrys thru years: 2015 - 2020") + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %M") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 




