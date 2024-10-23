# Working direction
################################################################################

getwd()

setwd('C:/Users/Tomy/Desktop/Proyectos Personales/Project 5 - Health Analysis (R)')
################################################################################
# Install & load libreries
################################################################################

#install.packages("tidyverse") # Instalamos una librería
library("tidyverse")          # Cargamos una librería
library("lubridate")
library(dplyr)
library(GGally)
library(caret)
library(party)
library(ggpubr)
library(randomForest)
library(corrplot)
################################################################################
# Data Management y Data Wrangling
################################################################################

risk <- read.csv(file='DeathRisk.csv')

str(risk)
table(risk$Country)
nrow(risk)

risk <- risk %>% 
  mutate(Country = factor(Country))

risk %>% 
  group_by(Country) %>% 
  summarise(Amount = n()) %>% 
  select(Country, Amount)

risk[2:30] <- lapply(risk[2:30], function(x) as.integer(x))

onlycountries <- risk %>% 
  filter(!grepl("\\(WB\\)|\\(WHO\\)", Country)) %>% 
  filter(Country != 'World' & Country != 'G20' & Country != 'OECD Countries')

Org <- risk %>% 
  filter(grepl("\\(WB\\)|\\(WHO\\)|World|G20|OECD Countries", Country))

onlycountries %>%
  group_by(Country) %>% 
  summarise(Amount = mean(Alcohol.Consumption)) %>% 
  select(Country, Amount) %>% 
  arrange(-Amount)

top10 <- onlycountries %>%
  group_by(Country) %>% 
  summarise(Amount = mean(Alcohol.Consumption)) %>% 
  select(Country, Amount) %>% 
  arrange(-Amount) %>% 
  head(10)

top10BP <- onlycountries %>% 
  filter(Country %in% top10$Country)

top10VD <- onlycountries %>% 
  filter(Country %in% top10$Country)

top10Alc <- onlycountries %>% 
  filter(Country %in% top10$Country)
################################################################################
#Visualization
################################################################################

#Blood preasure through years
top10BP %>% 
  group_by(Country, Year) %>% 
  summarise(avg = mean(High.BP)) %>% 
  ggplot(aes(x=Year, y= avg, color= Country)) +
    geom_smooth() +
     scale_y_continuous(labels = scales::comma) +
      labs(title = "Average dead High Blood Pressure Over Time by Country",
       x = "Year", y = "Average High BP")

Org %>% 
  group_by(Country, Year) %>% 
  summarise(avg = mean(High.BP)) %>% 
  arrange(Year, -avg) %>%  
  group_by(Year) %>%
  slice_max(order_by = avg, n = 5) %>%
  ggplot(aes(x=Year, y= avg, color= Country)) +
  geom_smooth() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average dead High Blood Pressure Over Time by Country",
       x = "Year", y = "Average High BP")

#Vitamin deficit through years
top10VD %>% 
  group_by(Country, Year) %>% 
  summarise(avg = mean(Vitamin..Deficiency)) %>% 
  ggplot(aes(x=Year, y= avg, color= Country)) +
  geom_smooth() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average dead Vitamin deficit Over Time by Country",
       x = "Year", y = "Average High BP")

#Alcohol consumption through years
top10Alc %>% 
  group_by(Country, Year) %>% 
  summarise(avg = mean(Alcohol.Consumption)) %>% 
  ggplot(aes(x=Year, y= avg, color= Country)) +
  geom_smooth() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average dead Alcohol consumption Over Time by Country",
       x = "Year", y = "Average High BP")

################################################################################
#Correlations
################################################################################

correlation <- cor(onlycountries$High.BP, onlycountries$Vitamin..Deficiency, use = "complete.obs")
print(correlation)

cor_matrix <- cor(onlycountries[, 2:30], use = "complete.obs")
print(cor_matrix)

selected_columns <- onlycountries[, c("High.BP", "Excess.Sodium", "High.Body.Mass.Index.Obesity.", "Alcohol.Consumption", "High.LDL.Cholesterol", "Passive.Smoking")]

correlation_matrix <- cor(selected_columns)
print(correlation_matrix)

corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 45)

################################################################################
#LM
################################################################################

model <- lm(High.BP ~ Excess.Sodium + 
              High.Body.Mass.Index.Obesity. + 
              Alcohol.Consumption + 
              High.LDL.Cholesterol +
              Passive.Smoking, data = onlycountries)
summary(model)

ggplot(onlycountries, aes(x = Excess.Sodium, y = High.BP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, col = "red")

ggplot(onlycountries, aes(x = High.Body.Mass.Index.Obesity., y = High.BP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, col = "blue")




