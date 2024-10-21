# Working direction
################################################################################

getwd()

setwd('C:/Users/Tomy/Desktop/Proyectos Personales/Project 4 - Traffic Accident Analysis (R)')

# Install & load libreries
################################################################################

#install.packages("tidyverse") # Instalamos una librería
library("tidyverse")          # Cargamos una librería
library("lubridate")


# Data Management y Data Wrangling
################################################################################

accidents <- read.csv(file='US_Accidents_March23.csv')

Maploc <- accidents %>% 
  select(Start_Lat,Start_Lng,Description)
rm(accidents)

str(accidents)

class(accidents)
nrow(accidents)     # Count rows
head(accidents)     # First 5 rows
head(accidents, 3)  
summary(accidents$Distance.mi.)   #summary(accidents[,"Distance.mi."])   
summary(accidents[,17])        
summary(accidents$Severity)
table(accidents$State)
table(accidents$Country)

# Missing values

table(is.na(accidents$Severity))  
table(is.na(accidents$End_Time))  
nrow(accidents[accidents$City == "",])
table(is.na(accidents$Weather_Condition))


#Less severe accidents // tidyverse pipe

select(accidents, City, Severity) %>%
  group_by(City) %>% 
  summarise(MaxSev = max(Severity)) %>% 
  arrange(MaxSev) %>%
  head(.,10)

# Enumerate Sources
accidents <- accidents %>%
  mutate(Source = factor(Source))
table(accidents$Source)

# Select Columns and create a new filtered table
colnames(accidents)

acctable <- accidents %>% 
  select(ID, Severity, Start_Time, End_Time, Start_Lat, Start_Lng, Description, City, State, Zipcode, Temperature.F.,Humidity...,Visibility.mi.,Wind_Speed.mph.,Precipitation.in.,Weather_Condition, Bump, Crossing,Give_Way,Junction,No_Exit,Railway,Roundabout,Station,Stop,Traffic_Calming,Traffic_Signal,Turning_Loop)

rm(accidents)
rm(nombres_lst)
us_accidents <- na.omit(acctable)
rm(acctable)

us_accidents <- us_accidents %>% filter(City != " ")
us_accidents <- us_accidents %>% filter(Start_Lat != "" | Start_Lat != " ")
us_accidents <- us_accidents %>% select(-End_Time)

#Dates

class(us_accidents$Start_Time)

us_accidents <- us_accidents %>% 
  mutate(Start_Time = as.Date(Start_Time)) #Date format to column

str(us_accidents)

us_accidents %>% 
  pull(Start_Time) %>% 
  min()                 #Min Date "2016-01-14"

us_accidents %>% 
  pull(Start_Time) %>% 
  min()                 #Max Date "2023-03-31"

us_accidents_17_22 <- us_accidents %>% 
  filter(between(Start_Time,as.Date("2017-01-01"),as.Date("2022-12-31")))

str(us_accidents_17_22)

#Group the reasons in 1 column
us_accidents_17_22 <- us_accidents_17_22 %>%
  mutate(Reason = pmap_chr(list(Bump, Crossing, Give_Way, Junction, No_Exit, Railway, 
                                Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop),
                           function(Bump, Crossing, Give_Way, Junction, No_Exit, Railway, 
                                    Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop) {
                             reasons <- c()
                             if (Bump == "True") reasons <- c(reasons, "Bump")
                             if (Crossing == "True") reasons <- c(reasons, "Crossing")
                             if (Give_Way == "True") reasons <- c(reasons, "Give_Way")
                             if (Junction == "True") reasons <- c(reasons, "Junction")
                             if (No_Exit == "True") reasons <- c(reasons, "No_Exit")
                             if (Railway == "True") reasons <- c(reasons, "Railway")
                             if (Roundabout == "True") reasons <- c(reasons, "Roundabout")
                             if (Station == "True") reasons <- c(reasons, "Station")
                             if (Stop == "True") reasons <- c(reasons, "Stop")
                             if (Traffic_Calming == "True") reasons <- c(reasons, "Traffic_Calming")
                             if (Traffic_Signal == "True") reasons <- c(reasons, "Traffic_Signal")
                             if (Turning_Loop == "True") reasons <- c(reasons, "Turning_Loop")
                             if (length(reasons) == 0) return(NA_character_) else return(paste(reasons, collapse = ", "))
                           }))       

us_accidents_17_22 <- us_accidents_17_22 %>% 
  select(-c(Crossing,Give_Way,Junction,No_Exit,Railway,Roundabout,Station,Stop,Traffic_Calming,Traffic_Signal,Turning_Loop))

us_accidents_17_22 <- us_accidents_17_22 %>% 
  select(-c(Zipcode,Description))

#Amount of accidents by state
#Top 10 States with least accidents
us_accidents_17_22 %>% 
  group_by(State) %>% 
  summarise(Amount = n()) %>% 
  select(State, Amount) %>% 
  arrange(Amount) %>% 
  mutate(prc = Amount/sum(Amount)) %>% 
  head(10)

#Top 10 States with most accidents
us_accidents_17_22 %>% 
  group_by(State) %>% 
  summarise(Amount = n()) %>% 
  select(State, Amount) %>% 
  arrange(-Amount) %>% 
  mutate(prc = Amount/sum(Amount)) %>% 
  head(20)

table(us_accidents_17_22$State)  

top_States <- us_accidents_17_22 %>% 
  group_by(State) %>% 
  summarise(Amount = n()) %>% 
  arrange(-Amount) %>% 
  mutate(prc = Amount/sum(Amount)) %>% 
  head(20)

top_accidents <- us_accidents_17_22 %>%
  filter(State %in% top_States$State)

table(top_accidents$State)
str(top_accidents)

top_accidents <- top_accidents %>% 
  mutate(City = factor(City),
         State = factor(State),
         Weather_Condition = factor(Weather_Condition),
         Reason = factor(Reason))

table(top_accidents$Reason)

top_accidents <- top_accidents %>% 
  select(-Bump)

train_table <- train_table %>% 
  select(-Bump)

test_table <- test_table %>% 
  select(-Bump)

top_reasons <- top_accidents %>% 
  separate_rows(Reason, sep=",") %>%
  mutate(Reason = trimws(Reason)) %>% 
  group_by(Reason) %>% 
  filter(Reason != is.na(Reason)) %>% 
  summarise(NumAcc = n())%>% 
  arrange(-NumAcc) %>% 
  head (20)

train_table <- top_accidents %>% 
  filter(!is.na(Reason))

test_table <- top_accidents %>% 
  filter(is.na(Reason))

#############################################################################
#Visualization

rm(us_accidents)
rm(us_accidents_17_22)

str(train_table)
plot(train_table$State,train_table$Severity)

ggplot(train_table[1:100,]) +
  geom_point(aes(x = State, y = Severity))

#Avg Severity per State
top_accidents %>%
  group_by(State) %>%
  summarise(Average_Severity = mean(Severity, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(State,Average_Severity), y = Average_Severity)) +
  geom_bar(stat = "identity", fill = "steelblue")

#Amount per State
top_accidents %>% 
  group_by(State) %>% 
  summarise(Amount = n()) %>% 
  ggplot(aes(x = reorder(State,Amount), y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Amount), vjust = -0.5, color = "black") +  # Añadir el total sobre cada barra
  labs(title = "Total Number of Accidents by State",  # Título del gráfico
       x = "State",  # Etiqueta del eje X
       y = "Number of Accidents") +  # Etiqueta del eje Y
  theme_minimal()

#Amount per Severity
top_accidents %>% 
  group_by(Severity) %>% 
  summarise(Amount = n()) %>% 
  ggplot(aes(x = (Severity), y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") 

#Amount per state and reason
train_table %>% 
  group_by(State, Reason) %>% 
  summarise(Amount = n()) %>% 
  ggplot(aes(x = reorder(State,Amount), y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  facet_wrap(~ Reason)

#Amount per severity and reason
train_table %>% 
  group_by(Severity, Reason) %>% 
  summarise(Amount = n()) %>% 
  ggplot(aes(x = Severity, y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  facet_wrap(~ Reason)

#Amount of accidents with reasons through time
train_table %>% 
  group_by(Start_Time) %>% 
  summarise(Amount = n()) %>%
  ggplot(aes(x = Start_Time, y = Amount)) +
  geom_point()

#Amount of accidents with reasons through time per month
train_table %>%
  mutate(Month_Year = floor_date(Start_Time, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Amount = n(), .groups = "drop") %>%  
  ggplot(aes(x = Month_Year, y = Amount)) +      
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red")

#Amount of accidents without reasons through time per month
test_table %>%
  mutate(Month_Year = floor_date(Start_Time, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Amount = n(), .groups = "drop") %>%  
  ggplot(aes(x = Month_Year, y = Amount)) +      
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red")

#Combined plot (Full Join)
combined_table <- train_table %>%
  mutate(Reason = "With reasons") %>%
  mutate(Month_Year = floor_date(Start_Time, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Amount = n(), .groups = "drop") %>%
  rename(Accidents_With_Reasons = Amount) %>%
  select(Month_Year, Accidents_With_Reasons) %>%
  full_join(
    test_table %>%
      mutate(Reason = "Without reasons") %>%
      mutate(Month_Year = floor_date(Start_Time, "month")) %>%
      group_by(Month_Year) %>%
      summarise(Amount = n(), .groups = "drop") %>%
      rename(Accidents_Without_Reasons = Amount),
    by = "Month_Year"
  )

combined_table %>%
  pivot_longer(cols = -Month_Year, names_to = "Reason", values_to = "Amount") %>%
  ggplot(aes(x = Month_Year, y = Amount, color = Reason)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

#By humidity
top_accidents %>%
  group_by(Humidity...) %>%
  summarise(Amount = n()) %>%  
  ggplot(aes(x = Humidity..., y = Amount)) +      
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red")

#By Visibility
top_accidents %>%
  group_by(Visibility.mi.) %>%
  summarise(Amount = n()) %>%  
  ggplot(aes(x = Visibility.mi., y = Amount)) +      
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red")

#By temperature
top_accidents %>%
  group_by(Temperature.F.) %>%
  summarise(Amount = n()) %>%  
  ggplot(aes(x = Temperature.F., y = Amount)) +      
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red")

#By Weather
table(top_accidents$Weather_Condition)

top_accidents %>%
  separate_rows(Weather_Condition, sep="/") %>%
  mutate(Weather_Condition = trimws(Weather_Condition)) %>% 
  group_by(Weather_Condition) %>%
  summarise(Amount = n(), .groups = "drop") %>%
  arrange(-Amount) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(Weather_Condition, -Amount), y = Amount)) +      
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Amount), vjust = -0.5, color = "black") +  
  labs(title = "Top 10 Weather Conditions Causing Accidents",  
       x = "Weather Condition",  
       y = "Number of Accidents") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(test_table, file = "test_table.csv")
write.csv(train_table, file = "train_table.csv")
write.csv(top_accidents, file = "top_accidents.csv")

################################################################################
#Lineal Regretion / Severity

model_1 <- lm(Severity ~ Weather_Condition, data = top_accidents)
summary(model_1) #R-squared:  0.009592
rm(model_1)

model_2 <- lm(Severity ~ State, data = top_accidents)
summary(model_2) #R-squared:  0.03782
rm(model_2)

model_3 <- lm(Severity ~ Reason, data = top_accidents)
summary(model_3) #R-squared:  0.03413
rm(model_3)

model_4 <- lm(Severity ~ Humidity..., data = top_accidents)
summary(model_4)
rm(model_4)

model_5 <- lm(Severity ~ Temperature.F., data = top_accidents)
summary(model_5)
rm(model_5)

model_6 <- lm(Severity ~ Visibility.mi., data = top_accidents)
summary(model_6)
rm(model_6)

model_7 <- lm(Severity ~ Wind_Speed.mph., data = top_accidents)
summary(model_7)
rm(model_7)

model_8 <- lm(Severity ~ Precipitation.in., data = top_accidents)
summary(model_8)
rm(model_8)

#No correlation between Severity and other Variables

###############################################################################
#Map Description

ggplot(top_accidents, aes(x = Start_Lng, y = Start_Lat)) +
  geom_point(color = "blue") +
  theme_minimal()

leaflet() %>%
  addTiles() %>%
  addMarkers(lat = Maploc$Start_Lat,
             lng = Maploc$Start_Lng,
             popup = Maploc$Description)

###############################################################################
