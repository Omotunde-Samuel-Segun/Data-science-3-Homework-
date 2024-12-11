# Data-science-3-Homework-
#Omotunde S. Segun
library(tidyverse)
library(broom)
#load data set
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
#Data exploration
str(clim)
#The altitude and p_mean are characters instead of integer
clim_frar <- clim %>% mutate(p_mean = as.numeric(p_mean),
                        altitude = as.numeric(altitude))
#above line of code introduces na to the data_frame
sum(is.na(clim_frar))
#there are 6 na
#remove na
clim_frar <- na.omit(clim_frar)
#left with 34 observations

#plot maps to visualiza the latitude, logitude and altitude according to each locations
install.packages("maps")

# Load world map data
library(maps)
# Plot the map with stations
# Extracting The Map Of France 
clim_frar_map <- map_data("world", region = "France")
 
# Plotting the stations
library(ggplot2)
map_frar<- ggplot() +
  geom_polygon(data = clim_frar_map, aes(x = long, y = lat, group = group), fill = "yellow", color = "black") +
  geom_point(data = clim_frar, aes(x = lon, y = lat), color = "blue", size = 2) +
  labs(title = "Climate Stations in France", x = "Longitude", y = "Latitude") +
  theme_minimal()
#ggsave("Climate_Stations_Map.png", plot = map_frar, width = 10, height = 8, dpi = 300)  
#ggsave("Climate_Stations_Map.pdf", plot = map_frar, width = 10, height = 

#Exercise 1
Mean_Temp_model<- lm(t_mean ~ altitude + lon + lat, data = clim_frar)
summary(Mean_Temp_model)
#result and interpretation
#Coefficients:
######      #Estimate Std. Error t value Pr(>|t|)##    
#(Intercept) 37.2089207  2.9656572  12.547 8.90e-13 ***
#altitude    -0.0064681  0.0009592  -6.743 3.06e-07 ***
#lon          0.0370152  0.0475819   0.778    0.443    
#lat         -0.5332257  0.0625716  -8.522 3.90e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7683 on 27 degrees of freedom
#Multiple R-squared:  0.8152,	Adjusted R-squared:  0.7947 
#F-statistic:  39.7 on 3 and 27 DF,  p-value: 4.885e-10


#interpretation
#intercept = 37.2 degrees, this is the anual mean temperature for 0 altitude, latitude and longitude
#there was a 0.0065 decrease in temperature for a unit increase in altitude at constant latitude and longitude
#there was an increase of 0.037 in temperature for a unit increase in longitude at equal altitude and latitude
#there was a decrease in temperature by a factor of 0.53 per unit rise in latitude when longitude and altitude are of no effect
#The model explains 81.5 % if the variation in latitude, longitude and altitude


#Exercise 2:
#the non significant variable from the result is longitude which will be removed from the model because the p value is not significant suggestinf that the probability of obseving a mean annual temperature difference of 0.037 or more extreme values is not significant at p<0.05, which means the result could have been observed by chance
#hence new model

Mean_Temp_model_reduced<- lm(t_mean ~ altitude  + lat, data = clim_frar)
summary(Mean_Temp_model_reduced)

#result displayed below with interpretation
#Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 38.0696666  2.7320520  13.934 4.06e-14 ***
#altitude    -0.0063864  0.0009466  -6.746 2.52e-07 ***
#lat         -0.5496529  0.0584839  -9.398 3.72e-10 ***

#Interpretation
#intercept = 38.06 degrees, this is the anual mean temperature for 0 altitude and latitude

#there was a 0.0064 decrease in temperature for a unit increase in altitude at constant latitude 

#there was a decrease in temperature by a factor of 0.55 per unit rise in latitude when longitude is of no effect
#The model explains 81.1 % if the variation in latitude and altitude

#Excluding non significant variable and predicting temperature for mont_ventoux and pic-du-midi

new_stations <- data.frame(
#stations 
  stations = c("Mont_Ventoux", "Pic-du-Midi"),
#Altitudes for Mont-Ventoux and Pic-du-Midi 
  altitude = c(1212, 2860),
#Latitudes for Mont-Ventoux and Pic-du-Midi
  lat = c(44.16, 42.93)  
)

#the linear model has been done above 

#predictions based on above model after exclusion of longitude

model_new_stat <- predict(Mean_Temp_model_reduced, newdata = new_stations, interval = "confidence")

model_new_stat

#According to the predictions, the mean temperature for both Mont_Ventoux and Pic-du-midi are 6.05 and -3.79   respectively which differe from the measured one on the table, which has value of 3.6 and -1.2 respectively. 

## Below is the result and its confidence interval

##        fit       lwr      upr
## 1  6.056723  3.974229 8.139217
## 2 -3.791931 -9.040129 1.456266

#EXERCISE 3: Evaluate model result by

#a 3D scatter plot

install.packages("plotly")
library(plotly)

Climate_Stations_3D<- plot_ly(
  data = clim_frar, x = ~altitude, y = ~lat, z = ~t_mean, 
  type = "scatter3d", mode = "markers", 
  marker = list(size = 5, color = ~t_mean, colorscale = "Viridis")
)
# dev.off()

#b Summary of output

#the summary of output has been done and interpretted above
#the model explain 81.1% of the variation in latitude and altitude

### Call:
## lm(formula = t_mean ~ altitude + lat, data = clim_frar)

# Residuals:
#    Min       1Q   Median       3Q      Max 
# -1.80839 -0.27488  0.01524  0.35388  2.70691 

## Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 38.0696666  2.7320520  13.934 4.06e-14 ***
altitude    -0.0063864  0.0009466  -6.746 2.52e-07 ***
lat         -0.5496529  0.0584839  -9.398 3.72e-10 ***
---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.7628 on 28 degrees of freedom
# Multiple R-squared:  0.8111,	Adjusted R-squared:  0.7976 
# F-statistic:  60.1 on 2 and 28 DF,  p-value: 7.389e-11
