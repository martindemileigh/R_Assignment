# Demi-Leigh, Luanne and Luvuyo
# This is a script containing some of teh figures that can be generated from the Ecofootprint data
# 25 June 2018

# Load libraries ----------------------------------------------------------

library(readr)
library(tidyverse)
library(dplyr)


# Load Data ---------------------------------------------------------------

countries <- read_csv("countries_ecological_footprint.csv")
View(countries)


# Potenitial figures ------------------------------------------------------


# Top 20 countries with the highest EcoFootprint in Africa
 ecoFoot <- countries %>% 
           group_by(Country) %>% 
           filter(Region == "Africa") %>% 
           summarise(EcoFootprintMedian = median(`Total Ecological Footprint`) ,
                                              na.rm = TRUE) %>% 
           mutate(Country = reorder(Country, EcoFootprintMedian)) %>% 
           arrange(desc(EcoFootprintMedian)) %>%
           head(20)  
  
  ecoFoot %>% 
  ggplot(aes(x = Country, y = EcoFootprintMedian))+
  geom_bar(stat = 'identity')+
  labs(x = 'Countries',y = 'Eco Footprint', title = 'Countries With Highest EcoFootprint')+
  coord_flip()+ theme(legend.position = "")

# Top 20 countries with the highest Biocapacities in Africa

biocap <- countries %>% 
           group_by(Country) %>% 
           filter(Region == "Africa") %>% 
           summarise(BiocapacityMedian = median(`Total Biocapacity`) , na.rm = TRUE) %>% 
           mutate(Country = reorder(Country, BiocapacityMedian)) %>% 
           arrange(desc(BiocapacityMedian)) %>%
           head(20)  
  
biocap %>% 
  ggplot(aes(x = Country, y = BiocapacityMedian))+
  geom_bar(stat = 'identity')+
  labs(x = 'Countries',y = 'Biocapacities', title = 'Countries With Highest Biocapacities')+
  coord_flip()+ theme(legend.position = "") 
  
# EcoFootprint vs Human Development Index in Africa

 afrieco <- countries %>% 
           group_by(HDI, Country) %>% 
           filter(Region == "Africa") %>% 
           summarise(EcofootprintMedian = median(`Total Ecological Footprint`),
                                          PopMedian = median(`Population (millions)`))  
  
 ggplot(afrieco, aes(x = HDI, y = EcofootprintMedian, colour = Country, size = PopMedian))+
  geom_point(alpha = 0.7) + ylab("Eco-Footprint") + labs(title = "Eco-Footprint Vs Human Development Index")
  
  
# Biocapacity vs Human Development Index in Africa
 
 afribio <- countries %>% 
           group_by(HDI, Country) %>% 
           filter(Region == "Africa") %>% 
           summarise(BiocapacityMedian = median(`Total Biocapacity`),
                                          MedianPop = median(`Population (millions)`))  

 ggplot(afribio, aes(x = HDI, y = BiocapacityMedian, colour = Country, size = MedianPop))+
   geom_point(alpha = 0.7) +  ylab("Biocapacity") + labs(title = "Biocapacity Vs Human Development Index")

 # Distribution of Country and Different Ecofootprint variables
 # Use HDI to determine Development status of countries
 # Does a High HDI = High EcoFootprint?
   #Country with Highest HDI = Mauritius = 0.77
    #Country with Lowest HDI = Niger = 0.34
 
 
  mauri <- countries %>% 
           filter(Country == "Mauritius")

  View(mauri)
 
  ggplot(mauri, aes(x = HDI ,`Total Ecological Footprint`))+ 
    geom_point(alpha = 0.7)+
    facet_wrap(~Footprint)+
    scale_y_log10()+
    labs(title = "Distribution of Each Eco-footprint")+ 
    theme(legend.position = "bottom") 
  
  niger <- countries %>% 
    filter(Country == "Niger")  
  View(niger)
  
  ggplot(niger, aes(x = HDI ,`Total Ecological Footprint`))+ 
    geom_point(alpha = 0.7)+
    facet_wrap(~Footprint)+
    scale_y_log10()+
    labs(title = "Distribution of Each Eco-footprint")+ 
    theme(legend.position = "bottom") 
  
