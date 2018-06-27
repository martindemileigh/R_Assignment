# Demi-Leigh Martin
# THis is a script that will be used to test whether or not my project folder
   # is linked to the Github repository
# 20 June 2018

library(tidyverse)
library(ggplot2)


eco <- read_delim("Ecological footprint/eco.csv", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

View(eco)

africa <- eco %>% 
  filter(Region == "Africa") %>% 
  mutate(Development_status = ifelse(HDI >= 0.7, "Developed",
                                    ifelse(HDI >= 0.5 & HDI < 0.69, "Developing", 
                                           "underdeveloped")))

View(africa)

eco %>%
  group_by(Country) %>%
  summarise(EcoFootprintMean = mean(`Total Ecological Footprint`, 
                                        na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Country = reorder(Country,EcoFootprintMean)) %>%
  arrange(desc(EcoFootprintMean)) %>%
  head(15) %>%
  
ggplot(aes(x = Country,y = EcoFootprintMean)) +
  geom_bar(stat='identity', fill = "#3288BD") +
  labs(x = 'Countries', 
       y = 'Eco Footprint', 
       title = 'Countries With Highest EcoFootprint')+  
  coord_flip() + theme(legend.position = "") 
  
  
# HDI Region Ecoprint and POpulation
by_region <- eco %>%
  group_by(HDI, Region) %>%
  summarize(EcofootprintMedian = median(`Total Ecological Footprint`),
            PopMedian = median(`Population (millions)`))

# Plot the change in EcofootprintMedian in each Region over HDI
ggplot(by_region, aes(x = HDI, y = EcofootprintMedian,
                      colour = Region, size = PopMedian)) +
  geom_point(alpha=0.7) + ylab("Eco-Footprint") + 
  labs(title="Eco-Footprint Vs Human Development Index") + 
  scale_colour_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                 "#E7298A", "#66A61E" ,"#E6AB02" ,
                                 "#A6761D"))+ 
  xlab("") +
  ylab("")
