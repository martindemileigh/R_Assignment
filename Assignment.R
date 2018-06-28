#Luanne Thomas
#26 June 2016
#Statistical analysis of data


# Libraries  --------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(dbplyr)
library(corrplot)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(plotly)

# Load Data ---------------------------------------------------------------

eco <- read.csv("Ecological.txt")

africa <- eco %>% 
  filter(Country %in% c("South Africa", "Botswana", "Libya", "Gabon", "Congo", "Malawi", "Mauritius",
                        "Mauritius", "Egypt", "Togo", "Uganda"))                             

# Country and Total ecological footprint ----------------------------------

africa %>% 
  group_by(Country) %>% 
  summarise(EcoFootprintMean = mean(Total.Ecological.Footprint, 
                                    na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(Country = reorder(Country,EcoFootprintMean)) %>%
  arrange(desc(EcoFootprintMean)) %>%
  ggplot(aes(x = Country, y = EcoFootprintMean)) +
  geom_bar(stat = "identity", fill = "#3288BD")+
  geom_text(aes(x = Country, y = 1, label = paste(' ',sep="")),
            hjust=0, vjust=.5, size = 4, colour = "black",
            fontface = 'italic') +
  labs(x = 'Countries', 
       y = 'Eco Footprint Mean', 
       title = 'Countries With Highest EcoFootprint') +
  coord_flip() + theme(legend.position = " ") 



# Country and Total Biocapacity -------------------------------------------

africa %>%
  group_by(Country) %>%
  summarise(BiocapacityMedian = median(Total.Biocapacity, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Country = reorder(Country,BiocapacityMedian)) %>%
  arrange(desc(BiocapacityMedian)) %>%
  ggplot(aes(x = Country,y = BiocapacityMedian)) +
  geom_bar(stat='identity', fill = "#3288BD") +
  geom_text(aes(x = Country, y = 1, label = paste0("(",BiocapacityMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = 'Countries', 
       y = 'Biocapacities', 
       title = 'Countries With Highest Biocapacities') +
  coord_flip()  + theme(legend.position = "") 


# HDI, Total Ecological Footprint and Population --------------------------

by_count <- africa %>% 
  group_by(HDI, Country) %>% 
  summarise(EcofooMedian = median(Total.Ecological.Footprint),
            PopMedian = median(Population..millions.)) 

# Plot the change in EcofootprintMedian in each Region over HDI

ggplot(by_count, aes(x = HDI, y = EcofooMedian, color = Country, size = PopMedian)) +
  geom_point(alpha = 0.7)  + ylab("Eco-Footprint") + labs(title="Eco-Footprint Vs Human Development Index") + 
  xlab("Human Development Index") + ylab("Ecological Footprint Median")


# HDI, Biocapacity and Population -----------------------------------------

by_count2 <- africa %>% 
  group_by(HDI, Country) %>%
  summarise(BiocapacityMedian = median(Total.Biocapacity),
            MedianPop = median(Population..millions.))

ggplot(by_count2, aes(x = HDI, y = BiocapacityMedian, color = Country, size = MedianPop)) +
  geom_point(alpha=0.7) + 
  ylab("Biocapacity") + labs(title="Biocapacity Vs Human Development Index") 
+ scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E" ,"#E6AB02" ,"#A6761D")) + xlab("") + ylab("")


# Cropland footprint ----------------------------------------------

crop <- ggplot(africa, aes(x = reorder(Country, -Cropland.Footprint),y = Cropland.Footprint)) +
  geom_bar(stat='identity', fill = "#6a51a3") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Cropland.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = ' ', 
       y = 'Cropland Footprint') +
  coord_flip()  + theme(legend.position = "") 

# Carbon Footprint --------------------------------------------------------

Carbon <- ggplot(africa, aes(x = reorder(Country, -Carbon.Footprint),y = Carbon.Footprint)) +
  geom_bar(stat='identity', fill = "#8c96c6") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Carbon.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = ' ', 
       y = 'Carbon Footprint') +
  coord_flip()  + theme(legend.position = "")


# Fish Footprint ----------------------------------------------------------

Fish <- ggplot(africa, aes(x = reorder(Country, -Fish.Footprint),y = Fish.Footprint)) +
  geom_bar(stat='identity', fill = "#9ebcda") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Carbon.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Fish Footprint') +
  coord_flip()  + theme(legend.position = "")


# Forest Footprint --------------------------------------------------------

Forest <- ggplot(africa, aes(x = reorder(Country, -Forest.Footprint),y = Forest.Footprint)) +
  geom_bar(stat='identity', fill = "#8c6bb1") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Forest.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Forest Footprint') +
  coord_flip()  + theme(legend.position = "")


# Grazing Footprint -------------------------------------------------------

Grazing <- ggplot(africa, aes(x = reorder(Country, -Grazing.Footprint),y = Grazing.Footprint)) +
  geom_bar(stat='identity', fill = "#9e9ac8") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Grazing.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Grazing Footprint') +
  coord_flip()  + theme(legend.position = "")

ecoprint <- ggarrange(crop, Forest, Carbon, Grazing, Fish, labels = c("A", "B", "C", "D", "E")) 

annotate_figure(ecoprint,
                top = text_grob("Distribution of each Eco-print", color = "Black", face = "bold", size = 14),
                left = text_grob("Country", color = "Black", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold")

