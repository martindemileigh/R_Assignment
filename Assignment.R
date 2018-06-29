#Luanne Thomas, Demi-Lee Martin & Luvuyo Kani
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
library(pgirmess)
library(reshape2)

# Load Data ---------------------------------------------------------------

eco <- read.csv("Ecological.txt")

africa <- eco %>%
  mutate(Development_status = ifelse(HDI >= 0.7, "Developed", 
                                     ifelse(HDI >= 0.5 & HDI < 0.69, "Developing", "Underdeveloped"))) %>%
  
  filter(Country %in% c("South Africa", "Botswana", "Libya", "Gabon", "Congo", "Malawi", "Mauritius",
                        "Mauritius", "Egypt", "Togo", "Uganda"))


           

# Country and Total ecological footprint ----------------------------------

africa %>% 
  group_by(Country, Development_status) %>% 
  summarise(EcoFootprintMean = mean(Total.Ecological.Footprint, 
                                    na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(Country = reorder(Country,EcoFootprintMean)) %>%
  arrange(desc(EcoFootprintMean)) %>%
  ggplot(aes(x = Country, y = EcoFootprintMean, fill = Development_status)) +
  geom_bar(stat="identity", show.legend = T)+
  geom_text(aes(x = Country, y = 1, label = paste(' ',sep="")),
            hjust=0, vjust=.5, size = 4, colour = "black",
            fontface = 'italic') +
  labs(x = 'Countries', 
       y = 'Eco Footprint Mean', 
       title = 'Countries With Highest EcoFootprint') +
  coord_flip() + theme(legend.position = "right") 



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
  ylab("Biocapacity") + labs(title="Biocapacity Vs Human Development Index")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E" ,"#E6AB02" ,"#A6761D", "#FB8072", "#BEBADA")) + 
  xlab("Human Development Index") + ylab("Biocapacity")


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

# Stats -------------------------------------------------------------------


# Assumptions -------------------------------------------------------------

#Normality

shapiro.test(africa$Total.Ecological.Footprint) #normal distributed

shapiro.test(africa$Population..millions.) #normal distributed

shapiro.test(africa$Total.Biocapacity) #not normal distributed

#Homoscedasticity

var(africa$Total.Ecological.Footprint)

var(africa$Population..millions.)

#(Total.Biocapacity vs Total.Ecological.Footprint)

africa_sub <- africa %>% 
  select(Total.Biocapacity, Total.Ecological.Footprint)

#Kendall 

cor.test(x = africa$Total.Biocapacity, africa$Total.Ecological.Footprint,
         use = "everything", method = "pearson")

africa_pearson <- cor(africa_sub)  

#The simple linear regression 
africa_lm <- lm(Total.Biocapacity ~ Total.Ecological.Footprint, data = africa)

summary(africa_lm)  

#linear regression graph

slope <- round(africa_lm$coef[2], 3)

p.val <- round(coefficients(summary(africa_lm))[2, 4], 3)

r2 <- round(summary(africa_lm)$r.squared, 3)

ggplot(data = africa, aes(x = Total.Biocapacity, y = Total.Ecological.Footprint)) +
  geom_point() +
  annotate("text", x = 0, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "",
       subtitle = "Linear regression",
       x = "Total Biocapacity",
       y = "Total Ecological Footprint")  

#(Total.Biocapacity vs Population..millions.)

africa_sub1 <- africa %>% 
  select(Total.Biocapacity, Population..millions.)

#Kendall 

cor.test(x = africa$Total.Biocapacity, africa$Population..millions.,
         use = "everything", method = "kendall")

africa_kendall <- cor(africa_sub1)  

#The simple linear regression 
africa_lm1 <- lm(Total.Biocapacity ~ Population..millions., data = africa)

summary(africa_lm1)  

#linear regression graph

slope_1 <- round(africa_lm1$coef[2], 3)

p.val_1 <- round(coefficients(summary(africa_lm1))[2, 4], 3)

r2_1 <- round(summary(africa_lm1)$r.squared, 3)

ggplot(data = africa, aes(x = Population..millions., y = Total.Biocapacity)) +
  geom_point() +
  annotate("text", x = 55, y = 21, label = paste0("slope == ", slope_1, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 55, y = 19., label = paste0("italic(p) < ", p.val_1), parse = TRUE, hjust = 0) +
  annotate("text", x = 55, y = 17, label = paste0("italic(r)^2 == ", r2_1), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "",
       subtitle = "Linear regression",
       x = "Population millions.",
       y = "Total.Biocapacity")  

#(Total.Ecological.Footprint VS Population..millions.)

africa_sub2 <- africa %>% 
  select(Total.Ecological.Footprint, Population..millions.)

#Kendall 

cor.test(x = africa$Population..millions., africa$Total.Ecological.Footprint,
         use = "everything", method = "kendall")

eco_kendall <- cor(eco_sub2)  

#The simple linear regression 
eco_lm2 <- lm(Total.Ecological.Footprint ~ Population..millions., data = eco)

summary(eco_lm2)  

#linear regression graph

slope_2 <- round(eco_lm$coef[2], 3)

p.val_2 <- round(coefficients(summary(eco_lm))[2, 4], 3)

r2_2 <- round(summary(eco_lm)$r.squared, 3)

ggplot(data = africa, aes(x = Population..millions., y = Total.Ecological.Footprint)) +
  geom_point() +
  annotate("text", x = 0, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "",
       subtitle = "Linear regression",
       x = "Population millions.",
       y = "Total Ecological Footprint")  

#correlation

cor.test(africa$Total.Ecological.Footprint, africa$Total.Biocapacity)

cor.test(africa$Total.Ecological.Footprint, africa$Population..millions.)

cor.test(africa$Population..millions., africa$Total.Biocapacity)

afr_sub <- africa%>% 
  select(Total.Biocapacity:Total.Ecological.Footprint)

afr_cor <- cor(afr_sub)

afr_cor

corrplot(afr_cor, method = "circle")
