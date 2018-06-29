#Luanne Thomas, Demi-Lee Martin & Luvuyo Kani
#26 June 2016
#Statistical analysis of data


# Libraries  --------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)
library(corrplot)
library(RColorBrewer)

# Load Data ---------------------------------------------------------------

eco <- read.csv("Ecological.txt")

africa <- eco %>%
  filter(Country %in% c("South Africa", "Botswana", "Kenya", "Gabon", "Congo", "Malawi", "Mauritius", "Egypt", "Togo", "Uganda")) %>% 
mutate(Development_status = ifelse(HDI >= 0.7, "Developed", 
                                     ifelse(HDI >= 0.5 & HDI < 0.69, "Developing", "Underdeveloped")))
  

# Total ecological footprint vs total biocapacity -------------------------



ggplot(africa, aes(x = Total.Ecological.Footprint, y = Total.Biocapacity, colour = Country)) +
geom_point(alpha = 1, size = 3) +
  xlab("Total Ecological Footprint") + ylab("Total Biocapacity") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
# Country and Total ecological footprint ----------------------------------

africa %>% 
  group_by(Country) %>% 
  summarise(EcoFootprintMean = mean(Total.Ecological.Footprint, 
                                    na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(Country = reorder(Country,EcoFootprintMean)) %>%
  arrange(desc(EcoFootprintMean)) %>%
  ggplot(aes(x = Country, y = EcoFootprintMean)) +
  geom_bar(stat = "identity", fill = "#3288BD") +
  geom_text(aes(x = Country, y = 1, label = paste("(" ,EcoFootprintMean,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = "black",
            fontface = 'italic') +
  labs(x = 'Country', 
       y = 'Total Ecological Footprint Mean') +
  coord_flip() + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Country and Total Biocapacity -------------------------------------------

africa %>%
  group_by(Country) %>%
  summarise(BiocapacityMean = mean(Total.Biocapacity, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Country = reorder(Country,BiocapacityMean)) %>%
  arrange(desc(BiocapacityMean)) %>%
  ggplot(aes(x = Country,y = BiocapacityMean)) +
  geom_bar(stat='identity', fill = "#3288BD") +
  geom_text(aes(x = Country, y = 1, label = paste0("(",BiocapacityMean,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = 'Country', 
       y = 'Total Biocapacity mean') +
    coord_flip()  + theme(legend.position = "none") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Biocapacity Def ---------------------------------------------------------

ggplot(africa, aes(x = reorder(Country, -Biocapacity.Deficit.or.Reserve), y = Biocapacity.Deficit.or.Reserve)) +
  geom_bar(stat = "identity", fill = "#3288BD") +
  geom_text(aes(x = Country, y = 1, label = paste("(" ,Biocapacity.Deficit.or.Reserve,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = "black",
            fontface = 'italic') +
  labs(x = 'Country', 
       y = 'Biocapacity (Deficit or Reserve)') +
  coord_flip() + theme(legend.position = " ") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# HDI, Total Ecological Footprint and Population --------------------------

africa %>% 
  group_by(HDI, Country) %>% 
  summarise(EcofooMean = mean(Total.Ecological.Footprint),
            PopMean = mean(Population..millions.)) %>% 
  ggplot(aes(x = HDI, y = EcofooMean, color = Country, size = PopMean)) +
  geom_point(alpha = 0.7)  + ylab("Eco-Footprint") + 
  xlab("Human Development Index") + ylab("Ecological Footprint Mean")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# HDI, Biocapacity and Population -----------------------------------------

africa %>% 
  group_by(HDI, Country) %>%
  summarise(BiocapacityMean = mean(Total.Biocapacity),
            MeanPop = mean(Population..millions.)) %>% 
ggplot(aes(x = HDI, y = BiocapacityMean, color = Country, size = MeanPop)) +
  geom_point(alpha=0.7) + 
  ylab("Biocapacity") + labs(caption = "Figure 5. The relationship between the total biocapacity  and the human development index")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E" ,"#E6AB02" ,"#A6761D", "#FB8072", "#BEBADA", "#6a51a3")) + 
  xlab("Human Development Index") + ylab("Biocapacity") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Cropland footprint ----------------------------------------------

crop <- ggplot(africa, aes(x = reorder(Country, -Cropland.Footprint),y = Cropland.Footprint)) +
  geom_bar(stat='identity', fill = "#6a51a3") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Cropland.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = ' ', 
       y = 'Cropland Footprint') +
  coord_flip()  + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Carbon Footprint --------------------------------------------------------

Carbon <- ggplot(africa, aes(x = reorder(Country, -Carbon.Footprint),y = Carbon.Footprint)) +
  geom_bar(stat='identity', fill = "#8c96c6") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Carbon.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = ' ', 
       y = 'Carbon Footprint') +
  coord_flip()  + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Fish Footprint ----------------------------------------------------------

Fish <- ggplot(africa, aes(x = reorder(Country, -Fish.Footprint),y = Fish.Footprint)) +
  geom_bar(stat='identity', fill = "#9ebcda") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Carbon.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Fish Footprint') +
  coord_flip()  + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Forest Footprint --------------------------------------------------------

Forest <- ggplot(africa, aes(x = reorder(Country, -Forest.Footprint),y = Forest.Footprint)) +
  geom_bar(stat='identity', fill = "#8c6bb1") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Forest.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Forest Footprint') +
  coord_flip()  + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



# Grazing Footprint -------------------------------------------------------

Grazing <- ggplot(africa, aes(x = reorder(Country, -Grazing.Footprint),y = Grazing.Footprint)) +
  geom_bar(stat='identity', fill = "#9e9ac8") +
  geom_text(aes(x = Country, y = 0.15, label = paste0("(",Grazing.Footprint,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'italic') +
  labs(x = '', 
       y = 'Grazing Footprint') +
  coord_flip()  + theme(legend.position = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ecoprint <- ggarrange(crop, Forest, Carbon, Grazing, Fish, labels = c("A", "B", "C", "D", "E")) 

annotate_figure(ecoprint, left = text_grob("Country", color = "Black", rot = 90),
                fig.lab = "", fig.lab.face = "bold")

# Stats -------------------------------------------------------------------


# Assumptions -------------------------------------------------------------

#Normality

shapiro.test(africa$Total.Ecological.Footprint) #normal distributed

shapiro.test(africa$Population..millions.) #not normal distributed

shapiro.test(africa$Total.Biocapacity) #not normal distributed

#Homoscedasticity

var(africa$Total.Ecological.Footprint)

#(Total.Biocapacity vs Total.Ecological.Footprint)

#Kendall 
cor.test(x = africa$Total.Biocapacity, africa$Total.Ecological.Footprint,
                use = "everything", method = "kendall")

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
  labs(caption = "Figure 7. Linear regression model on the total biocapacity and the total ecological footprint of each country",
       x = "Total Biocapacity",
       y = "Total Ecological Footprint") +
theme_bw() + theme(plot.caption = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#(Total.Biocapacity vs Population..millions.)

#Kendall 

cor.test(x = africa$Total.Biocapacity, africa$Population..millions.,
          use = "everything", method = "kendall")  

#The simple linear regression 

africa_lm1 <- lm(Total.Biocapacity ~ Population..millions., data = africa)

summary(africa_lm1)  

#(Total.Ecological.Footprint VS Population..millions.)

#Kendall 

cor.test(x = africa$Population..millions., africa$Total.Ecological.Footprint,
           use = "everything", method = "kendall")

#The simple linear regression 
africa_lm2 <- lm(Total.Ecological.Footprint ~ Population..millions., data = africa)

summary(africa_lm2)  

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
  labs(caption = "Figure 8. Linear regression model on the population and the total biocapacity",
       x = "Population (millions)",
       y = "Total.Biocapacity") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#linear regression graph

slope_2 <- round(africa_lm2$coef[2], 3)

p.val_2 <- round(coefficients(summary(africa_lm2))[2, 4], 3)

r2_2 <- round(summary(africa_lm2)$r.squared, 3)

ggplot(data = africa, aes(x = Population..millions., y = Total.Ecological.Footprint)) +
  geom_point() +
  annotate("text", x = 0, y = 5, label = paste0("slope == ", slope_2, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.75, label = paste0("italic(p) < ", p.val_2), parse = TRUE, hjust = 0) +
  annotate("text", x = 0, y = 4.5, label = paste0("italic(r)^2 == ", r2_2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(caption = "Figure 9. Linear regression model on the population and the total ecological footprint",
       x = "Population millions.",
       y = "Total Ecological Footprint") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#correlation

afr_sub <- africa%>% 
  select(Total.Biocapacity:Total.Ecological.Footprint)

afr_cor <- cor(afr_sub)

afr_cor

corrplot(afr_cor, type = "upper", order="FPC",
         col=brewer.pal(n=8, name="PuOr"))

afr_sub2 <- africa %>% 
  select(-Population..millions., -Country, -Urban.Land, -Grazing.Land, -Forest.Land, -Fishing.Water, -Cropland, -HDI, -Countries.Required
         , -GDP.per.Capita, -Data.Quality, -Earths.Required, -Biocapacity.Deficit.or.Reserve, -Region, -Development_status)

afr_cor2 <- cor(afr_sub2)

corrplot(afr_cor2, type="upper", order="FPC",
col=brewer.pal(n=8, name="PuOr"))
