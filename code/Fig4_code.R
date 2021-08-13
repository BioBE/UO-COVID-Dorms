## Load packages 
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(stargazer)
library(reshape2)
library(ggsignif)
library(plyr)
library(viridis)
library(wesanderson)
library(flextable)
library(webshot)
library(broom)
library(ggpubr)

## Load data
df_final = read.csv('~/data/Fig4_data.csv')

##---------------------------------------------------------
## Prep the data for Analysis - AerosolSense samples only 
##---------------------------------------------------------
## We only want the AerosolSense samples now
df_as = df_final %>%
  filter(Location == 'TF')

## Make the negative samples have a Ct of 40 
df_as$Ct[df_as$Result == "Negative"] <- 40

## Sum symptoms
df_as = df_as %>%
    mutate(Symptoms.Total = select(., Fever.Intensity:Brain.Fog) %>% rowSums(na.rm = TRUE))

symptoms = df_as %>%
  select(SubjectID, PosDay, Ct, Symptoms.Total, Entry)

symptoms_summarized <- ddply(symptoms, .(SubjectID, Entry), .drop=TRUE, summarise, symptoms = sum(Symptoms.Total), Ct = mean(Ct))

symptoms_summarized$group <- rep('Asymptomatic')
symptoms_summarized$group[symptoms_summarized$symptoms > 0] <- 'Symptomatic'

pal = wes_palette("Royal2")
## ggplot
as.boxplot = ggplot(symptoms_summarized, aes(group, Ct)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, alpha = 0.5, aes(fill = group)) +
  geom_signif(comparisons = list(c("Asymptomatic", "Symptomatic")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
  theme_classic() +
  scale_y_reverse(lim = c(40, 15)) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete=TRUE, direction = -1, option = "C") +
  xlab("") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=45, hjust = 1))

##---------------------------------------------------------
## Prep the data for Analysis - Aerosol (including settling plates) samples only 
##---------------------------------------------------------
## We only want the AerosolSense samples now
df_air = df_final %>%
  filter(Location !='Mouth',
         Location !='Bathroom Floor',
         Location !='Bathroom FLoor',
         Location !='Phone',
         Location !='Nose',
         Location !='Keyboard')#,
         #Location !='TF', 
         #Location !='Exhaust')

## Make the negative samples have a Ct of 40 
df_air$Ct[df_air$Result == "Negative"] <- 40

## Sum symptoms
df_air = df_air %>%
    mutate(Symptoms.Total = select(., Fever.Intensity:Brain.Fog) %>% rowSums(na.rm = TRUE))

symptoms = df_air %>%
  select(SubjectID, PosDay, Ct, Symptoms.Total, Entry)

symptoms_summarized <- ddply(symptoms, .(SubjectID, Entry), .drop=TRUE, summarise, symptoms = sum(Symptoms.Total), Ct = mean(Ct))

symptoms_summarized$group <- rep('Asymptomatic')
symptoms_summarized$group[symptoms_summarized$symptoms > 0] <- 'Symptomatic'

pal = wes_palette("Royal2")
## ggplot
air.boxplot = ggplot(symptoms_summarized, aes(group, Ct)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, alpha = 0.5, aes(fill = group)) +
  geom_signif(comparisons = list(c("Asymptomatic", "Symptomatic")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
  theme_classic() +
  scale_y_reverse(lim = c(40, 15)) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete=TRUE, direction = -1, option = "C") +
  xlab("") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=45, hjust = 1))

##---------------------------------------------------------
## Prep the data for Analysis - surface swabs only 
##---------------------------------------------------------
## We only want the surface swab samples now
df_swabs = df_final %>%
  filter(Location !='Mouth',
         Location !='Nose',
         Location !='TF', 
         Location !='Desk',
         Location !='Next to TFS',
         Location !='Exhaust Air',
         Location !='Exhaust swabs')

## Make the negative samples have a Ct of 40 
df_swabs$Ct[df_swabs$Result == "Negative"] <- 40

## Sum symptoms
df_swabs = df_swabs %>%
    mutate(Symptoms.Total = select(., Fever.Intensity:Brain.Fog) %>% rowSums(na.rm = TRUE))

symptoms = df_swabs %>%
  select(SubjectID, PosDay, Ct, Symptoms.Total, Entry)

symptoms_summarized <- ddply(symptoms, .(SubjectID, Entry), .drop=TRUE, summarise, symptoms = sum(Symptoms.Total), Ct = mean(Ct))

symptoms_summarized$group <- rep('Asymptomatic')
symptoms_summarized$group[symptoms_summarized$symptoms > 0] <- 'Symptomatic'

pal = wes_palette("Royal2")
##-visualization------------
swabs.boxplot = ggplot(symptoms_summarized, aes(group, Ct)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, alpha = 0.5, aes(fill = group)) +
  geom_signif(comparisons = list(c("Asymptomatic", "Symptomatic")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
  theme_classic() +
  theme(legend.position = "none") +
  #scale_fill_manual(values = c(pal[5], pal[3])) +
  scale_fill_viridis(discrete=TRUE, direction = -1, option = "C") +
  scale_y_reverse(lim = c(40, 15)) +
  xlab("") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=45, hjust = 1)) 

figure <- ggarrange(as.boxplot, air.boxplot, swabs.boxplot,
                    labels = c("a", "b", "c"),
                    nrow = 1)
                    
figure
