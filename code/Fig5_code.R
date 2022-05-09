##-Load packages------------------------------------------------
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
library(scales)
library(lmerTest)

## Read in the data
final_air = read.csv('~/data/Fig6_data.csv')

## Now determine whether the window was open or clsoed and which ACH reading to use based on that 
final_air = final_air %>% mutate(ACH =
                     case_when(Window...open == "0%" ~ final_air$ACH.closed, 
                               Window...open == "<25%" ~ final_air$ACH.closed,
                               is.na(Window...open) ~ final_air$ACH.closed,
                               Window...open == ">50%" ~ final_air$ACH.open,
                               Window...open == "25-50%" ~ final_air$ACH.open,
                               Window...open == "100%" ~ final_air$ACH.open))

final_air = final_air %>% mutate(window.final =
                     case_when(Window...open == "0%" ~ "Closed",
                               Window...open == "<25%" ~ "Closed",
                               is.na(Window...open) ~ "Closed",
                               Window...open == "25-50%" ~ "Closed",
                               Window...open == ">50%" ~ "Open",
                               Window...open == "100%" ~ "Open"))


air.bar = ddply(final_air, .(Room, ACH), .drop=TRUE, summarise, ACH = mean(ACH))
rows = nrow(air.bar)
air.bar$Room = 1:rows

## Calculate percentages 
## Simplify positives 
final_air$Result[final_air$Result =='Positive (2/3)' |final_air$Result =='Positive (3/3)'] <- 'Positive'

## Make a positives only dataframe
positives = final_air %>%
  filter(Result == 'Positive')

## Get totals for each subject at each entry 
total_counts <- ddply(final_air, .(SubjectID, Entry), .drop=FALSE, summarise, Total_Count = length(Result))

## Get positive totals for each subject at each entry 
positive_counts <- ddply(positives, .(SubjectID, Entry), .drop=FALSE, summarise, Pos_Count = length(Result))

## Combine 
combined_counts <- inner_join(total_counts, positive_counts, by = c("SubjectID", "Entry"))
combined_counts <- inner_join(combined_counts, final_air, by = c("SubjectID", "Entry"))

combined_counts$percent = (combined_counts$Pos_Count/combined_counts$Total_Count)*100

final.percents = combined_counts %>%
  select(SubjectID, Entry, Total_Count, Pos_Count, ACH, percent)
  
##-Visualization--------------------------
plot1 = ggplot(air.bar, aes(as.factor(reorder(Room, ACH)), ACH)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = ACH), colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_fill_viridis(option = "D") +
  theme_minimal() +
  xlab("Room Number") +
  ylab("Air Changes/Hour (ACH)") +
  theme(text = element_text(size=8),
        legend.position = 'none',
        axis.text.x=element_blank())

plot2 = ggplot(final_air, aes(ACH, Ct)) +
  geom_jitter(shape = 21, aes(fill = ACH)) +
  geom_smooth(method = 'lm', color = "black") +
  scale_fill_viridis(option = "D") +
  theme_minimal() +
  scale_y_reverse(lim = c(35, 17)) +
  xlab("Air Changes/Hour (ACH)") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  scale_x_continuous(breaks = breaks_pretty(n=5)) +
  theme(text = element_text(size=8),
        legend.position = 'none')

plot3 = ggplot(final.percents, aes(ACH, percent)) +
  geom_jitter(shape = 21, aes(fill = ACH)) +
  geom_smooth(method = 'lm', color = "black") +
  scale_fill_viridis(option = "D") +
  xlab("Air Changes/Hour (ACH)") +
  ylab("Percent Positive (%)") +
  theme_minimal() +
    theme(text = element_text(size=8),
        legend.position = 'none')
        
plot4 = ggplot(final_air, aes(window.final, Ct)) +
  geom_boxplot(aes(fill = window.final), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, aes(fill = window.final), alpha = 0.1) +
  geom_signif(comparisons = list(c("Open", "Closed")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
  theme_classic() +
  theme(legend.position = "none") +
  #scale_fill_manual(values = c("grey", "skyblue")) +
  scale_fill_viridis(discrete=TRUE, option = "D") +
  scale_y_reverse(lim = c(40, 15)) +
  xlab("Window Position") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8))

figure = ggarrange(plot1, plot2, plot3, plot4, labels = c("a", "b", "c", "d")) 

figure

## Stats
model.air = glm(Ct~ACH, data = final_air)
model.percent.air = glm(percent~ACH, data = final.percents)
