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

clean.data = read.csv('~/data/fig3_data.csv')

## Make the negative samples have a Ct of 40 
clean.data$Ct[clean.data$Result == "Negative"] <- 40

## Fix location names
clean.data$Location[clean.data$Location == "Bathroom FLoor"] <- "Bathroom Floor"
clean.data$Location[clean.data$Location == "Keyboard"] <- "Computer"
clean.data$Location[clean.data$Location == "Exhaust Air"] <- "Bathroom Exhaust"
clean.data$Location[clean.data$Location == "Desk"] <- "Near Settling Plate"
clean.data$Location[clean.data$Location == "Next to TFS"] <- "Far Settling Plate"
clean.data$Location[clean.data$Location == "TF"] <- "AerosolSense"
clean.data$Location[clean.data$Location == "Nose"] <- "Shallow Nasal Swab"
clean.data$Location[clean.data$Location == "Mouth"] <- "Mouth Swab"

##-Visualization----------------
locations = unique(clean.data$Location)

for(l in locations) { 
 nam <- paste(l)
 
 temp.location = clean.data[grepl(l, clean.data$Location), ]
 
 ## Calculate percentages 
## Simplify positives 
temp.location$Result[temp.location$Result =='Positive (2/3)' |temp.location$Result =='Positive (3/3)'] <- 'Positive'

## Make a positives only dataframe
temp.positives = temp.location %>%
  filter(Result == 'Positive')

temp.positives <- ddply(temp.positives, .(SubjectID, PosDay), .drop=FALSE, summarise, positive_count = length(Result))

temp.totals <- ddply(temp.location, .(SubjectID, PosDay), .drop=FALSE, summarise, total_count = length(Result))
 
## Combine
temp.combined <- inner_join(temp.totals, temp.positives, by = c('SubjectID','PosDay'))



temp.combined$percent = (temp.combined$positive_count/temp.combined$total_count)*100

 fit2 = lmer(percent~as.numeric(PosDay)+(1|SubjectID), data = temp.combined)
summary(fit2)
 
 stat = anova(fit2)
 stat$signif <- rep('No Significance')
 stat$signif[stat$`Pr(>F)` < 0.05] <- 'P < 0.05'
 stat$signif[stat$`Pr(>F)` < 0.01] <- 'P < 0.01'
 stat$signif[stat$`Pr(>F)` < 0.001] <- 'P < 0.001'
 
assign(nam, ggplot(temp.combined, aes(as.numeric(PosDay), percent, group = PosDay)) +
          geom_boxplot(fill = "grey", alpha = 0.5, outlier.shape = NA) +
          geom_jitter(shape = 21, alpha = 0.5) +
          #geom_smooth(method = 'lm', color = "green", aes(group=1)) +
          geom_smooth(method = 'lm', color = "black", aes(group=1)) +
          theme_classic() +
          ylab("Percent Positive (%)") +
          xlab("Days since Positive Diagnostic Test") +
          labs(title = paste(l), 
               subtitle = paste(stat$signif)) +
          scale_y_continuous(breaks = breaks_pretty(n=5), minor_breaks = NULL) +
          scale_x_continuous(breaks = breaks_pretty(12), minor_breaks = NULL) +
          theme(text = element_text(size=8),
                legend.position = 'none',
                plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.9, vjust = -4, face = "italic")))
}
 
final.figure = ggarrange(`Bathroom Floor`, `Phone`, `Shallow Nasal Swab`, `Computer`, `Mouth Swab`, `Bathroom Exhaust`, `Near Settling Plate`, `Far Settling Plate`, `AerosolSense`)

final.figure
