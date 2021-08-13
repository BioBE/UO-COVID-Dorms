##-Load packages------------------------------------------------
library(lubridate)
library(tidyverse)
library(RColorBrewer)
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
library(report)

clean.data = read.csv('~/data/Fig2_data.csv')

## Fix location names
clean.data$Location[clean.data$Location == "Bathroom FLoor"] <- "Bathroom Floor"
clean.data$Location[clean.data$Location == "Nose"] <- "Shallow Nasal Swab"
clean.data$Location[clean.data$Location == "Keyboard"] <- "Computer"
clean.data$Location[clean.data$Location == "Mouth"] <- "Mouth Swab"
clean.data$Location[clean.data$Location == "Exhaust Air"] <- "Bathroom Exhaust"
clean.data$Location[clean.data$Location == "Desk"] <- "Near Settling Plate"
clean.data$Location[clean.data$Location == "Next to TFS"] <- "Far Settling Plate"
clean.data$Location[clean.data$Location == "TF"] <- "AerosolSense"

final_locations <- ddply(clean.data, .(SubjectID, Location, PosDay), .drop=TRUE, summarise, Ct = mean(Ct))

##-Visualization---------------------
locations = unique(final_locations$Location)

for(l in locations) { 
 nam <- paste(l)
 
 temp.location = final_locations[grepl(l, final_locations$Location), ]
 
 fit = lmer(Ct~PosDay+(1|SubjectID), data=temp.location)
 fit2 = lmer(Ct~as.numeric(PosDay)+(1|SubjectID), data = temp.location)
summary(fit2)
 
 stat = anova(fit2)
 stat$signif <- rep('No Significance')
 stat$signif[stat$`Pr(>F)` < 0.05] <- 'P < 0.05'
 stat$signif[stat$`Pr(>F)` < 0.01] <- 'P < 0.01'
 stat$signif[stat$`Pr(>F)` < 0.001] <- 'P < 0.001'
 
 assign(nam, ggplot(temp.location, aes(as.numeric(PosDay), Ct, group = PosDay)) +
          geom_boxplot(fill = "grey", alpha = 0.5, outlier.shape = NA) +
          geom_jitter(shape = 21, aes(fill = Ct), alpha = 0.5) +
          geom_smooth(method = 'glm', color = "black", aes(group=1)) +
          theme_classic() +
          ylab(expression(Cycle~Threshold~(C[T]))) +
          xlab("Days since Positive Diagnostic Test") +
          scale_fill_viridis(option = "D", direction = -1) +
          scale_y_reverse(lim = c(40, 20)) +
          labs(title = paste(l),
               subtitle = paste(stat$signif)) +
          scale_x_continuous(breaks = breaks_pretty(n=12), minor_breaks = NULL) +
          theme(text = element_text(size=8),
                legend.position = 'none',
                plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic")))
}

fig2 = ggarrange(`Bathroom Floor`, `Phone`, `Shallow Nasal Swab`, `Computer`, `Mouth Swab`, `Bathroom Exhaust`, `Near Settling Plate`, `Far Settling Plate`, `AerosolSense`) 
