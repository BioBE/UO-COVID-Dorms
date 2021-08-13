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

## Functions------------------------------------------------
FitFlextableToPage <- function(ft, pgwidth = 6){

  ft_out <- ft %>% autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

## Load data
df_final = read.csv('~/data/Table2_data.csv')

model2 = lm(Ct ~ Fever.Intensity + Coughing.Intensity + Sneezing.Intensity + Difficulty.Breathing + Fatigue + Headache + Eyes.Ache + Eyes.Watering + Sore.Throat + Distorted.Taste + Loss.of.Taste + Distorted.Smell + Loss.of.Smell + Ears.Ringing + GI.Symptoms + Congestion + Brain.Fog, data = df_final, family = "poisson")

symptoms.poisson = tidy(model2)
symptoms.poisson$term = c("(Intercept)", "Fever", "Coughing", "Sneezing", "Difficulty Breathing", "Fatigue", "Headache", "Eyes Ache", "Eyes Watering", "Sore Throat", "Distorted Taste", "Loss of Taste", "Distorted Smell", "Loss of Smell", "Ears Ringing", "GI Symptoms", "Congestion", "Brain Fog")#Add better symptom names 
symptoms.poisson$signif <- rep('Not Significant')
symptoms.poisson$signif[symptoms.poisson$p.value < 0.05] <- '< 0.05'
symptoms.poisson$signif[symptoms.poisson$p.value < 0.01] <- '< 0.01'
symptoms.poisson$signif[symptoms.poisson$p.value < 0.001] <- '< 0.001'

final.table = symptoms.poisson %>%
  filter(term !='(Intercept)') %>%
  select(term, estimate, signif)
  
final.table = flextable(final.table) %>%
  colformat_double(digits = 2) %>%
  set_header_labels(final.table, term = "Symptom", estimate = "Slope", signif = "Significance Level" )
final.table = add_header_row(final.table, values = c("Symptom Correlation Coefficients"),
  colwidths = c(3))
final.table = theme_booktabs(final.table)
final.table = fontsize(final.table, size = 11)
final.table = align(final.table, align = "center", part = "all")
symptoms.table = bold(final.table, i = c(2, 8, 9, 13, 15, 16, 17), j = "signif")
symptoms.table = bold(symptoms.table, i = c(2, 8, 9, 13, 15, 16, 17), j = "term")
symptoms.table = bold(symptoms.table, i = c(2, 8, 9, 13, 15, 16, 17), j = "estimate")

symptoms.table = FitFlextableToPage(symptoms.table)
symptoms.table = set_caption(symptoms.table, 'Table 2. Linear correlations between the self-reported symptoms of study participants and measured cycle threshold values.')
symptoms.table
