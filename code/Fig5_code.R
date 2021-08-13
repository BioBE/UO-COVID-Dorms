#-Load Packages------------------------------------------
library(tidyverse)

## Load data
clean.data = read.csv('~/data/Fig5_data.csv')

clean.data$Result[clean.data$Result =='Positive (2/3)' |clean.data$Result =='Positive (3/3)'] <- 'Positive'
## Make the negative samples have a Ct of 40 
clean.data$Ct[clean.data$Result == "Negative"] <- 40

#-Break the data apart the way we want it to---------------------------------
# What are our unique subjects? 
subjects = unique(clean.data$SubjectID)
positives = data.frame()
negatives = data.frame()
investigate = data.frame()
for (s in subjects) {
    # Which subject are we on? 
  temp.subject = clean.data[grepl(paste0("^",s,"$"), clean.data$SubjectID), ]
  
  # What are our unique entries? 
  entries = unique(temp.subject$Entry)
  
    for (e in entries) {
      #print(paste("This is subject", s, "and entry",e))
      
      temp.entry = temp.subject[grepl(paste0("^",e,"$"), temp.subject$Entry), ]
      
      # Select the columns that we want (make this move a little faster maybe?)
      temp.entry = temp.entry %>% select("SubjectID", "Entry", "Result", "Location", "Ct")
      
      # Find the nasal swab (if it exists)
      nasal = temp.entry[grepl("Nose", temp.entry$Location), ]
      
        if (identical(nasal$Result, character(0)) == TRUE){
          #print(paste("There isn't a nasal swab for subject ",s, " entry ",e))
          next
        }
        else{
          if (length(nasal$Result) > 1){
            if (unique(nasal$Result) == "Positive"){
              positives = rbind(positives, temp.entry)
            }
            else{
              if (unique(nasal$Result) == "Negative"){
                negatives = rbind(negatives, temp.entry)
              }
              else{
                investigate = rbind(investigate, temp.entry)
              }
            }
            }
          else{
            if (nasal$Result == "Positive"){
              #print("Positive")
              positives = rbind(positives, temp.entry)
              }
            else{
              #print("Negative")
              negatives = rbind(negatives, temp.entry)
            }
          }
        }
    }
}

positives$group = "Positive Nasal Swab"
negatives$group = "Negative Nasal Swab"

plot.df = rbind(positives, negatives)

plot.df = plot.df[!grepl("Nose", plot.df$Location), ]
plot.df = plot.df[!grepl("Mouth", plot.df$Location), ]

output.nose = ggplot(plot.df, aes(group, Ct)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, alpha = 0.5, aes(fill = group)) +
  ggsignif::geom_signif(comparisons = list(c("Negative Nasal Swab", "Positive Nasal Swab")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
theme_classic() +
  theme(legend.position = "none") +
  #scale_fill_manual(values = c(pal[5], pal[3])) +
  scale_fill_viridis(discrete=TRUE, direction = -1, option = "C") +
  scale_y_reverse(lim = c(40, 15)) +
  xlab("") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8))

## MOUTH-----------------------
#-Read in and prep the final data ---------------------------------
data = read.csv('/Volumes/Data/COVID-Barnhart/data/DataTF_Final.csv')
survey = read.csv("/Volumes/Data/COVID-Barnhart/data/DataSurvey.csv")
## Do the same thing but with mouth swabs
data_filter = data %>%
  filter(Site == 'Barnhart')

locations = c("TF", "Mouth")

#locations = c("Mouth", "Keyboard", "Phone", "Bathroom Floor", "Bathroom FLoor", "Exhaust Air", "Desk", "Next to TFS", "TF", "Nose")

data_filter <- data_filter %>%
  #drop_na() %>%
  filter(SubjectID != 'NEG') %>%
  filter(Location %in% locations)

## Remove the samples that failed internal control 
rownames(data_filter) = data_filter$X
samples.to.remove = data_filter[!grepl("Positive", data_filter$Result), ]
samples.to.remove = samples.to.remove[grepl("No Amp", samples.to.remove$MS2), ]
#samples.to.remove = samples.to.remove[grepl("No Amp", samples.to.remove$S), ]
#samples.to.remove = samples.to.remove[grepl("No Amp", samples.to.remove$ORF1ab), ]
samples.to.remove = rownames(samples.to.remove)
clean.data = data_filter[-which(rownames(data_filter) %in% samples.to.remove), ]

clean.data$Result[clean.data$Result =='Positive (2/3)' |clean.data$Result =='Positive (3/3)'] <- 'Positive'
## Make the negative samples have a Ct of 40 
clean.data$Ct[clean.data$Result == "Negative"] <- 40

#-Break the data apart the way we want it to---------------------------------
# What are our unique subjects? 
subjects = unique(clean.data$SubjectID)
positives = data.frame()
negatives = data.frame()
investigate = data.frame()
for (s in subjects) {
    # Which subject are we on? 
  temp.subject = clean.data[grepl(paste0("^",s,"$"), clean.data$SubjectID), ]
  
  # What are our unique entries? 
  entries = unique(temp.subject$Entry)
  
    for (e in entries) {
      #print(paste("This is subject", s, "and entry",e))
      
      temp.entry = temp.subject[grepl(paste0("^",e,"$"), temp.subject$Entry), ]
      
      # Select the columns that we want (make this move a little faster maybe?)
      temp.entry = temp.entry %>% select("SubjectID", "Entry", "Result", "Location", "Ct")
      
      # Find the nasal swab (if it exists)
      nasal = temp.entry[grepl("Mouth", temp.entry$Location), ]
      
        if (identical(nasal$Result, character(0)) == TRUE){
          #print(paste("There isn't a nasal swab for subject ",s, " entry ",e))
          next
        }
        else{
          if (length(nasal$Result) > 1){
            if (unique(nasal$Result) == "Positive"){
              positives = rbind(positives, temp.entry)
            }
            else{
              if (unique(nasal$Result) == "Negative"){
                negatives = rbind(negatives, temp.entry)
              }
              else{
                investigate = rbind(investigate, temp.entry)
              }
            }
            }
          else{
            if (nasal$Result == "Positive"){
              #print("Positive")
              positives = rbind(positives, temp.entry)
              }
            else{
              #print("Negative")
              negatives = rbind(negatives, temp.entry)
            }
          }
        }
    }
}

positives$group = "Positive Mouth Swab"
negatives$group = "Negative Mouth Swab"

plot.df = rbind(positives, negatives)

plot.df = plot.df[!grepl("Nose", plot.df$Location), ]
plot.df = plot.df[!grepl("Mouth", plot.df$Location), ]

output.mouth = ggplot(plot.df, aes(group, Ct)) +
  geom_boxplot(aes(fill = group), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, alpha = 0.5, aes(fill = group)) +
  ggsignif::geom_signif(comparisons = list(c("Negative Mouth Swab", "Positive Mouth Swab")), map_signif_level = function(p)sprintf("P = %.2g", p), test = 't.test', fontface = "italic", textsize = 2.5, tip_length = 0) +
theme_classic() +
  theme(legend.position = "none") +
  #scale_fill_manual(values = c(pal[5], pal[3])) +
  scale_fill_viridis(discrete=TRUE, direction = -1, option = "C") +
  scale_y_reverse(lim = c(40, 15)) +
  xlab("") +
  ylab(expression(Cycle~Threshold~(C[T]))) +
  theme(text = element_text(size=8))

figure <- ggarrange(output.nose, output.mouth,
                    labels = c("a", "b"),
                    nrow = 1)
                    
figure
