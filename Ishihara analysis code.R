## ISHIHARA

library(readxl)
setwd("~/thesis/colas_analysis")

#READ FILES 
{
  #make a list of data file names
  files <- list.files(path="./data", pattern="*.csv", full.names=TRUE)
  #files
  
  #make a dataframe binding all datafiles
  data <- sapply(files, read.csv, simplify=FALSE) %>%
    lapply(\(x) mutate(x, across(Ishi_textbox.text, as.character))) %>% bind_rows(.id = "fileId") 
}

#MAKE DATAFRAME WITH ONE ROW PER ANSWER
{
IH_vars<- c("participant", "Ishi_textbox.text", "imageaddress")

#make catch data frame 
IHdata<-data[IH_vars]

#remove empty cells
IHdata <- IHdata %>% drop_na()
}

#Scoring reference 
scoringtable<- read_excel("PictureConditions.xlsx")

#inserting correct answers to participants dataframe
IHdata <- merge(IHdata, scoringtable, by= "imageaddress")

#MAKE IH_PERPERSON DATAFRAME WITH ONE ROW PER PARTICIPANT
{

#make dataframe with IH scores
IH_perperson<- data.frame(unique(IHdata$participant))
IH_perperson<-rename(IH_perperson, participant = unique.IHdata.participant.)

#get IH score as decimal 
#z=participant id 
get_IHscoredecimal<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscoredecimal<-format(round(correct_IH/total_IH, 2), nsmall = 2)
  IHscoredecimal<-as.numeric(IHscoredecimal)
  return(IHscoredecimal)
}
IH_perperson$score_decimal<-lapply(IH_perperson$participant, get_IHscoredecimal)  

#get IH score as fraction
#z=participant id
get_IHscorefraction<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscorefraction<-paste(correct_IH,"/",total_IH)
  return(IHscorefraction)
}
IH_perperson$score_fraction<-lapply(IH_perperson$participant, get_IHscorefraction)
}
