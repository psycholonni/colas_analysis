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
IHdata$match <- IHdata$Ishi_textbox.text==IHdata$correctanswer


#MAKE IH_PERPERSON DATAFRAME WITH ONE ROW PER PARTICIPANT
{

#make dataframe with IH scores
IH_perperson<- data.frame(unique(participants_complete$participant))
IH_perperson<-rename(IH_perperson, participant = unique.participants_complete.participant.)

#get IH score as decimal, not sure that's useful
#z=participant id 
get_IHscoredecimal<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscoredecimal<-format(round(correct_IH/total_IH, 2), nsmall = 2)
  IHscoredecimal<-as.numeric(IHscoredecimal)
  return(IHscoredecimal)
}
#IH_perperson$score_decimal<-lapply(IH_perperson$participant, get_IHscoredecimal)  

#get IH score as fraction
#z=participant id
get_IHscorefraction<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscorefraction<-paste(correct_IH,"/",total_IH,"/", "16")
  return(IHscorefraction)
}
IH_perperson$score_fraction<-lapply(IH_perperson$participant, get_IHscorefraction)
}

get_IHtotal_answered<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
   return(total_IH)
}
IH_perperson$total_answered<-lapply(IH_perperson$participant, get_IHtotal_answered)
IH_perperson$total_answered <- as.numeric(IH_perperson$total_answered)


#Rearrange dataframe 
#IH_perperson$score_decimal <- as.numeric(IH_perperson$score_decimal)
#IH_ordered <- IH_perperson[order(IH_perperson$score_decimal),]
IH_ordered <- IH_perperson[order(IH_perperson$total_answered),]

IH_perperson$score <- as.numeric(IH_perperson$score_fraction)
#Need to make a unique score to filter answers <14 & >16 

IH_perperson$decimal <- sapply(IH_perperson$score_fraction, function(x) eval(parse(text=x)))

#single participants
singleIHdata<-IHdata[IHdata$participant == "5ec3eefc909cda09ed268192", ]
singleIHdata
surveydata<-data[, survey_vars]
