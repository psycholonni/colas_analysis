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
IH_vars<- c("participant", "Ishi_textbox.text", "Ishi_trial_keyresp.rt", "imageaddress")

#make catch data frame 
IHdata<-data[IH_vars]

#remove empty cells
IHdata <- IHdata %>% drop_na()
}

#Scoring reference 
scoringtable<- read_excel("PictureConditions.xlsx")

#inserting correct answers to participants dataframe
IHdata <- merge(IHdata, scoringtable, by= "imageaddress")

#coercing answers into numeric for efficient matching
IHdata$answer.numeric <- as.numeric(IHdata$Ishi_textbox.text)
IHdata$match <- IHdata$answer.numeric==IHdata$correctanswer


#MAKE IH_PERPERSON DATAFRAME WITH ONE ROW PER PARTICIPANT
{
#make dataframe with IH scores
IH_perperson<- data.frame(unique(IHdata$participant))
IH_perperson<-rename(IH_perperson, participant = unique.IHdata.participant.)
}

#get IH score as decimal, not sure that's useful
{#z=participant id 
get_IHscoredecimal<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscoredecimal<-format(round(correct_IH/total_IH, 2), nsmall = 2)
  IHscoredecimal<-as.numeric(IHscoredecimal)
  return(IHscoredecimal)
}
IH_perperson$score_decimal<-lapply(IH_perperson$participant, get_IHscoredecimal)  
}
#get IH score as fraction
{#z=participant id
get_IHscorefraction<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
  correct_IH<- nrow(subset(temp_df,Ishi_textbox.text==correctanswer))
  IHscorefraction<-paste(correct_IH,"/",total_IH,"/", "16")
  return(IHscorefraction)
}
IH_perperson$score_fraction<-lapply(IH_perperson$participant, get_IHscorefraction)
}
#get IH correctly answered 
{get_IHcorrectQs<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  correct_IH<- nrow(subset(temp_df,match==TRUE))
  return(correct_IH)
}
  IH_perperson$correctA<-lapply(IH_perperson$participant, get_IHcorrectQs)
  IH_perperson$correctA <- as.numeric(IH_perperson$correctA)
}
#get IH questions answered 
{get_IHtotal_answered<- function(z){ 
  temp_df <- subset(IHdata,participant==z)
  total_IH <- nrow(temp_df)
   return(total_IH)
}
IH_perperson$total_answered<-lapply(IH_perperson$participant, get_IHtotal_answered)
IH_perperson$total_answered <- as.numeric(IH_perperson$total_answered)
}
#get average response time
{get_IHrt<- function(z){
  temp_df <- subset(IHdata,participant==z)
  rt_IH<-mean(temp_df$Ishi_trial_keyresp.rt)
  return(rt_IH)
}
IH_perperson$mean.rt <- lapply(IH_perperson$participant, get_IHrt)
IH_perperson$mean.rt <- as.numeric((IH_perperson$mean.rt))
}

#subsetting a single participant
singleIHdata<-IHdata[IHdata$participant == "61716a16c157db249e36fc46", ]
head(singleIHdata)

#this participant submitted two files
IH_perperson <- subset(IH_perperson,!participant == "61716a16c157db249e36fc46")

#Distribution of IH scores 
ggplot(IH_perperson, aes(correctA)) +
  geom_histogram() +
  scale_x_continuous(breaks = as.numeric(round(quantile(IH_perperson$correctA),digits = 2))) + 
  theme_pubr() +   
  theme(axis.line = element_blank())+
  geom_rangeframe()+
  labs(x="Ishihara score",
       title= "Histogram of Ishihara score")
