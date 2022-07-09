
setwd("~/thesis/colas_analysis")

#READ FILES 
{
  #make a list of data file names
  demo.files <- list.files(path="./demographics", pattern="*.csv", full.names=TRUE)
  #make a dataframe binding all datafiles
  demographics <- sapply(demo.files, read.csv, simplify=FALSE) %>% bind_rows(.id = "fileId")
}

#ADD CATCH SCORE 
get_catchscoredecimal<- function(z){ 
  temp_df <- subset(catchdata,participant==z)
  total_catch <- nrow(temp_df)
  correct_catch<- nrow(subset(temp_df,catchnumber==catchresponse))
  catchscoredecimal<-format(round(correct_catch/total_catch, 2), nsmall = 2)
  catchscoredecimal<-as.numeric(catchscoredecimal)
  return(catchscoredecimal)
}
demographics$catch.score_decimal<-lapply(demographics$participant, get_catchscoredecimal)
demographics$catch.score_decimal <- as.numeric(demographics$catch.score_decimal)


#MAKE DATAFRAME WITH NA CATCHSCORE 
demo.catch.na <- demographics[is.na(demographics$catch.score_decimal),]

#make a list of data file names
small.files <- list.files(path="./data/wave IV small files", pattern="*.csv", full.names=TRUE)
#make a dataframe binding all datafiles
datasmall<- sapply(small.files, read.csv, simplify=FALSE) %>% bind_rows(.id = "fileId")
test <- merge(datasmall, demographics, by.x = "participant", by.y = "participant_id") 
test <-unique(test$participant)
#GET MEAN & MEDIAN COMPLETION TIME 
demographics$time_mins <- demographics$time_taken/60
demographics <- subset(demographics, !status=='TIMED-OUT')
demographics <- subset(demographics, !status=='RETURNED')
demographics <- subset(demographics, !status=='REJECTED')
mean(demographics$time_mins)
median(demographics$time_mins)
