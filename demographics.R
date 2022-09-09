
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
sd(demographics$time_mins)

#Age Medan & median
mean(demographics$age, na.rm= TRUE)
sd(demographics$age, na.rm= TRUE)
median(demographics$age, na.rm= TRUE)

#Countries/ Nationality 
demographics %>%
 filter(!(reviewed_at_datetime %in% "") | is.na(reviewed_at_datetime)) %>%
 ggplot() +
 aes(x = Nationality) +
 geom_bar(fill = "#112446") +
 coord_flip() +
 theme_pubr()

#Time taken vs catch score
demographics %>%
 filter(!(reviewed_at_datetime %in% "") | is.na(reviewed_at_datetime)) %>%
 ggplot() +
 aes(x = time_mins, y = catch.score_decimal) +
 geom_point(shape = "circle", size = 1.5, 
 colour = "#112446") +
 theme_classic()

# get proportion of males/females as well as students 
table(demographics$Sex)

get_mean.rt<- function(z){ 
  temp_df <- subset(trialdata,participant==z)
  mean.rt<- mean(temp_df$response_time)
  return(mean.rt)
}

participants_complete$rt <-lapply(participants_complete$participant_id, get_mean.rt)
participants_complete$rt <- as.numeric(participants_complete$rt)

catch_perperson$score_decimal <- as.numeric(catch_perperson$score_decimal)
catch_perperson$response_time <- as.numeric(catch_perperson$response_time)
ggplot(catch_perperson, aes(response_time, score_decimal))+
  geom_point()+
  geom_hline(yintercept = .77)+
  theme_pubr()

ggplot(catch_perperson, aes(response_time, score_decimal))+
   geom_point()+
   geom_hline(yintercept = .8)+
   theme_pubr()+
   coord_cartesian(xlim = c(0,5))
