################################
##### THESIS ANALYSIS LONNI#####
################################
#This script is for the analysis of asymmetry in colour perception, 
#Reused code from "Sonia analysis code (Beth pilot)

#block most warnings when loading libraries
shhh <- suppressPackageStartupMessages 

#ariel's cheat to display colour in axis
block <- paste(strrep("\U2588",10),sep='')
block_rep <- rep(block,93)

#LOAD LIBRARIES
{library(tidyr)
library(plyr)
library(dplyr) %>% shhh
library(ggplot2) 
library(gridExtra) %>% shhh
library(MASS) %>% shhh
library(Matrix) %>% shhh
library(reshape2)%>% shhh
library(ape) %>% shhh # stats
library(vegan) %>% shhh # stats
library(RColorBrewer)
library(cocor)
library(DescTools) %>% shhh
library(reshape2) 
library(grid)
library(ggplotify)
library(ggpubr) %>% shhh
library(ggthemes)
library(stringr) 
library(rgl)
library(tidyverse)
library(plot.matrix) %>% shhh
library(farver)
library(matrixStats) %>% shhh
library(plotrix) %>% shhh
library(JWileymisc)
library(readxl)
library(visreg)
  }

setwd("~/thesis/colas_analysis")

#Create list of colours for factors 
{file <- read_xlsx("colourcodes.xlsx")
  
  ##CONVERT RGB TO HEX
  #split rgb strings
  #x=rgb string
  splitrgb <-function(x){ 
    variable<-str_replace_all(x, "\\[|\\]", "") #delete brackets
    variable<-strsplit(variable, split = ",") #split string
    variable<-lapply(variable, as.numeric)   #convert to numeric
    variable<-lapply(variable, "/", 100)  #convert rgb scale 
    variable<-lapply(variable, "*", 255)
    return (as.list(unlist(variable)))
  }
  
  #remove last character to clean 
  file$Colourcodes2 <- str_remove(file$Colourcodes, ";")
  file$colour <- lapply(file$Colourcodes2, splitrgb)
  
  #make dataframes with rows per r, g, and b
  colour_df <- as.data.frame(lapply(file$colour, function(x) t(do.call(cbind, x)))) 
  #swap rows and columns
  colour_df <- t(colour_df)
  #name columns
  colnames(colour_df) <- c("r", "g", "b") 
  #turn into a dataframe
  colour_df<-as.data.frame(colour_df) 
  colour_df$hex <- NA
  #rgb2hex function
  rgb2hex <- function(r, g, b) {rgb(r, g, b, maxColorValue = 255)}
  #Get hexcode 
  colour_df$hex <- apply(colour_df, 1, function(x) rgb2hex(x[1], x[2], x[3]))
  row.facs <- colour_df$hex
}

#READ FILES 
{
## Pilot data with Ishihara
#make a list of data file names
pilot.files <- list.files(path="./data/Pilot data", pattern="*.csv", full.names=TRUE)
#make a dataframe binding all datafiles
pilot.data <- sapply(pilot.files, read.csv, simplify=FALSE) %>% bind_rows(.id = "fileId")
#adding Ishihara colomn missing to allow rbind
pilot.data$Ishi_textbox.text <- NA

##Data with Ishihara test 
#make a list of data file names
files <- list.files(path="./data", pattern="*.csv", full.names=TRUE)
#make a dataframe binding all datafiles
data <- sapply(files, read.csv, simplify=FALSE) %>%
  lapply(\(x) mutate(x, across(Ishi_textbox.text, as.character))) %>% bind_rows(.id = "fileId") 

##Data with Ishihara test 
#make a list of data file names
waveiv.files <- list.files(path="./data/wave IV", pattern="*.csv", full.names=TRUE)
#make a dataframe binding all datafiles
waveiv.data <- sapply(waveiv.files, read.csv, simplify=FALSE) %>%
  lapply(\(x) mutate(x, across(Ishi_textbox.text, as.character))) %>% bind_rows(.id = "fileId") 

##Data with Ishihara test 
#make a list of data file names
pb.files <- list.files(path="./data/Problematic file", pattern="*.csv", full.names=TRUE)
#make a dataframe binding all datafiles
pb.data <- sapply(pb.files, read.csv, simplify=FALSE) %>%
  lapply(\(x) mutate(x, across(Ishi_textbox.text, as.character))) %>% bind_rows(.id = "fileId") 
pb.data$textbox.text <- as.character(pb.data$textbox.text)

data <- bind_rows(pb.data, data, pilot.data, waveiv.data)
}
#(n=537)
###################
###DATA CLEANING###
###################

#screening participants with framerate too low
data <- data[data$frameRate>29,]
#(n=527)
#MAKE CATCHDATA DATAFRAME WITH ONE ROW PER CATCH TRIAL, FOR ALL PARTICIPANTS
{
#select catch variables for analysis
catch_vars<- c("participant", "catchnumber", "catchresponse", "response_time_catch")

#make catch data frame 
catchdata<-data[data$Catch==1, catch_vars]

#remove empty cells
catchdata <- catchdata %>% drop_na()

#format response time
catchdata$response_time_catch<-as.numeric(catchdata$response_time_catch) #for graphing later
}
#MAKE CATCH_PERPERSON DATAFRAME WITH ONE ROW PER PARTICIPANT
{
#make dataframe with catch scores and mean response times
catch_perperson<- data.frame(unique(catchdata$participant))
catch_perperson<-rename(catch_perperson, participant = unique.catchdata.participant.)

#get catch score as decimal 
#z=participant id 
get_catchscoredecimal<- function(z){ 
  temp_df <- subset(catchdata,participant==z)
  total_catch <- nrow(temp_df)
  correct_catch<- nrow(subset(temp_df,catchnumber==catchresponse))
  catchscoredecimal<-format(round(correct_catch/total_catch, 2), nsmall = 2)
  catchscoredecimal<-as.numeric(catchscoredecimal)
  return(catchscoredecimal)
}
catch_perperson$score_decimal<-lapply(catch_perperson$participant, get_catchscoredecimal)

#get catch score as fraction - NOT VERY USEFUL
#z=participant id
get_catchscorefraction<- function(z){ 
  temp_df <- subset(catchdata,participant==z)
  total_catch <- nrow(temp_df)
  correct_catch<- nrow(subset(temp_df,catchnumber==catchresponse))
  catchscorefraction<-paste(correct_catch,"/",total_catch)
  return(catchscorefraction)
}
catch_perperson$score_fraction<-lapply(catch_perperson$participant, get_catchscorefraction)

#get mean catch response times
#z=participant id 
get_catchresponsetime<- function(z){ 
  temp_df <- subset(catchdata,participant==z)
  catchresponsetime<-mean(temp_df$response_time_catch)
  return(catchresponsetime)
}
catch_perperson$response_time<-lapply(catch_perperson$participant, get_catchresponsetime)


}
#HISTOGRAM OF DISTRIBUTION OF CATCH SCORES 
{#making decimal score numeric so it can be plotted
catch_perperson$score_decimal <- as.numeric(catch_perperson$score_decimal)
#making decimal a percentage just so plot is easier to understand
catch_perperson$percent <- catch_perperson$score_decimal*100
#plot
ggplot(catch_perperson) +
  aes(x = percent) +
  geom_histogram(binwidth= 20,fill = "#016450") +
  labs(x = "Percentage of correct answers",
        y = "Frequency (number of participants)") +
  scale_x_continuous(breaks = seq(0,100, by= 20))+
  theme_pubr(20)
}
#MAKING TRIALDATA DATAFRAME WITH ONE ROW PER TRIAL, FOR ALL PARTICIPANTS
{
#select trial variables for analysis
trial_vars<- c("participant", "Circle1_colour", "Circle2_colour", "Colourpair", "similarity", "response_time", "Temporder")

#make trial data frame
trialdata <- data[trial_vars]

#remove empty cells
trialdata <- trialdata %>% drop_na()
trialdata<- trialdata[!apply(trialdata == "", 1, any),]


#remove participants with low catch scores (<0.8)
high_catch<-subset(catch_perperson, score_decimal>=0.8)
low_catch <-subset(catch_perperson, score_decimal<0.8)
trialdata<-subset(trialdata, participant %in% high_catch$participant)

#remove participants with incomplete data 
participants <- data.frame(unique(trialdata$participant)) #make dataframe of participants

colnames(participants) <- c("participant_id")

get_trial_count<-function(x){ #get number of trials completed (x = participant id)
  trial_count<-sum(trialdata$participant == x)
  return(trial_count)
}
participants$trial_count <-lapply(participants$participant_id, get_trial_count)

participants_incomplete <- subset(participants,trial_count < 324) 

#dataframe of participants with complete data 
participants_complete<- subset(participants,trial_count==324)
participants_ubercomplete<- subset(participants,trial_count>324)

trialdata <- subset(trialdata, !participant %in% participants_incomplete$participant_id) #subset trialdata for participants who completed all trials

#make column with trial number 
#x=participant

for (x in 1:nrow(participants_complete)) {
  trialdata$trialno <- c(1:324)
}
}
#(n=494)
## RESCALE similarity to be 0-7 instead of -4/+4 - ONLY NECESSARY IF NOT ALREADY 0-7
{trialdata$similarity[trialdata$similarity == 4] <- 7
  trialdata$similarity[trialdata$similarity == 3] <- 6
  trialdata$similarity[trialdata$similarity == 2] <- 5
  trialdata$similarity[trialdata$similarity == 1] <- 4
  trialdata$similarity[trialdata$similarity == -1] <-3
  trialdata$similarity[trialdata$similarity == -2] <-2
  trialdata$similarity[trialdata$similarity == -3] <-1
  trialdata$similarity[trialdata$similarity == -4] <-0 
}
## HISTOGRAM OF ANSWERS PER PARTICIPANTS TO CHECK DISTRIBUTION ACROSS SCALE
{
#could modify y axis to show min/max count for better data/ink ratio
ggplot(trialdata) +
 aes(x = similarity, fill = participant) +
 geom_bar(position="dodge") +
 theme_minimal()+
    guides(fill="none")
ggplot(trialdata) +
    aes(x = similarity) +
    geom_bar(fill= "#016c59")+
    theme_pubr()+
  labs(x="Similarity rating", y= "Sum of trials")+
  scale_x_continuous(breaks = c(0:7))

#One facet per participant
#ggplot(trialdata) +
 aes(x = similarity, fill = participant) +
 geom_histogram(bins = 30L) +
 scale_fill_hue(direction = 1) +
 theme_minimal() +
 facet_wrap(vars(participant))
}

##CONVERT RGB TO HEX FOR GRAPHING (TRIALDATA)
{
#split rgb strings
#x=rgb string
splitrgb <-function(x){ 
  variable<-str_replace_all(x, "\\[|\\]", "") #delete brackets
  variable<-strsplit(variable, split = ",") #split string
  variable<-lapply(variable, as.numeric)   #convert to numeric
  variable<-lapply(variable, "+", 1)  #convert rgb scale 
  variable<-lapply(variable, "*", 127.5)
  return (as.list(unlist(variable)))
}

trialdata$Circle1_colour <- lapply(trialdata[,"Circle1_colour"], splitrgb) #apply function
trialdata$Circle2_colour <- lapply(trialdata[,"Circle2_colour"], splitrgb)
  
#make dataframes with rows per r, g, and b
circle1_df <- as.data.frame(lapply(trialdata[,"Circle1_colour"], function(x) t(do.call(cbind, x))))
circle2_df <- as.data.frame(lapply(trialdata[,"Circle2_colour"], function(x) t(do.call(cbind, x))))

#swap rows and columns
circle1_df <- t(circle1_df)
circle2_df <- t(circle2_df)

#name columns
colnames(circle1_df) <- c("r1", "g1", "b1") 
colnames(circle2_df) <- c("r2", "g2", "b2")

circle1_df<-as.data.frame(circle1_df) 
circle2_df<-as.data.frame(circle2_df)

circle1_df$hex1 <- NA
circle2_df$hex2 <- NA

#rgb2hex function
rgb2hex <- function(r, g, b) {rgb(r, g, b, maxColorValue = 255)}
#Get hex in each circle dataframe
circle1_df$hex1 <- apply(circle1_df, 1, function(x) rgb2hex(x[1], x[2], x[3]))
circle2_df$hex2 <- apply(circle2_df, 1, function(x) rgb2hex(x[1], x[2], x[3]))

#bind circle colour dataframes to trialdata
trialdata <- cbind(trialdata, circle1_df, circle2_df)
}

#############################
###DOUBLE PASS CORRELATION###
#############################

#MAKE PASS COLUMNS IN TRIAL DATA 
{
trialdata_passes<-trialdata
names(trialdata_passes)[names(trialdata_passes) == 'similarity'] <- 'firstpass_similarity'
trialdata_passes$secondpass_similarity <- NA 
names(trialdata_passes)[names(trialdata_passes) == 'response_time'] <- 'firstpass_responsetime'
trialdata_passes$secondpass_responsetime<-NA

for (x in 0:(nrow(participants_complete)-1)) { #put 2nd pass data in that column (x = row number in participants_complete)
  trialdata_passes$secondpass_similarity[((x*324)+1):((x*324)+162)] <- trialdata_passes$firstpass_similarity[((x*324)+163):((x*324)+324)] 
  trialdata_passes$secondpass_responsetime[((x*324)+1):((x*324)+162)] <- trialdata_passes$firstpass_responsetime[((x*324)+163):((x*324)+324)] 
}

#delete extra rows
trialdata_passes <- na.omit(trialdata_passes)

#make a column with the mean similarity value from the two passes 
means<-data.frame(similarity=rowMeans(trialdata_passes[,c("firstpass_similarity", "secondpass_similarity")]))
trialdata_passes<-cbind(trialdata_passes,means)
}
#CORRELATION DATAFRAME WITH ONE ROW PER PARTICIPANT
{
#make a dataframe to store correlation values 
pass_correlation <- data.frame(participants_complete$participant_id)
names(pass_correlation)[names(pass_correlation) == 'participants_complete.participant_id'] <- 'participant'

pass_correlation$pearson<- NA
pass_correlation$spearman<- NA

#put correlation values into dataframe
#x <- participant id
getpearson<-function(z){
  temp_df <- subset(trialdata_passes,participant==z)
  pearson <- cor(x=temp_df$firstpass_similarity, y=temp_df$secondpass_similarity, method="pearson")
  return(pearson)
} 

getspearman<-function(z){
  temp_df <- subset(trialdata_passes,participant==z)
  spearman <- cor(x=temp_df$firstpass_similarity, y=temp_df$secondpass_similarity, method="spearman")
  return(spearman)
}

pass_correlation$pearson<-lapply(participants_complete$participant, getpearson) #apply fn
pass_correlation$spearman<-lapply(participants_complete$participant, getspearman)

#make correlation numeric for graphing 
pass_correlation$pearson<-as.numeric(pass_correlation$pearson)
pass_correlation$spearman<-as.numeric(pass_correlation$spearman)

#if error: check trialdata_passes that resulted in NA correlation 
unique(unlist (lapply (pass_correlation, function (x) which (is.na (x))))) 
#trialdata_passes[trialdata_passes$participant == '61652222bfd9351c91a6585a',] 
#drop rows with NA correlation #
pass_correlation<- na.omit(pass_correlation) 

#covert rho to fisher's z
#x=rho value
get_fisherz <-function(x){
  variable<-(0.5*(log(1+x) - log(1-x)) )
  return(variable)
}

pass_correlation$fisherz <- lapply(pass_correlation[,"pearson"], get_fisherz) #apply function
pass_correlation$fisherz<-as.numeric(pass_correlation$fisherz) 
  }
#DOUBLE PASS VISUAL PER PARTICIPANT 
{
#z=participant id
get_passvisual <- function(z)
{
  temp_df <- subset(trialdata_passes,participant==z)
  G <- ggplot(temp_df,aes(x=trialno)) +
    geom_line(aes(y = firstpass_similarity,color="first pass"), size = .5) +
    geom_point(aes(y = firstpass_similarity,color="first pass"), size = 1.5) +
    geom_line(aes(y = secondpass_similarity,color="second pass"), size = .5) +
    geom_point(aes(y = secondpass_similarity,color="second pass"), size = 1.5) +
    geom_point(aes(y=0), color=temp_df$hex1) +
    geom_point(aes(y=0.2), color=temp_df$hex2)+
    ggtitle("double pass visual") +
    labs(title=paste("double pass visual", "\nparticipant: ", z), x="trial number", y="similarity rating")
  G
}

#List_passvisual <- lapply(pass_correlation$participant, get_passvisual)  #get visual for each participants
#List_passvisual <- lapply("614849337165274baeeed45a", get_passvisual)   #get visual for one participant
#List_passvisual    #print visual/s
}
#DOUBLE PASS SCATTERPLOT & CORRELATION PER PARTICIPANT
{
#z=participant id
get_doublepass_scatterplot <- function(z) {
  
  temp_df <- subset(pass_correlation,participant==z)
  pearson<- format(round(temp_df$pearson, 2), nsmall = 2)
  spearman<-format(round(temp_df$spearman, 2), nsmall = 2)
  
  G <- ggplot(subset(trialdata_passes,participant==z),aes(x=firstpass_similarity, y=secondpass_similarity)) +
    geom_point() +
    geom_jitter() +
    geom_smooth(method = "lm") +
    ggtitle("double pass correlation") +
    labs(title=paste("double pass correlation", "\nparticipant: ", z, "\nr =", pearson, "\nrho =", spearman), x="first pass similarity rating", y="second pass similarity rating")
  G
}

List_scatterplot <- lapply(pass_correlation$participant, get_doublepass_scatterplot) #get scatterplot per participant
#List_scatterplot <- lapply("5ec34c953a87ce196520eef7", get_doublepass_scatterplot)  #get scatterplot for one participant
#List_scatterplot  #print scatterplot/s
}
#HISTOGRAM OF DOUBLE PASS CORRELATIONS FOR ALL PARTICIPANTS
{
#save median
medianpass<- round(median(pass_correlation$pearson), 2)

#Histogram of all participants' dp correlation
ggplot(pass_correlation, aes(pearson)) +
  geom_histogram(binwidth = .007, fill= "#016c59") +
  scale_x_continuous(breaks = as.numeric(round(quantile(pass_correlation$pearson),digits = 2))) + 
    theme_pubr(20) +   
  theme(axis.line = element_blank())+
  geom_rangeframe()+
  labs(x="Double-pass correlation (rho)",
       y= "Frequency",
       title= "Histogram of double pass correlations (rho)",
       subtitle = paste("median=", medianpass))
}
#REASONABLE THRESHOLD OF EXCLUSION (Doublepass cor below 2SD)
{
#save mean fisherz
mean_z<- round(mean(pass_correlation$fisherz), 2)
sd_z<- round(sd(pass_correlation$fisherz), 2)

#Participants below 2 standard deviation of mean correlation
outsiders <- pass_correlation[pass_correlation$fisherz<(mean_z-2*sd_z),]

pass_correlation <- subset(pass_correlation, !participant %in% outsiders$participant)
trialdata <- subset(trialdata, !participant %in% participants_incomplete$participant_id)
trialdata <- subset(trialdata, !participant %in% outsiders$participant)
}
#(n=483)
#############################
###ISHIHARA COLOUR TESTING###
#############################
{
#MAKE DATAFRAME WITH ONE ROW PER ANSWER
{
  IH_vars<- c("participant", "Ishi_textbox.text", "imageaddress")
  #"Ishi_trial_keyresp.rt",
  
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

#subsetting a single participant if required
#singleIHdata<-IHdata[IHdata$participant == "61716a16c157db249e36fc46", ]
#head(singleIHdata)

#these participant submitted two files
IH_perperson <- subset(IH_perperson,!participant == "61716a16c157db249e36fc46")
IH_perperson <- subset(IH_perperson,!participant == "614deecf67f2c8ccfe3df23b")

#Distribution of IH scores 
ggplot(IH_perperson, aes(correctA)) +
  geom_histogram() +
  scale_x_continuous(breaks = as.numeric(round(quantile(IH_perperson$correctA),digits = 2))) + 
  theme_pubr() +   
  theme(axis.line = element_blank())+
  geom_rangeframe()+
  labs(x="Ishihara score",
       title= "Histogram of Ishihara score")

#below threshold of normal colour vision
IH_perperson.low <- subset(IH_perperson, correctA <14)

#Removing participants with low Ishihara scores
trialdata_passes <- subset(trialdata_passes, !participant %in% IH_perperson.low$participant)
}
#(n=451)

###################
###DATA ANALYSIS###
###################

#MAKE DATAFRAME OF COLOURPAIRS
{
#get unique hex arrangements
colourpairs <- unique(trialdata_passes[ , c("hex1", "hex2")]) 

#Create unique pair name by concatening first colour with second colour
colourpairs$pair <- str_c(colourpairs$hex1, '',colourpairs$hex2)

#do the same for trialdata_passes
trialdata_passes$pairofcolour <- str_c(trialdata_passes$hex1, '',trialdata_passes$hex2)
}

#Counting times a colour pair was tested
{get_pair_hitcount<-function(x){ #get number of hits on pair of colour (x = colourpair)
  pair_hitcount<-sum(trialdata_passes$pairofcolour == x)
  return(pair_hitcount)
}
colourpairs$pair_hitcount <-lapply(colourpairs$pair, get_pair_hitcount)
colourpairs$pair_hitcount <- as.integer(colourpairs$pair_hitcount)

#hitcount descriptives
summary(colourpairs$pair_hitcount)
sd(colourpairs$pair_hitcount)
}

#ADDING MEAN SIMILARITY PER COLOUR PAIR MAINTAINING DIRECTIONNALITY (AB=/=BA)
{
#mean_similarity<-colourpairs
#mean_similarity$mean<-NA
#for (x in 1:nrow(mean_similarity)) {#  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
#  a<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] & trialdata_passes$hex2==mean_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
#  b<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
#  c<-rbind(a, b) #all similarty ratings for colour pair x
#  mean_similarity$mean[x]<-mean(as.matrix(c))
#}

get_mean_similarity <- function(z){ 
  a<-subset(trialdata_passes, pairofcolour==z) #similarity with exact hex match
  mean<-mean(a$similarity)
  return(mean)
}
colourpairs$mean.similarity<-lapply(colourpairs$pair, get_mean_similarity) 
colourpairs$mean.similarity<- as.numeric(colourpairs$mean.similarity)
}
#ADDING MEAN VARIANCE PER COLOUR PAIR MAINTAINING DIRECTIONNALITY (AB=/=BA)
{get_similarity_variance <- function(z){ 
    a<-subset(trialdata_passes, pairofcolour==z) #similarity with exact hex match
    variance<-var(a$similarity)
    return(variance)
  }
  colourpairs$variance.similarity<-lapply(colourpairs$pair, get_similarity_variance) 
  colourpairs$variance.similarity<- as.numeric(colourpairs$variance.similarity)
}

#NOT IN USE 
{
#DO NOT USE -- FACTORS SET AT TOP OF SCRIPT-- MAKING COLOURS AS FACTORS FOR GRAPHING
{# Create blank matrix 
color.mat.df <- matrix(NA, ncol = 93, nrow = 93)
# setting all give colours as both row and column names
colnames(color.mat.df) <- rownames(color.mat.df) <- unique(trialdata$hex1)

# fill matrix
matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    row <- data[i,]
    matrix.df[row$hex1,row$hex2] <- row$mean.similarity
  }
  return(matrix.df)
}

gp.mean.data_vars <- c("hex1", "hex2", "mean.similarity")
gp.mean.data <- colourpairs[gp.mean.data_vars]

group.mean.mat.df <- matrix.df.fill(gp.mean.data,color.mat.df)
group.mean.mat.df <- as.data.frame(group.mean.mat.df)

# for visualisation purposes, make a column value indicating correlation with first row
row.cor <- function(df){
  v.cor <- vector()
  for(i in 1:nrow(df)){
    v.cor <- c(v.cor, cor(df[1],df[i]))
  }    
  return(v.cor)
}

row.factors <- function(colors, correlations){
  colors$cor <- correlations
  ordered <- colors[order(-colors$cor),]
  return(ordered$colour)
}

colours_disordered <- as.data.frame(unique(colourpairs$hex1))
colours_disordered <- rename(colours_disordered, colour = 'unique(colourpairs$hex1)')

row.facs <- row.factors(colours_disordered,row.cor(group.mean.mat.df))
}
#MAKE DATAFRAME WITH MEAN SIMILARITY PER COLOUR PAIR (AB==BA) - Not in Use
{mean_similarity<-colourpairs
mean_similarity$meanas<-NA

for (x in 1:nrow(mean_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] & trialdata_passes$hex2==mean_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
  b<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
  c<-rbind(a, b) #all similarty ratings for colour pair x
  mean_similarity$meanas[x]<-mean(as.matrix(c))
}
}
#SIMILARITY MATRIX VISUALISATION
{ggplot(mean_similarity) +
  aes(x = hex1, y = hex2, fill = mean) +
  geom_raster() +
  scale_fill_gradientn(colors = rainbow(7),breaks= c(0,1,2,3,4,5,6,7))+
  theme_pubr()+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(mean_similarity$hex1))))+
  theme(axis.text.y = element_text(size= 7, colour=sort(unique(mean_similarity$hex2))))
}
#MAKE DATAFRAME WITH MEDIAN SIMILARITY PER COLOUR PAIR (AB==BA) - Not in Use
{
median_similarity<-colourpairs
median_similarity$median<-NA
  
for (x in 1:nrow(median_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
    a<-subset(trialdata_passes,trialdata_passes$hex1==median_similarity$hex1[x] & trialdata_passes$hex2==median_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
    b<-subset(trialdata_passes,trialdata_passes$hex1==median_similarity$hex2[x] & trialdata_passes$hex2==median_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
    c<-rbind(a, b) #all similarty ratings for colour pair x
    median_similarity$median[x]<-median(as.matrix(c))
  }
}
#MEDIAN SIMILARITY MATRIX - Not in use
{ggplot(median_similarity) +
    aes(x = hex1, y = hex2, fill = median) +
    geom_raster() +
    scale_fill_gradient(low= "grey", high= "black",breaks= c(0,1,2,3,4,5,6,7))+
    theme_pubr()+
    theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(median_similarity$hex1))))+
    theme(axis.text.y = element_text(size= 7, colour=sort(unique(median_similarity$hex2))))
}
#MAKE DATAFRAME WITH MEAN VARIANCE PER COLOUR PAIR (AB==BA) - Not in Use
{
mean_variance<-colourpairs
mean_variance$var<-NA
  
for (x in 1:nrow(mean_variance)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
    a<-subset(trialdata,trialdata$hex1==mean_variance$hex1[x] & trialdata$hex2==mean_variance$hex2[x], c(similarity)) #similarity with exact hex match
    b<-subset(trialdata,trialdata$hex1==mean_variance$hex2[x] & trialdata$hex2==mean_variance$hex1[x], c(similarity)) #similarity with flipped hex order
    c<-rbind(a, b) #all similarty ratings for colour pair x
    mean_variance$var[x]<-var(c)
}
ggplot(mean_variance) +
  aes(x = hex1, y = hex2, fill = var) +
  geom_raster() +
  scale_fill_gradient(name="Variance" ,low = "grey", high = "black", na.value = "green")+
  theme_pubr()+
  theme(axis.text.x= element_text(size= 6, angle=90, colour=row.facs))+
  scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
  theme(axis.text.y = element_text(size= 6, colour=row.facs))+
  theme(legend.position = "left")+
  labs(x= "Colour presented first", y= "Colour presented second")
}

summary(mean_variance$var)
sd(mean_variance$var)

#Get Standard Error of Mean of Similarity
#standard_error <- function(x) sd(x) / sqrt(length(x))
mean_variance$sem <- sqrt(mean_variance$var)/sqrt(mean_variance$pair_hitcount)

#MAKE DATAFRAME WITH ASYMMETRY INDEX (AsIn) for each participant FULL FORMULA
{
#Calculating absolute difference between first and second pass similatirity ratings  
trialdata_passes$abs <- abs(trialdata_passes$firstpass_similarity-trialdata_passes$secondpass_similarity) 
#Creating unique ID for each pair by combining participant ID and Colourpair 
trialdata_passes$Colourpairperparticipant <- str_c(trialdata_passes$participant, '',trialdata_passes$Colourpair)

#selecting variables for asymmetry analysis
AsIndata_vars <- c("participant", "Colourpair", "Temporder", "hex1", "hex2", "similarity", "abs", "Colourpairperparticipant")
#making dataframe
AsIndata <- trialdata_passes[AsIndata_vars]

#changing to wide so that One row per colour pair per participant (4 ratings per row, first and second pass and reverse order)
AsIndata_wide <- reshape(AsIndata, idvar = "Colourpairperparticipant", timevar = "Temporder", direction = "wide")

#renaming because temporder first second makes it confusing with first/second pass
names(AsIndata_wide)[names(AsIndata_wide) == "similarity.first"] <- "mean13" #similarity.first was the mean of the pair for the first temporder so mean13
names(AsIndata_wide)[names(AsIndata_wide) == "similarity.second"] <- "mean24"

#Create unique pair name by concatening first colour with second colour
AsIndata_wide$pair13 <- str_c(AsIndata_wide$hex1.first, '',AsIndata_wide$hex2.first)
AsIndata_wide$pair24 <- str_c(AsIndata_wide$hex1.second, '',AsIndata_wide$hex2.second)

#calculating Asymmetry Index as per Nao's formula (M13 -M24)/((A13+A24)+1)
AsIndata_wide$AsIn13 <- (AsIndata_wide$mean13 - AsIndata_wide$mean24)/
  ((AsIndata_wide$abs.first + AsIndata_wide$abs.second) + 1)
#calculating Asymmetry Index as per Nao's formula (M24 -M13)/((A13+A24)+1)
AsIndata_wide$AsIn24 <- (AsIndata_wide$mean24 - AsIndata_wide$mean13)/
  ((AsIndata_wide$abs.first + AsIndata_wide$abs.second) + 1)
}
}

#MAKE DATAFRAME WITH ASYMMETRY INDEX (AsIn) for each participant SIMPLIFIED FORMULA 
{
  #Calculating absolute difference between first and second pass similatirity ratings  
  trialdata_passes$abs <- abs(trialdata_passes$firstpass_similarity-trialdata_passes$secondpass_similarity) 
  #Creating unique ID for each pair by combining participant ID and Colourpair 
  trialdata_passes$Colourpairperparticipant <- str_c(trialdata_passes$participant, '',trialdata_passes$Colourpair)
  
  #selecting variables for asymmetry analysis
  AsIndata_vars <- c("participant", "Colourpair", "Temporder", "hex1", "hex2", "similarity", "abs", "Colourpairperparticipant")
  #making dataframe
  AsIndata <- trialdata_passes[AsIndata_vars]
  
  #changing to wide so that One row per colour pair per participant (4 ratings per row, first and second pass and reverse order)
  AsIndata_wide <- reshape(AsIndata, idvar = "Colourpairperparticipant", timevar = "Temporder", direction = "wide")
  
  #renaming because temporder first second makes it confusing with first/second pass
  names(AsIndata_wide)[names(AsIndata_wide) == "similarity.first"] <- "mean13" #similarity.first was the mean of the pair for the first temporder so mean13
  names(AsIndata_wide)[names(AsIndata_wide) == "similarity.second"] <- "mean24"
  
  #Create unique pair name by concatening first colour with second colour
  AsIndata_wide$pair13 <- str_c(AsIndata_wide$hex1.first, '',AsIndata_wide$hex2.first)
  AsIndata_wide$pair24 <- str_c(AsIndata_wide$hex1.second, '',AsIndata_wide$hex2.second)
  
  #calculating Asymmetry Index as per Simplified formula (M13 -M24)
  AsIndata_wide$AsIn13 <- (AsIndata_wide$mean13 - AsIndata_wide$mean24)
  
  #calculating Asymmetry Index as per Simplified formula (M24 -M13)
  AsIndata_wide$AsIn24 <- (AsIndata_wide$mean24 - AsIndata_wide$mean13)
}

#GET AsIn in Colourpairs DATAFRAME
{
get_mean_asymmetry <- function(z){ 
  a<-subset(AsIndata_wide, pair13==z) #getting all the instance in pair13 matching the pair of hexcodes
  a<- a["AsIn13"] #only keeping the asymmetry score of interest 
  names(a)[names(a)== "AsIn13"] <- "AsIn" #rename to be able to rbind with b
  b<-subset(AsIndata_wide, pair24==z) #repeating with pair24
  b<- b["AsIn24"] # again keeping only AsIn24 
  names(b)[names(b)== "AsIn24"] <- "AsIn" #rename to be able to rbind with a
  c <-rbind(a, b)
  AsIn <- mean(c$AsIn)
  return(AsIn)
}

colourpairs$mean.asymmetry<-lapply(colourpairs$pair, get_mean_asymmetry) #apply function
colourpairs$mean.asymmetry<- as.numeric(colourpairs$mean.asymmetry) #make it numeric
}

#GET AsIn Variance in Colourpairs DATAFRAME
{get_var_asymmetry <- function(z) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(AsIndata_wide, pair13==z) #getting all the instance in pair13 matching the pair of hexcodes
  a<- a["AsIn13"] #only keeping the asymmetry score of interest 
  names(a)[names(a)== "AsIn13"] <- "AsIn" #rename to be able to rbind with b
  b<-subset(AsIndata_wide, pair24==z) #repeating with pair24
  b<- b["AsIn24"] # again keeping only AsIn24 
  names(b)[names(b)== "AsIn24"] <- "AsIn" #rename to be able to rbind with a
  c <-rbind(a, b) #combine both subset to have complete 
  var.AsIn <- var(c$AsIn)
  return(var.AsIn)
}

colourpairs$var.asymmetry<-lapply(colourpairs$pair, get_var_asymmetry) #apply function
colourpairs$var.asymmetry<- as.numeric(colourpairs$var.asymmetry) #make it numeric

colourpairs$sem.asymmetry <- sqrt(colourpairs$var.asymmetry)/sqrt(colourpairs$pair_hitcount)
}

## T-TESTING UNCORRECTED 
colourpairs$t <- NA
colourpairs$t <- colourpairs$mean.asymmetry/(sqrt(colourpairs$var.asymmetry)/sqrt(colourpairs$pair_hitcount))


#MAKE DATAFRAME WITH ASYMMETRY PER COLOUR PAIR (AB=BA) - NOT IN USE
{#gathering AsIn values using a for loop
mean_asymmetry<-mean_variance
mean_asymmetry$meanas<-NA

for (x in 1:nrow(mean_asymmetry)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(AsIndata_wide,AsIndata_wide$hex1.first==mean_asymmetry$hex1[x] & AsIndata_wide$hex1.second==mean_asymmetry$hex2[x], c(AsIn)) #similarity with exact hex match
  b<-subset(AsIndata_wide,AsIndata_wide$hex1.first==mean_asymmetry$hex2[x] & AsIndata_wide$hex1.second==mean_asymmetry$hex1[x], c(AsIn)) #similarity with flipped hex order
  c<-rbind(a, b) #all similarty ratings for colour pair x
  mean_asymmetry$meanas[x]<-mean(as.matrix(c))
}

#mean_asymmetry$hex1 <- with(mean_asymmetry, factor(hex1, levels = row.facs))
#mean_asymmetry$hex2 <- with(mean_asymmetry, factor(hex2, levels = row.facs))
}

#### MATRIX WITH ASYMMETRY VALUES #### DOES NOT WORK IF FACTORS ALREADY APPLIED
{# Create blank matrix 
  asym.matrix <- matrix(NA, ncol = 93, nrow = 93)
  # setting all give colours as both row and column names
  colnames(asym.matrix) <- rownames(asym.matrix) <- unique(trialdata$hex1)
  
  # fill matrix with asymmetry index values
  matrix.df.fill <- function(data,matrix.df){
    for(i in 1:nrow(data)){
      row <- data[i,]
      matrix.df[row$hex1,row$hex2] <- row$mean.asymmetry
    }
    return(matrix.df)
  }
  
  asym.data_vars <- c("hex1", "hex2", "mean.asymmetry")
  asym.data <- colourpairs[asym.data_vars]
  
  asym.matrix <- matrix.df.fill(asym.data,asym.matrix)
  asym.matrix.df <- as.data.frame(asym.matrix)
}

#calculate mean of each column
color.names <- colnames(asym.matrix.df) #get list of colours
asym_percolour.df<- as.data.frame(color.names) #make it a dataframe
asym_percolour.df$average.asymmetry <-colMeans(asym.matrix.df) # add new variable to dataframe with mean of each column in matrix

#calculate variance of each column
asym_percolour.df$Variance <- colVars(asym.matrix)

#replace NAs with zero for graphing 
asym_percolour.df[is.na(asym_percolour.df)] = 0

#RESCALING THE DATA TO DISSIMILARITY
{colourpairs$pair.reversed <- str_c(colourpairs$hex2, '',colourpairs$hex1)

get_mean.sim <- function(z){ 
  a<-subset(colourpairs, pair==z) #getting all the instance in pair matching the pair of hexcodes
  a<- a["mean.similarity"] #only keeping the similarity score of interest 
  b<-subset(colourpairs, pair.reversed==z) #repeating with pair reversed 
  b<- b["mean.similarity"] # again keeping only scores of reversed pairs
  c <-rbind(a, b) #all similarity ratings for one combination 
  mean.sim <- mean(c$mean.similarity)
  return(mean.sim)
}

colourpairs$mean.sim<-lapply(colourpairs$pair, get_mean.sim) 
colourpairs$mean.sim<- as.numeric(colourpairs$mean.sim)
colourpairs$mean.dissim <- 7- colourpairs$mean.sim
}
##MAKE DISSIMILARITY MATRIX 
{# Create blank matrix 
dissim.matrix <- matrix(3.5, ncol = 93, nrow = 93)
# setting all give colours as both row and column names
colnames(dissim.matrix) <- rownames(dissim.matrix) <- unique(trialdata$hex1)

# fill matrix with similarity values
matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    row <- data[i,]
    matrix.df[row$hex1,row$hex2] <- row$mean.dissim
  }
  return(matrix.df)
}

dissim.data_vars <- c("hex1", "hex2", "mean.dissim")
dissim.data <- colourpairs[dissim.data_vars]
dissim.data$mean.dissim <- as.numeric(dissim.data$mean.dissim)

dissim.matrix <- matrix.df.fill(dissim.data,dissim.matrix)
dissim.matrix.df <- as.data.frame(dissim.matrix)
}
#MDS 2d
{fit <- cmdscale(dissim.matrix.df, eig= TRUE, k =2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  
  fit.2df<- data.frame(fit$points)
  fit.2df$colour <- rownames(fit.2df)
  fit.2df$colour  <- with(fit.2df, factor(colour, levels = row.facs))
  
  fit.plot <- ggplot(data = fit.2df, aes(x = X1, y = X2, color=colour)) + geom_point() +
    scale_color_manual(values = c(row.facs))+
    guides(col = "none") +
    theme_pubr()
  fit.plot
  plot(x, y)
}

#3d plot
{fit <- cmdscale(dissim.matrix.df, eig= TRUE, k =3)
x <- fit$points[, 1]
y <- fit$points[, 2]
z <- fit$points[, 3]

colors <- colnames(dissim.matrix)

plot3d(x,y,z, col=colors, size = 20)
}

#MAKE DATAFRAME WITH MDS COORDINATES
{mds_coord <- as.data.frame(colors)
mds_coord$x <- x
mds_coord$y <- y
mds_coord$z <- z

#calculate distance in MDS 3d 
get_mds.distance <- function(c1,c2){ 
  a <-subset(mds_coord, colors == c1) #coord colour 1
  b <-subset(mds_coord, colors == c2) #coord colour 2
  mds.distance <- sqrt((a$x-b$x)^2+(a$y-b$y)^2+(a$z-b$z)^2)
  return(mds.distance)
}
colourpairs$mds.distance <- apply(colourpairs,1, function(x) get_mds.distance(x[1], x[2])) #apply function
colourpairs$mds.distance<- as.numeric(colourpairs$mds.distance) #make it numeric
}

################
###REGRESSION###
################
{
#Knocking down diagonal 
colourpairs$diagonal <- colourpairs$hex1==colourpairs$hex2  

regr.data <- colourpairs[colourpairs$diagonal==FALSE,]

#linear model 
m1 <- lm(abs(mean.asymmetry)~ mds.distance, 
         data= regr.data)
summary(m1)
lm.beta(m1)
md <- modelDiagnostics(m1, ev.perc=.005)
plot(md, ncol=2 , ask= FALSE)

visreg(m1, xvar= "mds.distance", gg= TRUE)+
  theme_pubr()
#quadratic model
m2 <- lm(abs(mean.asymmetry)~ poly(mds.distance,2), 
         data= regr.data)
summary(m2)
md2 <- modelDiagnostics(m2, ev.perc=.005)
plot(md2, ncol=2 , ask= FALSE)

#linear vs quadratic comparison
AIC(m1, m2)
BIC(m1,m2)

#cubic model
m3 <- lm(abs(mean.asymmetry)~ poly(mds.distance,3), 
         data= regr.data)
summary(m3)
md3 <- modelDiagnostics(m3, ev.perc=.005)
plot(md3, ncol=2 , ask= FALSE)

#quadratic vs cubic comparison
AIC(m2, m3)
BIC(m2,m3)

#4th model
m4 <- lm(abs(mean.asymmetry)~ poly(mds.distance,4), 
         data= regr.data)
summary(m4)
md4 <- modelDiagnostics(m4, ev.perc=.005)
plot(md4, ncol=2 , ask= FALSE)

#cubic vs 4th comparison
AIC(m3, m4)
BIC(m3,m4)

visreg(m3, xvar= "mds.distance", gg= TRUE)+
  scale_x_continuous(breaks = seq(0,7,by=1))+
  labs(x= "MDS distance", y= "Asymmetry (absolute mean)")+
  theme_pubr()


##MULTIPLE REGRESSION 
#Adding similarity and its variance to model to control
mlr <- lm(abs(mean.asymmetry)~ poly(mds.distance,3) + mean.similarity + variance.similarity, 
          data = regr.data)
summary(mlr)

mlr <- lm(abs(mean.asymmetry)~ poly(mds.distance,1) + mean.similarity + variance.similarity, 
          data = regr.data)
summary(mlr)
lm.beta(mlr)
#Adding interaction term
mlr.interaction <- lm(abs(mean.asymmetry)~ mds.distance + mean.similarity + variance.similarity+
                        mds.distance*mean.similarity*variance.similarity, 
                      data = regr.data)
summary(mlr.interaction)

mlr.poly.interaction <- lm(abs(mean.asymmetry)~ poly(mds.distance,3) + mean.similarity + variance.similarity+
                             poly(mds.distance,3)*mean.similarity*variance.similarity, 
                           data = regr.data)
summary(mlr.poly.interaction)
modelDiagnostics(mlr.interaction) %>% plot(ncol=2, ask=FALSE)

AIC(mlr.interaction, mlr.poly.interaction)
BIC(mlr.interaction, mlr.poly.interaction)

AIC(m3, mlr.poly.interaction)
BIC(m3, mlr.poly.interaction)

median.asym <- median(abs(regr.data$mean.asymmetry))

low.asym <- regr.data[abs(regr.data$mean.asymmetry)< median.asym,]
high.asym <- regr.data[abs(regr.data$mean.asymmetry)> median.asym,]

mean(high.asym$mds.distance)
mean(low.asym$mds.distance)

mean(high.asym$mean.similarity)
mean(low.asym$mean.similarity)

mean(high.asym$variance.similarity)
mean(low.asym$variance.similarity)

}

###################
###VISUALISATION###
###################

#APPLY COLOUR FACTORS TO COLOURPAIRS DATAFRAME
colourpairs$fhex1 <- with(colourpairs, factor(hex1, levels = row.facs))
colourpairs$fhex2 <- with(colourpairs, factor(hex2, levels = row.facs))

#creating one object with setting to ensure they all have the same, also only one thing to update
matrix.plot.settings <- list(geom_raster(),
                        theme_pubr(25),
                        theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs)),
                        theme(axis.text.y = element_text(size= 9, colour=row.facs)),
                        theme(legend.position = "bottom", legend.key.size = unit(1.5,'cm')),
                        scale_x_discrete(labels=block_rep), scale_y_discrete(labels=block_rep),
                        labs(x= "Colour presented first", y= "Colour presented second"))

#MATRIX PLOT OF HITS FOR COLOUR
{
  #heatmap of hitcount (the more a pair is tested, the 'hotter' it gets)
  ggplot(colourpairs) +
    aes(x = fhex1, y = fhex2, fill= pair_hitcount) +
    matrix.plot.settings +
    scale_fill_fermenter(palette = "Greys", direction = 1, name= "Times tested")
  
  #ggplot(colourpairs) + aes(x = hex1, y = hex2, fill= pair_hitcount) + geom_raster() +  theme_pubr()+  theme(axis.text.x=element_text(angle=90,hjust=1, size=7), axis.text.y = element_text(size = 7))
  
  #distribution of hitcounts
  ggplot(colourpairs) +
    aes(x= pair_hitcount) +
    geom_bar(fill= "#016450") +
    theme_pubr()+
    labs(y='Frequency(number of pairs)', x='Times tested')
}

#VISUALISE MEAN SIMILARITY
{
#B&W 
plot.similarity <- ggplot(colourpairs) +
    aes(x = fhex1, y = fhex2, fill = mean.similarity) +
    scale_fill_distiller(name= "Similarity",palette = "Greys", direction = 1,breaks= c(0,1,2,3,4,5,6,7))+
    matrix.plot.settings 
  
ggsave("mean similarity matrix.png",plot.similarity,height=13,width=17)
}

#VISUALISE SIMILARITY VARIANCE
{
#B&W gradient
plot.variance.sim <- ggplot(colourpairs) +
    aes(x = fhex1, y = fhex2, fill = variance.similarity) +
    scale_fill_distiller(name= "Variance",palette = "Greys", direction = 1)+
    matrix.plot.settings 

ggsave("variance similarity matrix.png",plot.variance.sim,height=13,width=17)

#B&W binned/stepped 
plot.variance.sim.bin <- ggplot(colourpairs) +
  aes(x = fhex1, y = fhex2, fill = variance.similarity) +
  scale_fill_fermenter(name= "Variance",palette = "Greys", direction = 1)+
  matrix.plot.settings 

ggsave("variance similarity matrix.bin.png",plot.variance.sim.bin,height=13,width=17)
}

#MAKE ASYMMETRY MATRIX ALL PARTICIPANTS -- To be refined
{

#gradient colour scale  
plot.gradient.asym <- ggplot(colourpairs) +
    aes(x = fhex1, y = fhex2, fill = mean.asymmetry) +
    scale_fill_distiller(name= "Asymmetry", palette = "PiYG") +
    matrix.plot.settings 

ggsave("gradient asym matrix.png",plot.gradient.asym,height=17,width=17)

#binned colour scale
ggplot(colourpairs) +
    aes(x = fhex1, y = fhex2, fill = mean.asymmetry) +
    scale_fill_fermenter(breaks = c(-2,-1,1,2), name= "Asymmetry", palette = "PiYG") +
    matrix.plot.settings   
  
    }

#Visualise MDS distance in matrix 
{mds.matrix <- ggplot(colourpairs) +
    aes(x = hex1, y = hex2, fill = mds.distance) +
    scale_fill_distiller(name= "MDS distance",palette = "Greys", direction = -1,breaks= c(0,1,2,3,4,5,6,7))+
    matrix.plot.settings
ggsave("mds.matrix.png",mds.matrix,height=17,width=17)
  }

#FUNNEL PLOT Asymmetry x Hitcount
ggplot(colourpairs) +
  aes(y = mean.asymmetry, x = pair_hitcount) +
  geom_point(position = "jitter") +
  geom_hline(yintercept = 0, colour = "red")+
  theme_pubr()

#FUNNEL PLOT Asymmetry x SEM
ggplot(colourpairs) +
  aes(y = mean.asymmetry, x = sem.asymmetry) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red")+
  geom_abline(slope=1, colour = "blue")+
  geom_abline(slope=2, colour = "green")+
  geom_abline(slope=3, colour = "cyan")+
  theme_pubr()+
  ylim(0,3)

#FUNNEL PLOT Asymmetry x SEM zoomed in
ggplot(colourpairs) +
  aes(y = mean.asymmetry, x = sem.asymmetry) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red")+
  geom_abline(slope=1, colour = "blue")+
  geom_abline(slope=2, colour = "green")+
  geom_abline(slope=3, colour = "cyan")+
  theme_pubr()+
  ylim(0,3)+
  xlim(0,1.5)

# density plot of mean asymmetry 
ggplot(colourpairs)+
  aes(x=mean.asymmetry)+
  geom_density()+
  theme_pubr()+
  geom_vline(xintercept = 0, colour ="grey")


#t-score matrix
ggplot(colourpairs)+
  aes(x = fhex1, y = fhex2, fill = t) +
  scale_fill_distiller(name= "t-score", palette = "PiYG") +
  matrix.plot.settings

#t-score x Variance zoomed in 
ggplot(colourpairs) +
  aes(y = t, x = var.asymmetry) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red")+
    geom_hline(yintercept = 2, colour = "blue")+
    geom_hline(yintercept = 4, colour = "green")+
  theme_pubr()+
  ylim(0,6)+
  xlim(0,1)
  
#t-score x Times tested 
ggplot(colourpairs) +
  aes(y = t, x = pair_hitcount) +
  geom_point() +
  theme_pubr()+
  ylim(0,15)
 

#Boxplot of t-values per times tested
ggplot(colourpairs) +
  aes(y = t)+
  geom_boxplot(aes(group = factor(pair_hitcount)))+
  theme_pubr()+
  coord_cartesian(ylim= c(0,15))


## p-TESTING UNCORRECTED --- WIP
colourpairs$p <- NA
colourpairs$p <- 2*pt(abs(colourpairs$t), df= colourpairs$pair_hitcount-1, lower.tail = FALSE)
colourpairs$p <- round(colourpairs$p, 4)

candidates_p <- subset(colourpairs, p<.05)

#p vs times tested zoomed in low p values
ggplot(colourpairs) +
  aes(y = p, x = pair_hitcount) +
  geom_point() +
  theme_pubr()+
  ylim(0,.1)

#p-testing CORRECTED --- WIP 
colourpairs$p.adjust <- NA
colourpairs$p.adjust <- p.adjust(colourpairs$p, method = "holm", n= length(colourpairs$p))

#Candidate pairs
candidates <- subset(colourpairs, colourpairs$t>4)

ggplot(trialdata) +
  aes(x = similarity, fill = participant) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(participant))

#Standard deviation of similarity ratings per colour pair
mean_variance$sd <- sqrt(mean_variance$var)

#Similarity st.dev x Times tested
ggplot(mean_variance) +
  aes(x = pair_hitcount, y = sd) +
  geom_point(shape = "circle", position = "jitter") +
  geom_smooth(span = 0.75) +
  labs(x = "Times tested", y = "St dev in similarity") +
  theme_pubr()


#### MATRIX WITH ASYMMETRY VALUES #### DOES NOT WORK IF FACTORS ALREADY APPLIED
{# Create blank matrix 
  asym.matrix <- matrix(NA, ncol = 93, nrow = 93)
  # setting all give colours as both row and column names
  colnames(asym.matrix) <- rownames(asym.matrix) <- unique(trialdata$hex1)
  
  # fill matrix with asymmetry index values
  matrix.df.fill <- function(data,matrix.df){
    for(i in 1:nrow(data)){
      row <- data[i,]
      matrix.df[row$hex1,row$hex2] <- row$mean.asymmetry
    }
    return(matrix.df)
  }
  
  asym.data_vars <- c("hex1", "hex2", "mean.asymmetry")
  asym.data <- colourpairs[asym.data_vars]
  
  asym.matrix <- matrix.df.fill(asym.data,asym.matrix)
  asym.matrix.df <- as.data.frame(asym.matrix)
  }

#calculate mean of each column
color.names <- colnames(asym.matrix.df) #get list of colours
asym_percolour.df<- as.data.frame(color.names) #make it a dataframe
asym_percolour.df$average.asymmetry <-colMeans(asym.matrix.df) # add new variable to dataframe with mean of each column in matrix

#apply factors to asym_percolour dataframe
asym_percolour.df$color.names <- with(asym_percolour.df, factor(color.names, levels = row.facs)) #make colour variable as factor for graphing

#calculate variance of each column
asym_percolour.df$Variance <- colVars(asym.matrix, na.rm = TRUE)

#replace NAs with zero for graphing 
asym_percolour.df[is.na(asym_percolour.df)] = 0

#visualise average asymmetry per column 
{avg.asym.percolumn <- ggplot(asym_percolour.df)+
  aes(x = color.names, y= -average.asymmetry, fill= color.names)+
  theme_pubr(25)+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank())+ 
  geom_hline(aes(yintercept =0))+
  labs(y= "Asymmetry average per row")
ggsave("avg.asym.percolumn.png",avg.asym.percolumn,height=7,width=17)  
  }
#visualise asymmetry variance per column 
{var.asym.perrow <- ggplot(asym_percolour.df)+
  aes(x = color.names, y= Variance, fill= color.names)+
  theme_pubr(20)+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x= element_blank())+
  labs(y= "Asymmetry variance")
ggsave("var.asym.perrow.png",var.asym.perrow,height=4,width=17)
  }
#Calculating rough tscore with population equal to 92 colours 
asym_percolour.df$t <- NA
asym_percolour.df$t <- asym_percolour.df$average.asymmetry/(sqrt(asym_percolour.df$Variance)/sqrt(92))
#asym_percolour.df$t <- abs(asym_percolour.df$t)
asym_percolour.df[is.na(asym_percolour.df)] = 0 #for graphing

#visualise asymmetry tscore per column 
{ggplot(asym_percolour.df)+
  aes(x = color.names, y= t, fill= color.names)+
  theme_pubr()+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))
}


##GREEN
#Isolating one colour for further analysis
green.df <- colourpairs[colourpairs$hex1=="#40A144",] #subsetting
mean(single.colour.df$mean.asymmetry) #calculating mean for sanity check 
var(single.colour.df$mean.asymmetry) #caculating variance for sanity check 
mean(single.colour.df$mean.asymmetry)/(sqrt(var(single.colour.df$mean.asymmetry))/sqrt(92)) #calculating tscore for sanity check 

green.df <- as.data.frame(green.df)
green.df$hex2 <- with(green.df, factor(hex2, levels = row.facs)) #make colour variable as factor for graphing

#distribution of asymmetry values for one colour
{green.dist <- ggplot(green.df)+
  aes(mean.asymmetry)+
  geom_histogram(fill= unique(green.df$hex1))+ #make colour of histogram the colour plotted
  scale_x_continuous(breaks = as.numeric(round(quantile(green.df$mean.asymmetry),digits = 2)))+ #change axis to display quantile value instead of arbitrary values
  labs(x = paste(unique(green.df$hex1)), y = "Frequency")+
  theme_pubr(20)+
    ylim(0,12)
  ggsave("green.dist.png",green.dist,height=4,width=17)
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of hitcount
{ggplot(green.df) +
  aes(x = hex2, fill = pair_hitcount, y = mean.asymmetry) +
  geom_col() +
  scale_fill_distiller(palette = "PuBuGn", direction = 1) +
  labs(
    x = "Colour compared to",
    y = "Asymmetry Index (averaged across participants)",
    title = paste("Asymmetries of ", unique(green.df$hex1), sep = ""),
    fill = "Number of participants") +
  theme_pubr()+
  scale_x_discrete(labels=block_rep)+
  theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs))
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of SEM
{green.bar <- ggplot(green.df) +
  aes(x = hex2, fill = sem.asymmetry, y = mean.asymmetry) +
  geom_col() +
  scale_fill_distiller(palette = "PuBuGn", direction = -1) +
  labs(
    x = "Colour compared to",
    y = "Asymmetry Index (averaged across participants)",
    title = paste("Asymmetries of ", unique(green.df$hex1), sep = ""),
    fill = "SEM asymmetry") +
  theme_pubr(15)+
  scale_x_discrete(labels=block_rep)+
  theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs),
        legend.key.size = unit(1, "cm"))
  ggsave("green.bar.png", green.bar, height = 7, width = 17)
}


###RED###
#Isolating one colour for further analysis
red.df <- colourpairs[colourpairs$hex1=="#DA0B27",] #subsetting
mean(red.df$mean.asymmetry) #calculating mean for sanity check 
var(red.df$mean.asymmetry) #caculating variance for sanity check 
mean(red.df$mean.asymmetry)/(sqrt(var(red.df$mean.asymmetry))/sqrt(92)) #calculating tscore for sanity check 

red.df <- as.data.frame(red.df)
red.df$hex2 <- with(red.df, factor(hex2, levels = row.facs)) #make colour variable as factor for graphing

#distribution of asymmetry values for one colour
{red.dist <- ggplot(red.df)+
    aes(mean.asymmetry)+
    geom_histogram(fill= unique(red.df$hex1))+ #make colour of histogram the colour plotted
    scale_x_continuous(breaks = as.numeric(round(quantile(red.df$mean.asymmetry),digits = 2)))+ #change axis to display quantile value instead of arbitrary values
    labs(x = paste(unique(red.df$hex1)), y = "Frequency")+
    theme_pubr(20)+
    ylim(0,12)
  ggsave("red.dist.png",red.dist,height=4,width=17)
  
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of hitcount
{ggplot(red.df) +
    aes(x = hex2, fill = pair_hitcount, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = 1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(red.df$hex1), sep = ""),
      fill = "Number of participants") +
    theme_pubr(20)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs))
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of SEM
{red.bar <- ggplot(red.df) +
    aes(x = hex2, fill = sem.asymmetry, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = -1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(red.df$hex1), sep = ""),
      fill = "SEM asymmetry") +
    theme_pubr(15)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs),
          legend.key.size = unit(1, "cm"))
  ggsave("red.bar.png", red.bar, height = 7, width = 17)
}

###YELLOW###
#Isolating one colour for further analysis
yellow.df <- colourpairs[colourpairs$hex1=="#F4D000",] #subsetting
mean(yellow.df$mean.asymmetry) #calculating mean for sanity check 
var(yellow.df$mean.asymmetry) #caculating variance for sanity check 
mean(yellow.df$mean.asymmetry)/(sqrt(var(yellow.df$mean.asymmetry))/sqrt(92)) #calculating tscore for sanity check 

yellow.df <- as.data.frame(yellow.df)
yellow.df$hex2 <- with(yellow.df, factor(hex2, levels = row.facs)) #make colour variable as factor for graphing

#distribution of asymmetry values for one colour
{yellow.dist <- ggplot(yellow.df)+
    aes(mean.asymmetry)+
    geom_histogram(fill= unique(yellow.df$hex1))+ #make colour of histogram the colour plotted
    scale_x_continuous(breaks = as.numeric(round(quantile(yellow.df$mean.asymmetry),digits = 2)))+ #change axis to display quantile value instead of arbitrary values
    labs(x = paste(unique(yellow.df$hex1)), y = "Frequency")+
    theme_pubr(20)+
    ylim(0,12)
  ggsave("yellow.dist.png", yellow.dist, height=4,width=17)
  
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of hitcount
{ggplot(yellow.df) +
    aes(x = hex2, fill = pair_hitcount, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = 1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(yellow.df$hex1), sep = ""),
      fill = "Number of participants") +
    theme_pubr(20)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs))
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of SEM
{yellow.bar <-ggplot(yellow.df) +
    aes(x = hex2, fill = sem.asymmetry, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = -1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(yellow.df$hex1), sep = ""),
      fill = "SEM asymmetry") +
    theme_pubr(15)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs),
          legend.key.size = unit(1, "cm"))
  ggsave("yellow.bar.png", yellow.bar, height = 7, width = 17)
}

###BLUE###
#Isolating one colour for further analysis
blue.df <- colourpairs[colourpairs$hex1=="#02529A",] #subsetting
mean(blue.df$mean.asymmetry) #calculating mean for sanity check 
var(blue.df$mean.asymmetry) #caculating variance for sanity check 
mean(blue.df$mean.asymmetry)/(sqrt(var(blue.df$mean.asymmetry))/sqrt(92)) #calculating tscore for sanity check 

blue.df <- as.data.frame(blue.df)
blue.df$hex2 <- with(blue.df, factor(hex2, levels = row.facs)) #make colour variable as factor for graphing

#distribution of asymmetry values for one colour
{blue.dist <- ggplot(blue.df)+
    aes(mean.asymmetry)+
    geom_histogram(fill= unique(blue.df$hex1))+ #make colour of histogram the colour plotted
    scale_x_continuous(breaks = as.numeric(round(quantile(blue.df$mean.asymmetry),digits = 2)))+ #change axis to display quantile value instead of arbitrary values
    labs(x = paste(unique(blue.df$hex1)), y = "Frequency")+
    theme_pubr(20)
ggsave("blue.dist.png",blue.dist,height=4,width=17)   
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of hitcount
{ggplot(blue.df) +
    aes(x = hex2, fill = pair_hitcount, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = 1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(blue.df$hex1), sep = ""),
      fill = "Number of participants") +
    theme_pubr(20)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs))
}
#Visualisation of asymmetry index for one colour only against all other colours to see impact of SEM
{blue.bar <- ggplot(blue.df) +
    aes(x = hex2, fill = sem.asymmetry, y = mean.asymmetry) +
    geom_col() +
    scale_fill_distiller(palette = "PuBuGn", direction = -1) +
    labs(
      x = "Colour compared to",
      y = "Asymmetry Index (averaged across participants)",
      title = paste("Asymmetries of ", unique(blue.df$hex1), sep = ""),
      fill = "SEM asymmetry") +
    theme_pubr(15)+
    scale_x_discrete(labels=block_rep)+
    theme(axis.text.x= element_text(size= 9, angle=90, colour=row.facs),
          legend.key.size = unit(1, "cm"))
  ggsave("blue.bar.png", blue.bar, height = 7, width = 17)
}
