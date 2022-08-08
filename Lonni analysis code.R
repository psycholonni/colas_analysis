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
library(readxl)}

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

###################
###DATA CLEANING###
###################

#screening participants with less framerate too low
data <- data[data$frameRate>29,]

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

#save plot
correlation_histogram <- hist(pass_correlation$pearson,
                              main="Histogram of double pass correlations (rho)", 
                              sub=paste("median=", medianpass),
                              xlab="doublepasscorrelation(rho)",
                              border="steelblue",
                              col="blue",
                              xlim=c(-.5,1),
                              las=1,
                              breaks=20)

#Same plot as above (hist of dp corr) with Lonni's viz
ggplot(pass_correlation, aes(pearson)) +
  geom_histogram(fill= "#63B8FF") +
  scale_x_continuous(breaks = as.numeric(round(quantile(pass_correlation$pearson),digits = 2))) + 
    theme_pubr() +   
  theme(axis.line = element_blank())+
  geom_rangeframe()+
  labs(x="doublepasscorrelation(rho)",
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
get_pair_hitcount<-function(x){ #get number of hits on pair of colour (x = colourpair)
  pair_hitcount<-sum(trialdata_passes$pairofcolour == x)
  return(pair_hitcount)
}
colourpairs$pair_hitcount <-lapply(colourpairs$pair, get_pair_hitcount)

colourpairs$pair_hitcount <- as.integer(colourpairs$pair_hitcount)

#MATRIX PLOT OF HITS FOR COLOUR
{
#heatmap of hitcount (the more a pair is tested, the 'hotter' it gets)
ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill= pair_hitcount) +
  geom_raster() +
  theme_pubr()+
  theme(axis.text.x=element_text(angle=90,hjust=1, size=6, colour = sort(unique(colourpairs$hex1))),
        axis.text.y = element_text(size = 6, colour = sort(unique(colourpairs$hex1))))+
  theme(legend.position = "left")+
  scale_fill_gradient(name= "Times tested",low = "grey", high = "black", na.value = "green")+
  labs(x= "Colour presented first", y= "Colour presented second")
  

#ggplot(colourpairs) + aes(x = hex1, y = hex2, fill= pair_hitcount) + geom_raster() +  theme_pubr()+  theme(axis.text.x=element_text(angle=90,hjust=1, size=7), axis.text.y = element_text(size = 7))

#distribution of hitcounts
ggplot(colourpairs) +
  aes(x= pair_hitcount) +
  geom_bar() +
  theme_minimal()
}
#hitcount descriptives
summary(colourpairs$pair_hitcount)
sd(colourpairs$pair_hitcount)

#MAKE DATAFRAME WITH MEAN SIMILARITY PER COLOUR PAIR MAINTAINING DIRECTIONNALITY (AB=/=BA)
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

#APPLY COLOUR FACTORS TO COLOURPAIRS DATAFRAME
colourpairs$hex1 <- with(colourpairs, factor(hex1, levels = row.facs))
colourpairs$hex2 <- with(colourpairs, factor(hex2, levels = row.facs))

#VISUALISE MEAN SIMILARITY
{
#Rainbow  
ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill = mean.similarity) +
  geom_raster() +
  scale_fill_gradientn(colors = rainbow(7),breaks= c(0,1,2,3,4,5,6,7))+
  theme_pubr()+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=row.facs))+
  theme(axis.text.y = element_text(size= 7, colour=row.facs))

#B&W 
ggplot(colourpairs) +
    aes(x = hex1, y = hex2, fill = mean.similarity) +
    geom_raster() +
    scale_fill_gradient(low= "grey", high= "black",breaks= c(0,1,2,3,4,5,6,7))+
    theme_pubr()+
    theme(legend.position = "left")+
    theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))+
    theme(axis.text.y = element_text(size= 5, colour=row.facs))+
    scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
    labs(x= "Colour presented first", y= "Colour presented second")
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
#SIMILARITY MATRIX
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

#MAKE DATAFRAME WITH MEAN VARIANCE PER COLOUR PAIR (AB==BA)
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

#MAKE DATAFRAME WITH ASYMMETRY INDEX (AsIn) for each participant
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

#GET AsIn variance 
get_var_asymmetry <- function(z) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
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

#MAKE DATAFRAME WITH ASYMMETRY PER COLOUR PAIR (AB=BA)
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

#MAKE ASYMMETRY MATRIX ALL PARTICIPANTS -- To be refined
{

#plot with colour pairs CORRECT ONE TO USE 
ggplot(colourpairs) +
    aes(x = hex1, y = hex2, fill = mean.asymmetry) +
    geom_raster() +
    scale_fill_gradient2(low ="red", high= "white", mid ="white", midpoint= 0, na.value = "green") +
    scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
    theme_pubr()+
    theme(legend.position = "left")+
    theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))+
    theme(axis.text.y = element_text(size= 5, colour=row.facs))

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

## T-TESTING UNCORRECTED --- WIP
colourpairs$t <- NA
colourpairs$t <- colourpairs$mean.asymmetry/(sqrt(colourpairs$var.asymmetry)/sqrt(colourpairs$pair_hitcount))

#t-score matrix
ggplot(colourpairs)+
  aes(x = hex1, y = hex2, fill = t) +
  geom_raster() +
  scale_fill_gradient2(low ="red", high= "blue", mid ="white", midpoint= 0, na.value = "green") +
  scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
  theme_pubr()+
  theme(legend.position = "left")+
  theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))+
  theme(axis.text.y = element_text(size= 5, colour=row.facs))

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

candidates_data <- trialdata[]

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


#### MATRIX WITH ASYMMETRY VALUES ####
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
asym_percolour.df$color.names <- with(asym_percolour.df, factor(color.names, levels = row.facs)) #make colour variable as factor for graphing

#calculate variance of each column
asym_percolour.df$Variance <- colVars(asym.matrix)

#replace NAs with zero for graphing 
asym_percolour.df[is.na(asym_percolour.df)] = 0

#visualise average asymmetry per column 
ggplot(asym_percolour.df)+
  aes(x = color.names, y= average.asymmetry, fill= color.names)+
  theme_pubr()+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+ 
  geom_hline(aes(yintercept =0))

#visualise asymmetry variance per column 
ggplot(asym_percolour.df)+
  aes(x = color.names, y= Variance, fill= color.names)+
  theme_pubr()+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))

#Calculating rough tscore with population equal to 92 colours 
asym_percolour.df$t <- NA
asym_percolour.df$t <- asym_percolour.df$average.asymmetry/(sqrt(asym_percolour.df$Variance)/sqrt(92))
asym_percolour.df$t <- abs(asym_percolour.df$t)
asym_percolour.df[is.na(asym_percolour.df)] = 0 #for graphing

#visualise asymmetry tscore per column 
ggplot(asym_percolour.df)+
  aes(x = color.names, y= t, fill= color.names)+
  theme_pubr()+
  geom_col()+
  scale_x_discrete(labels=block_rep)+
  scale_fill_manual(values =c(row.facs))+
  guides(fill="none")+
  theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))
