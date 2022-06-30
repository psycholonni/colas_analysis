################################
##### THESIS ANALYSIS LONNI#####
################################
#This script is for the analysis of asymmetry in colour perception, 
#Reused code from "Sonia analysis code (Beth pilot)

#block annoying warnings when loading libraries
shhh <- suppressPackageStartupMessages 

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
library(JWileymisc)}

setwd("~/thesis/colas_analysis")
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

data <- bind_rows(pilot.data, data)
}

###################
###DATA CLEANING###
###################

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

#get catch score as fraction
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
  trialdata$similarity[trialdata$similarity == -4] <-0 }
## HISTOGRAM OF ANSWERS PER PARTICIPANTS TO CHECK DISTRIBUTION ACROSS SCALE
{
#could modify y axis to show min/max count for better data/ink ratio
ggplot(trialdata) +
 aes(x = similarity, fill = participant) +
 geom_bar(position="dodge") +
 theme_minimal()


#One facet per participant
ggplot(trialdata) +
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

#convert rgb to hex
for (x in 1:nrow(circle1_df)){
  circle1_df$hex1[x] <- rgb(circle1_df[x,1],circle1_df[x,2],circle1_df[x,3], maxColorValue=255)
  circle2_df$hex2[x]<- rgb(circle2_df[x,1],circle2_df[x,2],circle2_df[x,3], maxColorValue=255)
}

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

pass_correlation$pearson<-lapply(catch_perperson$participant, getpearson) #apply fn
pass_correlation$spearman<-lapply(catch_perperson$participant, getspearman)

#make correlation numeric for graphing 
pass_correlation$pearson<-as.numeric(pass_correlation$pearson)
pass_correlation$spearman<-as.numeric(pass_correlation$spearman)

#if error: check trialdata_passes that resulted in NA correlation 
#unique(unlist (lapply (pass_correlation, function (x) which (is.na (x))))) 
#trialdata_passes[trialdata_passes$participant == '614849337165274baeeed45a',] 
#drop rows with NA correlation #pass_correlation<- na.omit(pass_correlation) 

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
#REASONABLE THRESHOLD OF EXCLUSION--unfinished
{
#save mean fisherz
mean_z<- round(mean(pass_correlation$fisherz), 2)
sd_z<- round(sd(pass_correlation$fisherz), 2)

#save histogram of fisher z for all participants
z_histogram <- hist(pass_correlation$fisherz,
                    main="double pass correlations (z)", 
                    sub=paste("mean=", mean_z, "\nstandard deviation: ", sd_z),
                    xlab="fisher z",
                    border="steelblue",
                    col="blue",
                    xlim=c(-3,3),
                    las=1,
                    breaks = 30)
}

###################
###DATA ANALYSIS###
###################

#MAKE DATAFRAME OF COLOURPAIRS
{
#get unique hex arrangements
colourpairs <- unique(trialdata_passes[ , c("hex1", "hex2")]) 

#remove flipped colour pairs so each colour pair is only listed once

for (i in 2:nrow(colourpairs)) {
  for (j in 1:(i-1)){
    if ((colourpairs$hex1[i] == colourpairs$hex2[j]) 
        && (colourpairs$hex2[i] == colourpairs$hex1[j])) {
      colourpairs$hex1[i] = "duplicate"
    }
  }
}

colourpairs<-subset(colourpairs, !(hex1=="duplicate"))
}
#MATRIX PLOT OF HITS FOR COLOUR
{
#Create unique pair name by concatening first colour with second colour
colourpairs$pair <- str_c(colourpairs$hex1, '',colourpairs$hex2)

trialdata_passes$pairofcolour <- str_c(trialdata_passes$hex1, '',trialdata_passes$hex2)

#adding count of colour pairs 
get_pair_hitcount<-function(x){ #get number of hits on pair of colour (x = colourpair)
  pair_hitcount<-sum(trialdata_passes$pairofcolour == x)
  return(pair_hitcount)
}
colourpairs$pair_hitcount <-lapply(colourpairs$pair, get_pair_hitcount)

colourpairs$pair_hitcount <- as.integer(colourpairs$pair_hitcount)

ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill= pair_hitcount) +
  geom_tile(size = 1.2) +
  theme_pubr()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
}

#MAKE DATAFRAME WITH MEAN SIMILARITY PER COLOUR PAIR (across both colour orders/both passes/all participants)
mean_similarity<-colourpairs
mean_similarity$mean<-NA

for (x in 1:nrow(mean_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] & trialdata_passes$hex2==mean_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
  b<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
  c<-rbind(a, b) #all similarty ratings for colour pair x
  mean_similarity$mean[x]<-mean(as.matrix(c))
}



#VISUALISE DISTANCES FROM ONE COLOUR
{
#list of unique hex values
colours<-unique(trialdata_passes$hex1)

#add column to similarity_means with random numbers 0-1 (to space out colour dots on graph)
mean_similarity$random<-runif(nrow(mean_similarity))

#plot mean distances from one colour
#z=hex string
get_colourdistance<-function(z) {
  temp_df<-rbind(subset(mean_similarity, hex1==z), subset(mean_similarity, hex2==z)) #df for one colour 
  temp_df$plothex<-NA  #column with all the colours that were compared to colour z
  for (x in 1:nrow(temp_df)){
    if(temp_df$hex1[x]==z){
      temp_df$plothex[x]<-temp_df$hex2[x]
    }
    if(temp_df$hex2[x]==z){
      temp_df$plothex[x]<-temp_df$hex1[x]
    }
    if(temp_df$hex1[x]==z & temp_df$hex2[x]==z){
      temp_df$plothex[x]<-temp_df$hex1[x]
    }
  }
  
  G <- ggplot(temp_df,aes(x=mean)) +
    geom_point(aes(y = random,color=z), size = 2, col=temp_df$plothex) +
    labs(title="distance from", subtitle = z, x="mean similarity rating", y="random spread for clarity") +
    theme(plot.subtitle = element_text(colour = z))
  G
}

List_colourdistance<-lapply(colours, get_colourdistance) #apply fn to each colour, one at a time
#List_colourdistance<-lapply("#40A144", get_colourdistance) #apply fn to one colour
#List_colourdistance
}

#MAKE DATAFRAME WITH ASYMMETRY PER COLOUR PAIR, FOR EACH PARTICIPANT Sonia (Beth's code )
{
#make dataframe with a row for each similarity rating
similarity_perperson<-trialdata_passes[ ,c("participant", "hex1", "hex2", "similarity")]
similarity_perperson<-rename(similarity_perperson, similarity_given = similarity) #for similarity rating with given hex order (hex 1, hex2)
similarity_perperson$similarity_reverse<-NA #for similarity rating with reverse hex order (hex2, hex1)

#put data in similarity_reverse column (this for loop is quite slow--use Beth's dissimilarity matrix code instead)
for (i in 2:nrow(similarity_perperson)){
  for (j in 1:i){
    if (similarity_perperson$participant[i]==similarity_perperson$participant[j]
        & similarity_perperson$hex1[i]==similarity_perperson$hex2[j]
        & similarity_perperson$hex2[i]==similarity_perperson$hex1[j]){
      similarity_perperson$similarity_reverse[i]<-similarity_perperson$similarity_given[j]
      similarity_perperson$similarity_reverse[j]<-"duplicate"
    }
  }
}

#delete extra rows
similarity_perperson<-subset(similarity_perperson, !(similarity_reverse=="duplicate"))

#asymmetry column
similarity_perperson$similarity_given<-as.numeric(similarity_perperson$similarity_given)
similarity_perperson$similarity_reverse<-as.numeric(similarity_perperson$similarity_reverse)

similarity_perperson$asymmetry<-abs(similarity_perperson$similarity_given-similarity_perperson$similarity_reverse)
}

# MAKE DATAFRAME WITH ASYMMETRY INDEX (AsIn) 
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

#calculating Asymmetry Index as per Nao's formula (M13 -M24)/((A13+A24)+1)
AsIndata_wide$AsIn <- (AsIndata_wide$similarity.first - AsIndata_wide$similarity.second)/
  ((AsIndata_wide$abs.first + AsIndata_wide$abs.second) + 1)
}

#MAKE ASYMMETRY MATRIX ALL PARTICIPANTS -- To be refined
{ggplot(AsIndata_wide) +
  aes(x = hex1.first, y = hex1.second, fill = AsIn) +
  geom_tile(size = 1.5) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  ggthemes::theme_base()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
}
  
#BETH'S CODE: MAKE SIMILARITY MATRIX FOR ONE PARTICIPANT--unfinished
{
# factor the dataframes for the plot function
dissimdata2 <- function(trialdata, colours){
  
  # refactor the levels so they can be plotted properly later if need be
  trialdata$hex1 <- with(trialdata, factor(hex1, levels = colours))
  trialdata$hex2 <- with(trialdata, factor(hex2, levels = colours))
  
  return(trialdata)
}
}


