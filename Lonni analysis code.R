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
}

#############################
###ISHIHARA COLOUR TESTING###
#############################



###################
###DATA ANALYSIS###
###################

#MAKE DATAFRAME OF COLOURPAIRS
{
#get unique hex arrangements
colourpairs <- unique(trialdata_passes[ , c("hex1", "hex2")]) 

#remove flipped colour pairs so each colour pair is only listed once
#for (i in 2:nrow(colourpairs)) {
#  for (j in 1:(i-1)){
#    if ((colourpairs$hex1[i] == colourpairs$hex2[j]) 
#        && (colourpairs$hex2[i] == colourpairs$hex1[j])) {
#      colourpairs$hex1[i] = "duplicate"
#    }
#  }
#}

#colourpairs<-subset(colourpairs, !(hex1=="duplicate"))
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

#heatmap of hitcount (the more a pair is tested, the 'hotter' it gets)
ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill= pair_hitcount) +
  geom_raster() +
  theme_pubr()+
  theme(axis.text.x=element_text(angle=90,hjust=1, size=7), axis.text.y = element_text(size = 7))+
  scale_fill_gradient(low = "grey", high = "black", na.value = "green")

#ggplot(colourpairs) + aes(x = hex1, y = hex2, fill= pair_hitcount) + geom_raster() +  theme_pubr()+  theme(axis.text.x=element_text(angle=90,hjust=1, size=7), axis.text.y = element_text(size = 7))

#distribution of hitcounts
#ggplot(colourpairs) +
#  aes(x = pair_hitcount) +
#  geom_bar() +
#  theme_minimal()
}

#MAKE DATAFRAME WITH MEAN SIMILARITY PER COLOUR PAIR MAINTAINING DIRECTIONNALITY (AB=/=BA)
{
#mean_similarity<-colourpairs
#mean_similarity$mean<-NA
#for (x in 1:nrow(mean_similarity)) {#  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] & trialdata_passes$hex2==mean_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
  b<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
  c<-rbind(a, b) #all similarty ratings for colour pair x
  mean_similarity$mean[x]<-mean(as.matrix(c))
#}

get_mean_similarity <- function(z){ 
  a<-subset(trialdata_passes, pairofcolour==z) #similarity with exact hex match
  mean<-mean(a$similarity)
  return(mean)
}
colourpairs$mean.similarity<-lapply(colourpairs$pair, get_mean_similarity) 
colourpairs$mean.similarity<- as.numeric(colourpairs$mean.similarity)
}
#VISUALISE MEAN SIMILARITY
{ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill = mean.similarity) +
  geom_raster() +
  scale_fill_gradientn(colors = rainbow(7),breaks= c(0,1,2,3,4,5,6,7))+
  theme_pubr()+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(colourpairs$hex1))))+
  theme(axis.text.y = element_text(size= 7, colour=sort(unique(colourpairs$hex2))))
}

#MAKE DATAFRAME WITH MEAN SIMILARITY PER COLOUR PAIR (AB==BA)
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

#MAKE DATAFRAME WITH MEDIAN SIMILARITY PER COLOUR PAIR (AB==BA)
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
#MEDIAN SIMILARITY MATRIX
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
  ggthemes::theme_base()+
  theme(axis.text.x= element_text(size= 6, angle=90, colour=sort(unique(mean_variance$hex1))))+
  theme(axis.text.y = element_text(size= 6, colour=sort(unique(mean_variance$hex2))))+
  theme(legend.position = "right")+
  labs(x= "Colour presented first", y= "Colour presented second")
}

#Get Standard Error of Mean 
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

#calculating Asymmetry Index as per Nao's formula (M13 -M24)/((A13+A24)+1)
AsIndata_wide$AsIn <- (AsIndata_wide$similarity.first - AsIndata_wide$similarity.second)/
  ((AsIndata_wide$abs.first + AsIndata_wide$abs.second) + 1)


#Create unique pair name by concatening first colour with second colour
AsIndata_wide$pair <- str_c(AsIndata_wide$hex1.first, '',AsIndata_wide$hex1.second)

#GET AsIn in Colourpairs DATAFRAME
get_mean_asymmetry <- function(z){ 
  a<-subset(AsIndata_wide, pair==z) #similarity with exact hex match
  mean<-mean(a$AsIn)
  return(mean)
}
colourpairs$mean.asymmetry<-lapply(colourpairs$pair, get_mean_asymmetry) 
colourpairs$mean.asymmetry<- as.numeric(colourpairs$mean.asymmetry)

test <- subset(colourpairs, is.na(colourpairs$mean.asymmetry))
test <- subset(AsIndata_wide, is.na(AsIndata_wide$AsIn))
}

#MAKE DATAFRAME WITH ASYMMETRY PER COLOUR PAIR (AB=BA)
{#gathering AsIn using for loop
mean_asymmetry<-mean_variance
mean_asymmetry$meanas<-NA

for (x in 1:nrow(mean_asymmetry)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(AsIndata_wide,AsIndata_wide$hex1.first==mean_asymmetry$hex1[x] & AsIndata_wide$hex1.second==mean_asymmetry$hex2[x], c(AsIn)) #similarity with exact hex match
  b<-subset(AsIndata_wide,AsIndata_wide$hex1.first==mean_asymmetry$hex2[x] & AsIndata_wide$hex1.second==mean_asymmetry$hex1[x], c(AsIn)) #similarity with flipped hex order
  c<-rbind(a, b) #all similarty ratings for colour pair x
  mean_asymmetry$meanas[x]<-mean(as.matrix(c))
}
}

#MAKE ASYMMETRY MATRIX ALL PARTICIPANTS -- To be refined
{
#order from ariel   
#col.od.df <- read_excel("Colour order from Ariel.xlsx")
#apply order 

    
#plot  
ggplot(AsIndata_wide) +
  aes(x = hex1.first, y = hex1.second, fill = AsIn) +
  geom_raster() +
  scale_fill_distiller(palette = "RdBu", direction = 1, na.value = "green") +
  ggthemes::theme_base()+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(AsIndata_wide$hex1.first))))+
  theme(axis.text.y = element_text(size= 7, colour=sort(unique(AsIndata_wide$hex1.second))))

#plot with colour pairs  
ggplot(colourpairs) +
    aes(x = hex1, y = hex2, fill = mean.asymmetry) +
    geom_raster() +
    scale_fill_distiller(palette = "RdBu", direction = 1, na.value = "green") +
    ggthemes::theme_base()+
    theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(colourpairs$hex1))))+
    theme(axis.text.y = element_text(size= 7, colour=sort(unique(colourpairs$hex2))))
  
#plot with mean_asymmetry  CORRECT ONE TO USE 
ggplot(mean_asymmetry) +
  aes(x = hex1, y = hex2, fill = meanas) +
  geom_raster() +
  scale_fill_distiller(palette = "RdBu", direction = 1, na.value = "green") +
  ggthemes::theme_base()+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=sort(unique(mean_asymmetry$hex1))))+
  theme(axis.text.y = element_text(size= 7, colour=sort(unique(mean_asymmetry$hex2))))
  }

#FUNNEL PLOT Asymmetry x Hitcount
ggplot(mean_asymmetry) +
  aes(y = meanas, x = pair_hitcount) +
  geom_point(position = "jitter") +
  geom_hline(yintercept = 0, colour = "red")+
  theme_pubr()

#FUNNEL PLOT Asymmetry x SEM
ggplot(mean_asymmetry) +
  aes(y = meanas, x = sem) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red")+
  theme_pubr()
