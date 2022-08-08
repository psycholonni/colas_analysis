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
  AsIndata_wide$AsIn13 <- (AsIndata_wide$mean13 - AsIndata_wide$mean24)
   
  #calculating Asymmetry Index as per Nao's formula (M24 -M13)/((A13+A24)+1)
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


#MAKE ASYMMETRY MATRIX ALL PARTICIPANTS -- To be refined
{
  
#plot with colour pairs CORRECT ONE TO USE 
ggplot(colourpairs) +
    aes(x = hex1, y = hex2, fill = mean.asymmetry) +
    geom_raster() +
    scale_fill_gradient2(low ="red", high= "blue", mid ="grey", midpoint= 0, na.value = "green") + scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
    theme_pubr()+
    theme(legend.position = "left")+
    theme(axis.text.x= element_text(size= 5, angle=90, colour=row.facs))+
    theme(axis.text.y = element_text(size= 5, colour=row.facs))

#Red Blue colour scale  
#scale_fill_viridis_c(option = "viridis", direction = 1) +
  

#plot with mean_asymmetry 
ggplot(mean_asymmetry) +
    aes(x = hex1, y = hex2, fill = meanas) +
    geom_raster() +
    scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
    scale_fill_gradient2(low ="red", high= "blue", mid ="white", midpoint= 0, na.value = "green") +
    ggthemes::theme_base()+
    theme(axis.text.x= element_text(size= 7, angle=90, colour=row.facs))+
    theme(axis.text.y = element_text(size= 7, colour=row.facs))
  
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

#testing normality of distribution
ks.test(colourpairs$mean.asymmetry, "pnorm", 0, sd(colourpairs$mean.asymmetry),alternative='two.sided')

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

