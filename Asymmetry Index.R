#make DT for one person
trialdata_oneparticipant <- trialdata_passes[trialdata_passes$participant == "614849337165274baeeed45a",]

#Calculate mean of both passes for each direction 
mean_similarity$mean13<-NA
mean_similarity$mean24<-NA
for (x in 1:nrow(mean_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  a<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] & trialdata_passes$hex2==mean_similarity$hex2[x], c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
  b<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
  mean_similarity$mean13[x]<-mean(as.matrix(a))
  mean_similarity$mean24[x]<-mean(as.matrix(b))
  }


#absolute difference between two passes one direction
mean_similarity$abs13<- NA
for (x in 1:nrow(mean_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair 
                                      #(x = row in mean_similarity ie. one per colourpair)
  d<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex1[x] &
                              trialdata_passes$hex2==mean_similarity$hex2[x], 
            c(firstpass_similarity, secondpass_similarity)) #similarity with exact hex match
  mean_similarity$abs13[x]<-abs(d$firstpass_similarity[x]-d$secondpass_similarity[x])
}

#absolute difference between two passes reverse direction
mean_similarity$abs24<- NA
for (x in 1:nrow(mean_similarity)) {  #makes a temporary dataframe with all the similarity ratings for one colourpair (x = row in mean_similarity ie. one per colourpair)
  e<-subset(trialdata_passes,trialdata_passes$hex1==mean_similarity$hex2[x] & trialdata_passes$hex2==mean_similarity$hex1[x], c(firstpass_similarity, secondpass_similarity)) #similarity with flipped hex order
  mean_similarity$abs24[x]<-abs(e$firstpass_similarity[x]-e$secondpass_similarity[x])
}
