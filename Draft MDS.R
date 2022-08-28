#RESCALING THE DATA TO DISSIMILARITY
{ trialdata$similarity[trialdata$similarity == -4] <- 7
  trialdata$similarity[trialdata$similarity == -3] <- 6
  trialdata$similarity[trialdata$similarity == -2] <- 5
  trialdata$similarity[trialdata$similarity == -1] <- 4
  trialdata$similarity[trialdata$similarity == 1] <-3
  trialdata$similarity[trialdata$similarity == 2] <-2
  trialdata$similarity[trialdata$similarity == 3] <-1
  trialdata$similarity[trialdata$similarity == 4] <-0
  
  


get_mean_similarity <- function(z){ 
    a<-subset(trialdata_passes, pairofcolour==z) #similarity with exact hex match
    mean<-mean(a$similarity)
    return(mean)
  }
colourpairs$mean.similarity<-lapply(colourpairs$pair, get_mean_similarity) 
colourpairs$mean.similarity<- as.numeric(colourpairs$mean.similarity)


colourpairs$pair.reversed <- str_c(colourpairs$hex2, '',colourpairs$hex1)

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
colourpairs$mean.sim <- 7- colourpairs$mean.sim

}


## Make a similarity matrix
# Create blank matrix 
sim.matrix <- matrix(3.5, ncol = 93, nrow = 93)
# setting all give colours as both row and column names
colnames(sim.matrix) <- rownames(sim.matrix) <- unique(trialdata$hex1)

# fill matrix with similarity values
matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    row <- data[i,]
    matrix.df[row$hex1,row$hex2] <- row$mean.sim
  }
  return(matrix.df)
}

sim.data_vars <- c("hex1", "hex2", "mean.sim")
sim.data <- colourpairs[sim.data_vars]
sim.data$mean.sim <- as.numeric(sim.data$mean.sim)

sim.matrix <- matrix.df.fill(sim.data,sim.matrix)
sim.matrix.df <- as.data.frame(sim.matrix)


fit <- cmdscale(sim.matrix.df, eig= TRUE, k =2)
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

#3d plot
fit <- cmdscale(sim.matrix.df, eig= TRUE, k =3)
x <- fit$points[, 1]
y <- fit$points[, 2]
z <- fit$points[, 3]

colors <- colnames(sim.matrix)

plot3d(x,y,z, col=colors, size = 15)

#### From Ariel's code
# convert a similarity judgment dataframe to a distance matrix for MDS
dist.create <- function(data){
  #print(head(data))
  mean.data.temp <- group.avg.df(data,colourSetHEX)
  dist.temp <- matrix.df.fill(mean.data.temp,color.mat.df)
  #print(head(dist.temp))
  #dist.temp$color1 <- rowsTotalSetHEX$colour
  #dist.temp$color2 <- rowsTotalSetHEX$colour
  #return(dist.temp)
  dist.mat.temp <- as.dist(t(dist.temp), diag=TRUE)
  #print(head(dist.mat))
  return(dist.mat.temp)
}

# make a set of 3D visualisations
plot.set.3D <- function(data.list, name, log=FALSE,quick=TRUE){
  folder.str <- 'CV 3D sets/'
  dir.create(folder.str)
  if(log){
    name <- paste(name,'_log',sep='')
  }
  dir.create(paste(folder.str,name,sep=''))
  
  for(i in 1:length(data.list)){
    subfolder.str <- paste(folder.str,name,'/',i,sep='')
    dir.create(subfolder.str)
    
    dist.mat <- dist.create(data.list[[i]])
    
    print(subfolder.str)
    if(log){
      visualise3d(log(dist.mat+1),subfolder.str,diagnose=quick)
    } else {
      visualise3d(dist.mat,subfolder.str,diagnose=quick)
    }
  }
}
