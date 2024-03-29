#RESCALING THE DATA TO DISSIMILARITY
{ trialdata$dissimilarity[trialdata$similarity == -4] <- 7
  trialdata$dissimilarity[trialdata$similarity == -3] <- 6
  trialdata$dissimilarity[trialdata$similarity == -2] <- 5
  trialdata$dissimilarity[trialdata$similarity == -1] <- 4
  trialdata$dissimilarity[trialdata$similarity == 1] <-3
  trialdata$dissimilarity[trialdata$similarity == 2] <-2
  trialdata$dissimilarity[trialdata$similarity == 3] <-1
  trialdata$dissimilarity[trialdata$similarity == 4] <-0
}  
  
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
colourpairs$mean.dissim <- 7- colourpairs$mean.sim




## Make a dissimilarity matrix
# Create blank matrix 
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
fit <- cmdscale(dissim.matrix.df, eig= TRUE, k =3)
x <- fit$points[, 1]
y <- fit$points[, 2]
z <- fit$points[, 3]

colors <- colnames(dissim.matrix)

plot3d(x,y,z, col=colors, size = 15)


#MAKE DATAFRAME WITH MDS COORDINATES
mds_coord <- as.data.frame(colors)
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

#Visualise MDS distance in matrix 
ggplot(colourpairs) +
  aes(x = hex1, y = hex2, fill = mds.distance) +
  geom_raster() +
  scale_fill_distiller(name= "MDS distance",palette = "Greys", direction = -1,breaks= c(0,1,2,3,4,5,6,7))+
  theme_pubr()+
  theme(legend.position = "left")+
  theme(axis.text.x= element_text(size= 7, angle=90, colour=row.facs))+
  theme(axis.text.y = element_text(size= 7, colour=row.facs))+
  scale_x_discrete(labels=block_rep) + scale_y_discrete(labels=block_rep) +
  labs(x= "Colour presented first", y= "Colour presented second")


#dichotomise long distance and short distance
above.medianmds <- subset(colourpairs, mds.distance>median(colourpairs$mds.distance))
below.medianmds <- subset(colourpairs, mds.distance<median(colourpairs$mds.distance))

#calculate average asymmetry for each distance group
avasym.abovemds <- mean(abs(above.medianmds$mean.asymmetry))
avasym.belowmds <- mean(abs(below.medianmds$mean.asymmetry))


ggplot(colourpairs)+
  aes(x= log(8-mds.distance), y= abs(mean.asymmetry))+
  geom_point()+
  #geom_point(aes(x=4.55, y= avasym.abovemds), col = "#67a9cf")+
  #geom_point(aes(x=4.55, y= avasym.belowmds), col = "#ef8a62")+
    theme_pubr()+
  geom_smooth()

cor(log(8-colourpairs$mds.distance), abs(colourpairs$mean.asymmetry))

#linear model 
m1 <- lm(abs(mean.asymmetry)~ poly(mds.distance,1), 
             data= colourpairs)
lmer1 <- lmer(abs(mean.asymmetry)~poly(mds.distance,1) + (1|), data=colourpairs)
summary(m1)

md <- modelDiagnostics(m1, ev.perc=.005)
plot(md, ncol=2 , ask= FALSE)

visreg(m1, xvar= "mds.distance", gg= TRUE)+
  theme_pubr()
#quadratic model
m2 <- lm(abs(mean.asymmetry)~ poly(mds.distance,2), 
         data= colourpairs)
summary(m2)
md2 <- modelDiagnostics(m2, ev.perc=.005)
plot(md2, ncol=2 , ask= FALSE)

#linear vs quadratic comparison
AIC(m1, m2)
BIC(m1,m2)

#cubic model
m3 <- lm(abs(mean.asymmetry)~ poly(mds.distance,3), 
         data= colourpairs)
summary(m3)
md3 <- modelDiagnostics(m3, ev.perc=.005)
plot(md3, ncol=2 , ask= FALSE)

#quadratic vs cubic comparison
AIC(m2, m3)
BIC(m2,m3)

#4th model
m4 <- lm(abs(mean.asymmetry)~ poly(mds.distance,4), 
         data= colourpairs)
summary(m4)
md4 <- modelDiagnostics(m4, ev.perc=.005)
plot(md4, ncol=2 , ask= FALSE)

#cubic vs 4th comparison
AIC(m3, m4)
BIC(m3,m4)


visreg(m4, xvar= "mds.distance", gg= TRUE)+
  theme_pubr()

#quintic model
m5 <- lm(abs(mean.asymmetry)~ poly(mds.distance,5), 
         data= colourpairs)
summary(m5)
md5 <- modelDiagnostics(m5, ev.perc=.005)
plot(md5, ncol=2 , ask= FALSE)

#4th vs 5th dimension comparison
AIC(m4, m5)
BIC(m4, m5)


mlr <- lm(abs(mean.asymmetry)~ poly(mds.distance,4) + mean.similarity + variance.similarity, 
   data = colourpairs)
summary(mlr)

mlr <- lm(abs(mean.asymmetry)~ poly(mds.distance,1) + mean.similarity + variance.similarity, 
          data = colourpairs)
summary(mlr)

mlr.interaction <- lm(abs(mean.asymmetry)~ mds.distance + mean.similarity + variance.similarity+
                        mds.distance*mean.similarity*variance.similarity, 
                        data = colourpairs)
summary(mlr.interaction)

mlr.poly.interaction <- lm(abs(mean.asymmetry)~ poly(mds.distance,4) + mean.similarity + variance.similarity+
                        poly(mds.distance,4)*mean.similarity*variance.similarity, 
                      data = colourpairs)
summary(mlr.poly.interaction)

AIC(mlr.interaction, mlr.poly.interaction)
BIC(mlr.interaction, mlr.poly.interaction)

AIC(m4, mlr.poly.interaction)
BIC(m4, mlr.poly.interaction)

test <-lmer(abs(firstpass_similarity-secondpass_similarity) ~ similarity + (1|pairofcolour), data = trialdata_passes, REML = TRUE)
plot(modelDiagnostics(test))
