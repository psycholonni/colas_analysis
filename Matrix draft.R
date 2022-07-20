#Currently lonni's plot of asymmetry index
{ggplot(AsIndata_wide) +
    aes(x = color1, y = color2, fill = AsIn) +
    geom_tile(size = 1.5) +
    scale_fill_distiller(palette = "RdBu", direction = 1) +
    ggthemes::theme_base()+
    theme(axis.text.x=element_text(colour=row.facs,angle=90,hjust=1),
          axis.text.y = element_text(colour=row.facs))
    
}

ggplot(mean.df, aes(x = color1, y = color2, fill = value)) + 
  geom_raster() + ggtitle('dissimilarity') +
  theme(axis.text.x = element_text(colour=row.facs, angle=90),
        axis.text.y = element_text(colour=row.facs)) +
  scale_fill_gradientn(colours = c("white","black"),
                       limits = c(0,7),
                       breaks=c(0,1,2,3,4,5,6,7),
                       na.value='green')


#

##TLDR 
## 1. create a matrix with each colour mean similarity rating
## 2. calculate correlation of mean between 
## 3. reorder based on correlation with other colours, so that everybody is next to a friend :)

# Create blank matrix 
color.mat.df <- matrix(NA, ncol = 93, nrow = 93)
# setting all give colours as both row and column names
colnames(color.mat.df) <- rownames(color.mat.df) <- unique(trialdata$hex1)

# fill matrix
matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    #print(i)
    row <- data[i,]
    #print(matrix.df[row$firstColour,row$secondColour])
    matrix.df[row$hex1,row$hex2] <- row$mean.similarity
    #matrix.df[row$secondColour,row$firstColour] <- row$value # symmetric
    #print(matrix.df[row$firstColour,row$secondColour])
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


######
# plot a matrix
mean.matrix.plot <- function(AsIndata_wide,name,log=FALSE,symmetry=TRUE,skip.mean=FALSE,normalised=FALSE){
  if(skip.mean){
    mean.df <- AsIndata_wide
  } else{
    mean.df <- group.avg.df(AsIndata_wide, colourSetHEX)
  }
  #print(log)
  #print(head(mean.df))
  #row.facs <- row.factors(rowsTotalSetHEX,row.cor(mean.df))
  #mean.df$color1 <- rowsTotalSetHEX$colour
  #mean.df$color2 <- rowsTotalSetHEX$colour
  mean.df$color1 <- with(mean.df, factor(hex, levels = row.facs)) # reorder the color labels according to row.facs, another vector
  mean.df$color2 <- with(mean.df, factor(secondColour, levels = row.facs))
  
  if(symmetry){
    sym.data <- mean.df
    sym.data$C1 <- mean.df$color1
    sym.data$C2 <- mean.df$color2
    sym.data$color1 <- sym.data$C2
    sym.data$color2 <- sym.data$C1
    sym.data <- subset(sym.data, select = -c(C1, C2))
    #print(colnames(mean.df))
    #print(colnames(sym.data))
    mean.df <- rbind(mean.df,sym.data)
  }
  
  if(log){
    name <- paste(name,'_log_',sep='')
    plot <- ggplot(mean.df, aes(x = color1, y = color2, fill = log(value+1))) + 
      geom_raster() + ggtitle('log(dissimilarity+1)') +
      theme(axis.text.x = element_text(colour=row.facs, angle=90), # sets the individual colours for the text for each colour
            axis.text.y = element_text(colour=row.facs)) +
      scale_fill_gradientn(colours = c("white","black"),
                           limits = c(0,log(7+1)),
                           breaks=c(0,1,2,3,4,5,6,7),
                           na.value='green')
  } else if(normalised){
    plot <- ggplot(mean.df, aes(x = color1, y = color2, fill = value)) + 
      geom_raster() + ggtitle('dissimilarity') +
      theme(axis.text.x = element_text(colour=row.facs, angle=90),
            axis.text.y = element_text(colour=row.facs)) +
      scale_fill_gradientn(colours = c("white","black"),
                           limits = c(0,1),
                           breaks=c(0,0.5,1),
                           na.value='green')
  } else {
    plot <- ggplot(mean.df, aes(x = color1, y = color2, fill = value)) + 
      geom_raster() + ggtitle('dissimilarity') +
      theme(axis.text.x = element_text(colour=row.facs, angle=90),
            axis.text.y = element_text(colour=row.facs)) +
      scale_fill_gradientn(colours = c("white","black"),
                           limits = c(0,7),
                           breaks=c(0,1,2,3,4,5,6,7),
                           na.value='green')
  }
  name <- paste(name,'.png',sep='')
  ggsave(name,plot,height=15,width=17)
}