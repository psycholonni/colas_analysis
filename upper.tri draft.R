# set lower triangle of matrix to NA - this gives heatmap its unique upper triangle
upperTriangularMatrix <- upper.tri(matrix(1, ncol = 93, nrow = 93), 
                                   diag = FALSE)
upperTriangularMatrix[upperTriangularMatrix == 1] <- NA
color.mat.df <- data.frame(upperTriangularMatrix)
color.mat <- upperTriangularMatrix

# setting all give colours as both row and column names
colnames(color.mat.df) <- rownames(color.mat.df) <- unique(trialdata$hex1)
colnames(color.mat) <- rownames(color.mat) <- unique(trialdata$hex1)

#
upper.asym.matrix <- asym.matrix #makes copy of full matrix 
upper.asym.matrix[lower.tri(upper.asym.matrix)] <- NA #replaces values in lower triangle with NA

hist(upper.asym.matrix)

shapiro.test(upper.asym.matrix)

# Create blank matrix 
mds.matrix <- matrix(3.5, ncol = 93, nrow = 93)
# setting all give colours as both row and column names
colnames(mds.matrix) <- rownames(mds.matrix) <- unique(trialdata$hex1)

# fill matrix with mds distances
mds.matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    row <- data[i,]
    matrix.df[row$hex1,row$hex2] <- row$mds.distance
  }
  return(matrix.df)
}

mds.data_vars <- c("hex1", "hex2", "mds.distance")
mds.data <- colourpairs[mds.data_vars]
mds.data$mds.distance <- as.numeric(mds.data$mds.distance)

mds.matrix <- mds.matrix.df.fill(mds.data,mds.matrix)
mds.matrix[lower.tri(mds.matrix)] <- NA

hist(mds.matrix)
shapiro.test(mds.matrix)
