# Install and load the e1071 package for skewness calculation
if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}

## Load the data from the "skew.RData" file using the load() function.
load("skew.RData")

## Check the dimensions of the 'dat' matrix
# This file contains a matrix named 'dat'
dim(dat)

## Set up a 3x3 grid for QQ-plots
par(mfrow = c(3, 3))

## Loop through the columns and create QQ-plots
skewed_columns <- c()  # To store the indices of skewed columns

# Loop through the columns (from 1 to 9)
for (i in 1:9) {
  ## Create QQ-plots for each column
  qqnorm(dat[, i], main = paste("Column", i))
  
  ## Add a reference line for a normal distribution
  abline(h = 0, col = "red")
  
  ## Calculate skewness using the skewness function from the e1071 package
  sk <- skewness(dat[, i])
  
  ## Check for skewness based on skewness value
  if (abs(sk) > 0.5) {
    # Adjust the threshold as needed
    ## If a column is found to be skewed, it is labeled as such
    cat("Column", i, "is skewed with skewness =", sk, "\n")
    
    ## Its index is stored in the skewed_columns vector
    skewed_columns <- c(skewed_columns, i)
  }
}

## Reset the graph to show one plot at a time
par(mfrow = c(1, 1))

## Create histograms for the skewed columns
for (col_index in skewed_columns) {
  hist(dat[, col_index], main = paste("Column", col_index), xlab = "Value")
  
  ## Add labels for positive and negative skew based on histogram shapes
  sk <- skewness(dat[, col_index])
  
  if (sk > 0) {
    ## If a column has positive skew, label it as such
    cat("Column",
        col_index,
        "has positive skew (right tail) with skewness =",
        sk,
        "\n")
  } else if (sk < 0) {
    ## If a column has negative skew, label it as such
    cat("Column",
        col_index,
        "has negative skew (left tail) with skewness =",
        sk,
        "\n")
  } else {
    ## If a column is approximately normally distributed, label it as such
    cat("Column",
        col_index,
        "is approximately normally distributed.\n")
  }
}
