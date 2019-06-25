          #####################
          ##    Chapter 1    ##
          #####################

# GETTING STARTED WITH R

# Comments, indents, and semicolons
  # Anything prefaced by a pound sign (#) is a comment.
  # Comments are not executed by R. Instead, they explain what the code is doing.
  # Indented code (that is not a comment) will run in R as if it was on one line
  # Code separated by semicolons will run as if the code was on separate lines,
  # with the semicolon marking the line break

# Open a dataset and display the data
  # Replace “C:/…/” with the exact location of the file you want to open
  cars <- read.csv(file = "cars.txt", stringsAsFactors = FALSE)
  cars # To display the whole dataset, type the dataset name
  head(cars) # Display the first few records of a dataset
  names(cars) # Display variable names of a data frame, one kind of data in R
  cars$weight # Look at only the weight variable within data frame cars
  
# Matrices
  # Create a matrix with three rows, two columns, and every value equal to 0.0
  mat <- matrix(0.0, nrow = 3, ncol = 2); mat
  colnames(mat) <- c("Var 1", "Var 2") # Give a matrix variable names
  colnames(mat) # Display variable names of a matrix
  
# Subset data and declare new variables
  cars.rsub <- cars[1:50,] # Subset the data by rows
  cars.csub <- cars[,1:3] # Subset by columns
  cars.rcsub <- cars[c(1,3,5), c(2,4)] # Subset by specific rows and columns
  cars.vsub <- cars[which(cars$mpg> 30),] # Subset by a logical condition

# To declare new variables, type the variable name, a left-arrow, then the value of the variable
  firstletter <- 'a'
  weight <- cars$weight

# Display more than one figure at a time
  par(mfrow=c(1,1)) # plots one figure; the default setting
  par(mfrow=c(2,3)) # plots six figures: three in the top row, three in the bottom row
  # Plots will fill the plot space row by row
  
# Download and install an R Package
  # Example: ggplot2, from Chapter 3. install.packages("ggplot2")
  # Pick any CRAN mirror, as shown
  # Open the new package
  #install.packages("ggplot2")
  library(ggplot2)
  
  
  
          #####################
          ##    Chapter 2    ##
          #####################
  
# READ IN THE CARS AND CARS2 DATASETS
  cars <- read.csv("cars.txt", stringsAsFactors = FALSE)
  cars2 <- read.csv("cars2.txt", stringsAsFactors = FALSE)
  
# MISSING DATA
  # Look at four variables from cars
  cars.4var <- cars[, c(1, 3, 4, 8)]
  head(cars.4var)
  
  # Make certain entries missing
  cars.4var[2,2] <- cars.4var[4,4] <- NA
  head(cars.4var)
  
  # Replace missing values with constants
  cars.4var[2,2] <- 0
  cars.4var[4,4] <- "Missing"
  head(cars.4var)
  
  # Replace values with mean and mode
  cars.4var[2,2] <- mean(na.omit(cars.4var$cubicinches))
  our_table <- table(cars.4var$brand)
  our_mode <- names(our_table)[our_table == max(our_table)]
  cars.4var[4,4] <- our_mode
  head(cars.4var)
  
  # Generate random observations
  obs_brand <- sample(na.omit(cars.4var$brand), 1)
  obs_cubicinches <- sample(na.omit(cars.4var$cubicinches), 1)
  cars.4var[2,2] <- obs_cubicinches
  cars.4var[4,4] <- obs_brand
  head(cars.4var)
  
# CREATE A HISTOGRAM
  # Set up the plot area
  par(mfrow = c(1,1))
  
  # Create the histogram bars
  hist(cars2$weight,
       breaks = 30,
       xlim = c(0, 5000),
       col = "blue",
       border = "black",
       ylim = c(0, 40),
       xlab = "Weight",
       ylab = "Counts",
       main = "Histogram of Car Weights")
  
  # Make a box around the plot
  box(which = "plot", lty = "solid", col = "black")
  
# CREATE A SCATTERPLOT
  plot(cars2$weight, cars2$mpg,
       xlim = c(0, 5000), ylim = c(0, 600),
       xlab = "Weight", ylab = "MPG",
       main = "Scatterplot of MPG by Weight",
       type = "p", pch = 16, col = "blue")
  
  #Add open black circles
  points(cars2$weight, cars2$mpg,
         type = "p", col = "black")
  
# DESCRIPTIVE STATISTICS
  mean(cars$weight) # Mean
  median(cars$weight) # Median
  length(cars$weight) # Number of observations
  sd(cars$weight) # Standard deviation
  summary(cars$weight) # Min, Q1, Median, Mean, Q3, Max
  
# TRANSFORMATIONS
  # Min-max normalization
  summary(cars$weight)
  mi <- min(cars$weight)
  ma <- max(cars$weight)
  minmax.weight <- (cars$weight - mi)/(ma - mi)
  minmax.weight
  
  # Z-score standarization
  m <- mean(cars$weight); s <- sd(cars$weight)
  z.weight <- (cars$weight - m)/s
  z.weight
  length(cars$weight)
  
  # Decimal scaling
  max(abs(cars$weight)) # 4 digits
  d.weight <- cars$weight/(10^4); d.weight
  
# SIDE-BY-SIDE HISTOGRAMS
  par(mfrow = c(1,2))
  
  # Create two histograms
  hist(cars$weight, breaks = 20, xlim = c(1000, 5000),
       main = "Histogram of Weight", xlab = "Weight", ylab = "Counts")
  box(which = "plot", lty = "solid", col = "black")
  hist(z.weight, breaks = 20, xlim = c(-2, 3),
       main = "Histogram of Z- score of Weight",
       xlab = "Z-score of Weight", ylab = "Counts")
  box(which = "plot", lty = "solid", col = "black")
  
# SKEWNESS
  (3*(mean(cars$weight) - median(cars$weight)))/sd(cars$weight)
  (3*(mean(z.weight) - median(z.weight)))/sd(z.weight)
  
# TRANSFORMATIONS FOR NORMALITY
  sqrt.weight <- sqrt(cars$weight) # Square root
  sqrt.weight_skew <- (3*(mean(sqrt.weight) - median(sqrt.weight))) / sd(sqrt.weight)
  ln.weight <- log(cars$weight) # Natural log
  ln.weight_skew <- (3*(mean(ln.weight) - median(ln.weight))) / sd(ln.weight)
  invsqrt.weight <- 1 / sqrt(cars$weight) # Inverse square root
  invsqrt.weight_skew <- (3*(mean(invsqrt.weight) - median(invsqrt.weight))) /sd(invsqrt.weight)
  
# HISTOGRAM WITH NORMAL DISTRIBUTION OVERLAY
  par(mfrow=c(1,1))
  x <- rnorm(1000000, mean = mean (invsqrt.weight),
             sd = sd(invsqrt.weight))
  hist(invsqrt.weight, breaks = 30, xlim = c(0.0125, 0.0275),
       col = "lightblue", prob = TRUE, border = "black",
       xlab = "Inverse Square Root of Weight",
       ylab = "Counts", main = "Histogram of  Inverse Square Root of Weight")
  box(which = "plot", lty = "solid", col="black")
  
  # Overlay with  Normal density
  lines(density(x), col = "red")
  
# NORMAL Q-Q PLOT
  qqnorm(invsqrt.weight, datax = TRUE, col = "red", ylim = c(0.01, 0.03),
         main = "Normal Q-Q Plot of Inverse Square Root of Weight")
  qqline(invsqrt.weight, col = "blue", datax = TRUE)
  
# DE-TRANSFORM DATA
  # Transform x using y = 1 / sqrt(x)
  x <- cars$weight[1]; y <- 1 / sqrt(x)
  
  # Detransform x using x = 1 / (y)^2
  detransformedx <- 1 / y^2
  x; y; detransformedx
  
# CREATE INDICATOR VARIABLES
  north_flag <- east_flag <- south_flag <- c(rep(NA, 10))
  region <- c(rep(c("north", "south", "east", "west"),2), "north", "south")
  
  # Change the region variable to indicators
  for (i in 1:length(region)) {
    if(region[i] == "north") north_flag[i] = 1
    else north_flag[i] = 0
    if(region[i] == "east") east_flag[i] = 1
    else east_flag[i] = 0
    if(region[i] == "south") south_flag[i] = 1
    else south_flag[i] = 0
  }
  north_flag; east_flag; south_flag
  
# INDEX FIELDS
  # Data frames have an index field;
  # the left-most column
  cars
  cars[order(cars$mpg),]
  
  # For vectors or matrices,
  # add a column to act as an index field
  x <- c(1,1,3:1,1:4,3); y <- c(9,9:1)
  z <- c(2,1:9)
  matrix <- t(rbind(x,y,z)); matrix
  indexed_m <- cbind(c(1:length(x)), matrix); indexed_m
  indexed_m[order(z),]
  
# DUPLICATE RECORDS
  # For number of duplicate records, use anyDuplicated
  anyDuplicated(cars)
  
  # To examine each record, use Duplicated
  duplicated(cars)
  # ‘True’: record is a duplicate,
  # ‘False’: record is not a duplicate
  
  # Let's duplicate the first record
  new.cars <- rbind(cars, cars[1,])
  
  # Check for duplicates
  anyDuplicated(new.cars)
  
  # The 262nd record is a duplicate
  duplicated(new.cars)
  
#