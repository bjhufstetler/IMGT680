          #####################
          ##    Chapter 10    ##
          #####################
          
          
# CREATE THE DATA USING TABLE 10.3
new <- c(0.05,0.25)
A <- c(0.0467,	0.2471)
B <- c(0.0533,	0.1912)
C <- c(0.0917,	0.2794)
	
# Create dataframe object and establish row/column names
data <- rbind(A, B, C)
dimnames(data) <- list(c("Dark", "Medium", "Light"),c("Age (MMN)", "Na/K (MMN)"))

# Declare true classifications of A, B, and C.
trueclass <- c("Dark", "Medium", "Light")

# Run KNN
# Requires package “class”
library(class)
knn <- knn(data, new, cl = trueclass, k = 3, prob = TRUE)
knn

# Calculate the Euclidean distance
# Requires package "fields"
library(fields)

together <- rbind(new, data) 
# The top row has the distances from New
rdist(together)

# Stretch the axes
ds_newA <- sqrt((new[1]-A[1])^2 + (3*(new[2]-A[2]))^2)
ds_newB <- sqrt((new[1]-B[1])^2 + (3*(new[2]-B[2]))^2)
ds_newC <- sqrt((new[1]-C[1])^2 + (3*(new[2]-C[2]))^2)

# Table 10.4
# Same thing as previous but adding BP as another variable
distance <- c(ds_newA, ds_newB, ds_newC)
BP <- c(120, 122, 130)
data <- cbind(BP, data, distance)
data

# Locally Weighted Averaging
weights <- (1/(distance^2))
sum_wi <- sum(weights)
sum_wiyi <- sum(weights*data[,1])
yhat_new <- sum_wiyi/sum_wi
yhat_new






# CLASSFIY RISK EXAMPLE: PREP THE DATA
#	Read in the ClassifyRisk dataset
risk <- read.csv(file = "classifyrisk.txt", stringsAsFactors=FALSE, header=TRUE, sep="\t")

# Table <link	href="#urn:x-wiley:9783527333455:xml-component:c10:c10-tbl0005"/> contains Records 51, 65, 79, 87, 124, 141, 150, 162, 163
# Pull select samples from risk table along with selected variables
risk2 <- risk[c(51, 65, 79, 87, 124, 141, 150, 162), c(5, 1, 4, 6)]

# Categorical variables cannot be used in modeling
# Therefore turn marriage status into an indicator vairable
risk2$married.I <- ifelse(risk2$marital_status=="married", 1, 0)
risk2$single.I <- ifelse(risk2$marital_status=="single", 1, 0)

# Remove the two original categorical variables from the set
risk2 <- risk2[, -2];
risk2 <- risk2[, -2];

# Pull an observation out to be the test sameple and repeat the above process
new2	<- risk[163, c(5, 1, 4)] 
new2$married.I <- 1;
new2$single.I <- 0; 
new2 <- new2[, -2];
new2 <- new2[, -2];

# Establish response label
cll <- c(risk2[, 2])

# ClassifyRisk example: KNN
# Train the KNN model and test it against the one sample pulled for the test set
knn2 <- knn(train = risk2[,c(1,3,4)], test = new2, cl = cll, k = 3)

#Display results
knn2  
