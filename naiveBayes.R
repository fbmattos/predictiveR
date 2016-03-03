#############################################################
#############################################################
###### Naive Bayes
#############################################################
#############################################################

# Load needed libraries
out <- tryCatch(
  {
    library(e1071)
  },
  error=function(cond) {
    install.packages("e1071")
    library(e1071)
  })  

source('~fbmattos/Desktop/R/predictiveR/QTM2000.R')

# Load dataset
full <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/FlightDelays.csv', header = TRUE, sep = ",")
str(full) # details on data fields

# Remove some variables
# focus on numerical variables only (KNN)
full$DISTANCE = NULL
full$Weather= = NULL
full$DAY_OF_MONTH = NULL
full$TAIL_NUM = NULL
full$FL_NUM = NULL
str(full) # details on data fields

# Convert DAY_WEEK to a factor
full$DAY_WEEK = as.factor(full$DAY_WEEK)
str(full) # details on data fields

# Split into training and test
idx = proportionalPartition(full, full$Flight.Status, 0.8)
training = full[idx,]
test = full[-idx,]

# Naive Bayes model
model = naiveBayes(Flight.Status ~ ., data=training)
predictions = predict(model, test)

# Get results and error rate
resultsTable = CrossTable(predictions, test$Flight.Status, expected=F, prop.r=F, prop.c=F, prop.t=F, pop.chisq=F)

errorRate = sum(predictions != test$Flight.Status)/nrow(test)

benchmarkErrorRate(training$Flight.Status, test$Flight.Status)

