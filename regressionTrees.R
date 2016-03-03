#############################################################
#############################################################
###### Regression Trees
#############################################################
#############################################################

#############################################################
#############################################################
###### Classification Trees
#############################################################
#############################################################

# Load needed libraries
out <- tryCatch(
  {
    library(rpart)
  },
  error=function(cond) {
    install.packages("rpart")
    library(rpart)
  })

out <- tryCatch(
  {
    library(rpart.plot)
  },
  error=function(cond) {
    install.packages("rpart.plot")
    library(rpart.plot)
  })

out <- tryCatch(
  {
    library(rattle)
  },
  error=function(cond) {
    install.packages("rattle")
    library(rattle)
  })

source('~fbmattos/Desktop/R/predictiveR/QTM2000.R')

# Load data file
df <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/ToyotaCorolla.csv', header = TRUE, sep = ",")

# Remove factors
df$Fuel_Type = NULL
df$Model = NULL

# Split into training and test
idx = proportionalPartition(df, df$Price, 0.8)
training = df[idx,]
test = df[-idx,]

# Create model
stoppingRules = rpart.control(minsplit=40, minbucket=20, cp=0)
model = rpart(Price ~ ., data=training, control=stoppingRules)
fancyRpartPlot(model, main="Full Tree")
predictions = predict(model, test)

