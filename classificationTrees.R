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
df <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/UniversalBank.csv', header = TRUE, sep = ",")
df$Personal.Loan = as.factor(df$Personal.Loan)

# Split into training and test
idx = proportionalPartition(df, df$Personal.Loan, 0.8)
training = df[idx,]
test = df[-idx,]

# Create model
model = rpart(Personal.Loan ~ ., data=training)
fancyRpartPlot(model, main="Full Tree")
predictions = predict(model, test, type="class")

#################################
### Stopping Rules
# minsplit = size of the smallest node we allow to split
# minbucket = smallest allowable number of nodes in any leaf
# cp = minimum allowable increase in fit.
stoppingRules = rpart.control(minsplit=40, minbucket=20, cp=0)
model = rpart(Personal.Loan ~ ., data=training, control=stoppingRules)
fancyRpartPlot(model, main="Full Tree")
predictions = predict(model, test, type="class")
predictionsTF = (predictions == 1)

# Get results and error rate
test$Personal.LoanTF = (test$Personal.Loan == 1)
resultsTable = CrossTable(predictionsTF, test$Personal.LoanTF, expected=F, prop.r=F, prop.c=F, prop.t=F, pop.chisq=F)

errorRate = sum(predictionsTF != test$Personal.LoanTF)/nrow(test)

benchmarkErrorRate = benchmarkErrorRate(training$Personal.Loan, test$Personal.Loan)

# Lift Chart
liftChart(test$Personal.LoanTF, predictionsTF)


#################################
### Pruning
stoppingRules = rpart.control(cp=-1)
modelFull = rpart(Personal.Loan ~ ., data=training, control=stoppingRules)
modelPruned = easyPrune(modelFull)


fancyRpartPlot(modelFull, main="Full Tree")
fancyRpartPlot(modelPruned, main="Pruned Tree")

predictionsFull = predict(modelFull, test, type="class")
predictionsFullTF = (predictionsFull == 1)

predictionsPruned = predict(modelPruned, test, type="class")
predictionsPrunedTF = (predictionsPruned == 1)


# Get results and error rate
test$Personal.LoanTF = (test$Personal.Loan == 1)

errorRateFull = sum(predictionsFullTF != test$Personal.LoanTF)/nrow(test)
errorRatePruned = sum(predictionsPrunedTF != test$Personal.LoanTF)/nrow(test)

benchmarkErrorRate = benchmarkErrorRate(training$Personal.Loan, test$Personal.Loan)

# Lift Chart
liftChart(test$Personal.LoanTF, predictionsFullTF)
liftChart(test$Personal.LoanTF, predictionsPrunedTF)

