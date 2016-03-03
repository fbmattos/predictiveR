#############################################################
#############################################################
###### KNN
#############################################################
#############################################################

# Load needed libraries
library(gmodels)
library(class)

source('~fbmattos/Desktop/R/predictiveR/QTM2000.R')

# Load original dataset
full <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/GermanCredit.csv', header = TRUE, sep = ",")
str(full) # details on data fields

# focus on numerical variables only (KNN)
reduced = full[,c("DURATION", "AMOUNT", "AGE", "RESPONSE")]
str(reduced)

# Split into training & validation
split = 0.8
N = nrow(reduced)
trainingSize = round(N*split)
trainingCases = sample(N, trainingSize)
training = reduced[trainingCases,]
test = reduced[-trainingCases,]

# 'knn' function requires that result be stored as a factor.
# Currently, RESPONSE is 0 or 1 (integer). Let's fix that:
predictions = knn(training, test, as.factor(training$RESPONSE), k=5)

resultsTable = CrossTable(predictions, test$RESPONSE, expected=F, prop.r=F, prop.c=F, prop.t=F, pop.chisq=F)

totalErrors = sum(predictions != test$RESPONSE)
errorRate = totalErrors/nrow(test)

benchmarkErrorRate(training$RESPONSE, test$RESPONSE)


###### With STANDARDIZATION now
stanTraining = easyStandardize(training, c(1,2,3))
stanTest = easyStandardize(test, c(1,2,3))

predictions = knn(stanTraining, stanTest, as.factor(stanTraining$RESPONSE), k=5)

resultsTable = CrossTable(predictions, stanTest$RESPONSE, expected=F, prop.r=F, prop.c=F, prop.t=F, pop.chisq=F)

totalErrors = sum(predictions != stanTest$RESPONSE)
errorRate = totalErrors/nrow(stanTest)

benchmarkErrorRate(training$RESPONSE, stanTest$RESPONSE)

