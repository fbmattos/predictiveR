#############################################################
#############################################################
###### Logistic Regression
#############################################################
#############################################################

# Load needed libraries
source('~fbmattos/Desktop/R/predictiveR/QTM2000.R')

df <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/BostonHousing.csv', header = TRUE, sep = ",")

# Split into training and test
idx = proportionalPartition(df, df$IsMale, 0.8)
training = df[idx,]
test = df[-idx,]

bigModel = glm(CAT..MEDV ~ CRIM+INDUS+NOX+RM+AGE, data=training, family = binomial(logit))
littleModel = step(bigModel) #default direction is backwards

summary(littleModel)

predictions = predict(littleModel, test, type="response")
predictionsTF = (predictions > 0.5)


# Get results and error rate
test$CAT..MEDVTF = (test$CAT..MEDV > 0.5)
resultsTable = CrossTable(predictionsTF, test$CAT..MEDVTF, expected=F, prop.r=F, prop.c=F, prop.t=F, pop.chisq=F)

errorRate = sum(predictionsTF != test$CAT..MEDVTF)/nrow(test)

benchmarkErrorRate = benchmarkErrorRate(training$CAT..MEDV, test$CAT..MEDV)

# Lift Chart
liftChart(test$CAT..MEDVTF, predictionsTF)

# Manually compare observations to predictions:
temp$obs = test$CAT..MEDV
temp$pred = predictions
View(temp)

###############################
# Comparing LIFT CHARTS
model1 = glm(CAT..MEDV ~ 1, data=training, family = binomial(logit))
model2 = glm(CAT..MEDV ~ RM+INDUS, data=training, family = binomial(logit))

test$CAT..MEDVTF = (test$CAT..MEDV > 0.5)

predictions1 = predict(model1, test, type="response")
predictions1TF = (predictions1 > 0.5)

predictions2 = predict(model2, test, type="response")
predictions2TF = (predictions2 > 0.5)

errorRate1 = sum(predictions1TF != test$CAT..MEDVTF)/nrow(test)
errorRate2 = sum(predictions2TF != test$CAT..MEDVTF)/nrow(test)

liftChart(test$CAT..MEDVTF, predictions1TF)
liftChart(test$CAT..MEDVTF, predictions2TF)

##########################################################
# ROC Chart ==> Receiver Operating Characteristic
# Shows the performance of the model as the threshold is varied.
# Plot: True positive rate (TPR) x the false positive rate (FPR) at various thresholds.
# True-positive rate = Sensitivity (or "recall" in machine learning)
# False-positive rate = fall-out = (1 - specificity)

ROCChart(test$CAT..MEDV, predictions1)
ROCChart(test$CAT..MEDV, predictions2)

