#############################################################
#############################################################
###### Linear Regression
#############################################################
#############################################################

# Load needed libraries
out <- tryCatch(
  {
    library(gmodels)
  },
  error=function(cond) {
    install.packages("gmodels")
    library(gmodels)
  })  


# Load original dataset
df <- read.csv('~fbmattos/Desktop/R/predictiveR/Rawdata/ToyotaCorolla.csv', header = TRUE, sep = ",")

# Split into training & validation
split = 0.7
N = nrow(df)
trainingSize = round(N*split)
trainingCases = sample(N, trainingSize)

training = df[trainingCases,]
test = df[-trainingCases,]

###### Models - Manual creation
model1 = lm(Price ~ 1, data=training)
model2 = lm(Price ~ Age+KM+Weight+cc+HP+Fuel_Type, data=training)
model3 = lm(Price ~ ., data=training)

summary(model1)
summary(model2)
summary(model3)


###### Models - Forward and Backward selection
model1 = lm(Price ~ 1, data=training)
model3 = lm(Price ~ ., data=training)

step(model1, scope = list("upper" = model3), direction = "forward")
# Result: lm(formula = Price ~ Model + Age + KM + Weight, data = training)

step(model3, scope = list("lower" = model1), direction = "backward")
# Result: lm(formula = Price ~ Model + Age + KM + Weight, data = training)
# AIC = 15862.94
# AIC=2k - 2ln(L), where k is no of predictor and L is likelihood of model. The lower the AIC the better





predictions = predict(model, test)

