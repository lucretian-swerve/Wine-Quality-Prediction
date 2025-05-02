library(olsrr)
set.seed(1234)
wine <- read.csv("winequality-red.csv",header = T)

#scale data
wine[,-12] <- scale(wine[,-12])

#Replace numerals with characters
wine$quality <- ifelse(wine$quality %in% 3:4, 1,
                       ifelse(wine$quality %in% 5:6, 2, 3))

#Seperate into training and testing
n <- nrow(wine)
train_row <- sample(1:n,size = round(0.7*n) ) #Approximately 70% for training

#Seperate into training and testing data
wine_train <- wine[train_row,]
wine_test <- wine[-train_row,]

wine.fit <- lm(quality~.,data=wine_train)
all.models <- ols_step_best_subset(wine.fit, metric = "cp")

plot(all.models)
print(all.models)

# Regressors = "volatile.acidity", "chlorides ", "pH ","sulphates", "alcohol")

wine.model <- lm(quality~volatile.acidity+chlorides+
pH+sulphates+alcohol,data=wine_test)

summary(wine.model)

#make predictions based on test data
predictions <- predict(wine.model,newdata=wine_test[,-12])
#round predictions
predictions <- round(predictions)

confusion <- table(Predicted = predictions, Actual = wine_test$quality)
accuracy <- mean(predictions == wine_test$quality)
confusion
paste("Accuracy of Linear Regression Model with test data: ",100*round(accuracy,4),"%")