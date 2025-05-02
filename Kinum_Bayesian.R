set.seed(1234)
wine <- read.csv("winequality-red.csv",header = T)
wine[,-12] <- scale(wine[,-12])
n <- nrow(wine)
train_row <- sample(1:n,size = round(0.7*n) ) #Approximately 70% for training

#Separate into training and testing data
wine_train <- wine[train_row,]
wine_test <- wine[-train_row,]

#Assume Normal and estimate parameters

train_low <- subset(wine_train,quality <=4)
train_med <- subset(wine_train,quality==5|quality==6)
train_high <- subset(wine_train,quality >=7)

Prior <- data.frame(
  low = nrow(train_low)/n,
  med = nrow(train_med)/n,
  high = nrow(train_high)/n
)

#Estimate Parameters
mu <- data.frame(
  low = colMeans(train_low[,-12]),
  med = colMeans(train_med[,-12]),
  high = colMeans(train_high[,-12])
)
Sigma <- list(
  low = cov(train_low[,-12]),
  med = cov(train_med[,-12]),
  high = cov(train_high[,-12])
)

classify <- function(data_row,first,second){
  Sigma1 <- Sigma[[first]]
  Sigma2 <- Sigma[[second]]
  mu1 <- mu[[first]]
  mu2 <- mu[[second]]
  Prior1 <- Prior[[first]]
  Prior2 <- Prior[[second]]
  
  Sigma1_inv <- solve(Sigma1)
  Sigma2_inv <- solve(Sigma2)
  
  #Weights
  W1 <- -1/2*Sigma1_inv
  W2 <- -1/2*Sigma2_inv
  
  w1 <- Sigma1_inv%*%mu1
  w2 <- Sigma2_inv%*%mu2
  
  w10 <- -1/2*t(mu1)%*%Sigma1_inv%*%mu1-1/2*log(det(Sigma1))+log(Prior1)
  w20 <- -1/2*t(mu2)%*%Sigma2_inv%*%mu2-1/2*log(det(Sigma2))+log(Prior2)
  
  #Coefficients
  A <- W1-W2
  B <-w1-w2
  C <- w10-w20
  dat <- as.matrix(data_row)
  value <- dat%*% A %*% t(dat) +t(B) %*% t(dat) + C
  return(value) #Positive if first item and Negative if second item
}

classify_points <- function(data){
  low_vs_med  <-classify(data,1,2)
  low_vs_high <- classify(data,1,3)
  med_vs_high <- classify(data,2,3)
  
  #Determine class based on boundary evaluations
  if(low_vs_med > 0 && low_vs_high > 0){
    return("low")
  }else if(low_vs_med <= 0 && med_vs_high > 0){
    return("med")
  }else{
    return("high")
  }
}
  
predictions <- c()
for (i in 1:nrow(wine_test)){
  prediction <- classify_points(wine_test[i,-12])
  predictions <- append(predictions,prediction)
}
wine_test$predicted_quality <- predictions

#Replace with "low","med","high"
class <- c()
for (i in 1:nrow(wine_test)){
  if(wine_test$quality[i] <=4 ){
    class[i] <- "low"
  }else if(wine_test$quality[i] >=7){
    class[i] <- "high"
  }else{
    class[i] <- "med"
  }
}

wine_test$class <- class

confusion_matrix <- table(Predicted = wine_test$predicted_quality,
                          Actual = wine_test$class)

#Reorder so it goes low,med,high
new_order <- c(2:nrow(confusion_matrix), 1)

# Reorder rows and columns
confusion_matrix <- confusion_matrix[new_order, new_order]

accuracies <- data.frame(
  total = sum(diag(confusion_matrix))/sum(confusion_matrix)*100,
  low = confusion_matrix[1,1]/ sum(confusion_matrix[1,]) *100,
  med = confusion_matrix[2,2]/ sum(confusion_matrix[2,]) *100,
  high = confusion_matrix[3,3]/ sum(confusion_matrix[3,]) *100
)

# Print the confusion matrix and accuracies

for(i in 1:length(accuracies)){
  if( i == 1){
    print(confusion_matrix)
    cat("\n")
  }
  print(paste(colnames(accuracies[i]),"accuracy =",round(accuracies[i],2),"%"))
}