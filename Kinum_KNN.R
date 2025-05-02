set.seed(1234)
wine <- read.csv("winequality-red.csv",header = T)

#scale data
wine[,-12] <- scale(wine[,-12])

#Replace numerals with characters
wine$quality <- ifelse(wine$quality %in% 3:4, "low",
                       ifelse(wine$quality %in% 5:6, "med", "high"))

#Seperate into training and testing
n <- nrow(wine)
train_row <- sample(1:n,size = round(0.7*n) ) #Approximately 70% for training

#Seperate into training and testing data
wine_train <- wine[train_row,]
wine_test <- wine[-train_row,]


#K nearest neighbors
knn <- function(train_data, train_labels, test_data, k) {
  # Compute pairwise distances
  distance_matrix <- as.matrix(dist(rbind(test_data, train_data)))
  test_size <- nrow(test_data)
  train_size <- nrow(train_data)
  
  # rows represent test point, columns represent training points
  distance_matrix <- distance_matrix[1:test_size, 
                                     (test_size + 1):(test_size + train_size)]
  
  # Initialize predictions
  predictions <- vector("character", length = nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    # Find the indices of the k-nearest neighbors
    neighbor_indices <- head(order(distance_matrix[i, ]), k)
    
    # Get the labels of the nearest neighbors
    neighbor_labels <- train_labels[neighbor_indices]
    
    # Use majority vote for classification
    predictions[i] <- names(which.max(table(neighbor_labels)))
  }
  
  return(predictions)
}

accuracies <- c() #Vector to store accuracies


for(k in 1:50){
  predictions <- knn(wine_train[,-12], wine_train$quality, wine_test[,-12], k=k)
  accuracy <- mean(predictions == wine_test$quality)
  accuracies <- c(accuracies,accuracy)
}

max <- which.max(accuracies)

plot(accuracies,xlab="k",ylab="Accuracies", type="o",
     main="Accuracies of KNN vs K")
points(max,accuracies[max],pch=10,col="red",cex=2)
text(x = 25, y = par("usr")[3]+0.005,paste("Max Accuracy: k =",max))


predictions <- knn(wine_train[,-12], wine_train$quality, wine_test[,-12], k=max)
confusion <- table(Predicted = predictions, Actual = wine_test$quality)

confusion
100*round(accuracies[max],4)