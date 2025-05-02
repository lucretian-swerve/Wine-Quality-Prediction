set.seed(1234)

wine <- read.csv("winequality-red.csv")              

#Replace scores with 1,2,3: low, medium, high
wine$quality <- ifelse(wine$quality %in% 3:4, 1,
                       ifelse(wine$quality %in% 5:6, 2, 3))

x <- wine[,-12]   #features
y <- wine$quality  #classes

#Scale input features
x <- scale(x)

#Convert Y to one-hot encoding
Y <- matrix(0, nrow=length(y), ncol=max(y))
for(i in 1:nrow(Y)) {
  Y[i, y[i]] <- 1 
}

y <- Y

# Network architecture
input_size <- ncol(x)
hidden_size <- 64   #Number of nodes in hidden layer
output_size <- ncol(y)

# Initialize weights and biases
w1 <- matrix(rnorm(input_size * hidden_size), nrow=input_size, ncol=hidden_size)
b1 <- matrix(0, nrow=1, ncol=hidden_size)
w2 <- matrix(rnorm(hidden_size * output_size), nrow=hidden_size, ncol=output_size)
b2 <- matrix(0, nrow=1, ncol=output_size)

# Sigmoid activation function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Sigmoid derivative function
sigmoid_derivative <- function(x) {
  sig <- sigmoid(x)
  sig * (1 - sig)
}

# Softmax function for output layer
softmax <- function(x) {
  exp(x) / rowSums(exp(x))
}


# Forward propagation
forward <- function(x, w1, b1, w2, b2) {
  #Replicate b across all rows of z to ensure proper addition
  z1 <- x %*% w1 + matrix(rep(b1, nrow(x)), nrow=nrow(x), byrow=TRUE)  
  a1 <- sigmoid(z1)
  z2 <- a1 %*% w2 + matrix(rep(b2, nrow(a1)), nrow=nrow(a1), byrow=TRUE)
  a2 <- softmax(z2)
  list(a1=a1, a2=a2, z1=z1, z2=z2)
}

# Function to calculate accuracy
compute_accuracy <- function(predictions, labels) {
  correct_predictions <- sum(apply(predictions, 1, which.max) == apply(labels, 1, which.max))
  accuracy <- correct_predictions / nrow(labels)
  return(accuracy)
}

# Backward propagation
backward <- function(x, y, forward_cache, w1, w2, learning_rate) {
  m <- nrow(x)
  
  dz2 <- forward_cache$a2 - y
  dw2 <- t(forward_cache$a1) %*% dz2 / m
  db2 <- colSums(dz2) / m
  
  da1 <- dz2 %*% t(w2)
  dz1 <- da1 * sigmoid(forward_cache$z1) * (1 - sigmoid(forward_cache$z1))
  dw1 <- t(x) %*% dz1 / m
  db1 <- colSums(dz1) / m
  
  w1 <- w1 - learning_rate * dw1
  b1 <- b1 - learning_rate * db1
  w2 <- w2 - learning_rate * dw2
  b2 <- b2 - learning_rate * db2
  
  list(w1=w1, b1=b1, w2=w2, b2=b2)
}

train <- function(train_x, train_y, test_x, test_y, iterations, learning_rate) {
  for(iteration in 1:iterations) {
    forward_cache <- forward(train_x, w1, b1, w2, b2)
    train_accuracy <- compute_accuracy(forward_cache$a2, train_y)
    
    # Forward pass on test data
    test_forward_cache <- forward(test_x, w1, b1, w2, b2)
    test_accuracy <- compute_accuracy(test_forward_cache$a2, test_y)
    
    if(iteration==1 || iteration %%50 ==0){
      cat("Iteration:", iteration, "Train Accuracy:", train_accuracy, "Test Accuracy:",
          test_accuracy, "\n")
    }
    params <- backward(train_x, train_y, forward_cache, w1, w2, learning_rate)
    w1 <<- params$w1
    b1 <<- params$b1
    w2 <<- params$w2
    b2 <<- params$b2
  }
}

#Splid data into training and testing
train_rows <- sample(1:nrow(x), size = round(0.7 * nrow(x))) #Approximately 70% for training
train_x <- x[train_rows, ]
train_y <- y[train_rows, ]
test_x <- x[-train_rows, ]
test_y <- y[-train_rows, ]


# Train the network using training and testing data
train(train_x, train_y, test_x, test_y,iterations = 2000,learning_rate = 0.01)

# Final forward pass on test data
test_forward_cache <- forward(test_x, w1, b1, w2, b2)
test_predictions <- test_forward_cache$a2
predicted_classes <- apply(test_predictions, 1, which.max)
actual_classes <- apply(test_y, 1, which.max)  

#Confusion matrix
confusion <- table(predicted_classes, actual_classes)
accuracy <- sum(diag(confusion))/sum(confusion)
confusion
paste("Accuracy of final model with test data: ",100*round(accuracy,4),"%")