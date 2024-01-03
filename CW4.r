## Group 07:
# Qingqing Fan
# Xuan Jin s2477282
# Zifan Huang s2550946

## Github repo:
# https://github.com/service000/Group-7.git

## Contribution:
#
# Xuan Jin wrote the fuctions 'netup','train','split_data'.Train dataset, review, 
# rectify, organize codes and rewrite comments.
# Zifan Huang wrote the 'backward' and 'train' function, complete the overview and 
# comments part.

## Overview: 
# This code wants to set up a basic neural network for classification, the main 
# method of training this network is the stochastic gradient descent which can make 
# the input data best predict the output data by repeatedly adjusting parameters 
# according to the loss function. These parameters are linking each network layer 
# to the next by combinations and transformations.
#
#
# The whole structure includes mainly 4 parts:
# 1. Initialization and preparation 
# Firstly, we write 'netup' function to initialze our neural network with nodes' 
# values, weights and offset vectors. The next function 'forward' will perform the 
# forward propagation in the neural networ, which will update network nodes' values 
# under the rule. Another function 'backward' will compute the derivatices of the 
# loss with the output of class k, it will return the gradients for nodes, weights 
# and offsets.
#
# 2. Training the data
# After preparation, we then write a function 'train' to train this neural network 
# 'nn' given the input data and its corresponding lables in vector. This function 
# returns the network nn after training under some optimization steps and a value 
# of randomly chosen mini-batches for updating parameters. The loss was dramatically 
# decreased after training 'nn'.
#
# 3. Split and process dataset (iris)
# In order to use 'train' function, we should preprocess dataset in a desired form. 
# Here, we use iris dataset and split it into training and testing features, training 
# and testing labels.
#
# 4. Testing and the misclassification rate
# Finally, we use trained neural network and prepared testing data to classify 
# the test data based on the most probable class in the final output layer. 
# Then, the misclassification rate can be obtained by counting how many rows were
# classified into wrong class and the total number of testing data rows.



set.seed(2) # Set seed for reproducibility as an example

netup <- function(d){
  # This function will set up the whole structure of the network, and also returns  
  # some initialized necessary elements that will be used in the future, such as  
  # the weight matrices and offset vectors.
  #
  # input:
  #  - d: vector, each element is the number of nodes in each layer of a network
  #
  # output:
  #  - nn: list, represents the network containing the initial nodes for each layer, 
  #        a list of weight matrices and a list of offset vectors.
  
  # Set initial nodes for each layer
  h <- lapply(d, function(x) numeric(x)) 
  
  # Set initialized weight matrices linking layer l to layer l+1
  W <- lapply(1:(length(d) - 1), function(i) {
    matrix(runif(d[i+1] * d[i], 0, 0.2), nrow = d[i+1], ncol = d[i])})
  
  # Set initialized offset vectors linking layer l to layer l+1
  b <- lapply(2:length(d), function(i){
    runif(d[i],0, 0.2)})
  
  # Wrap our variables into one list object and return it
  nn <- list(h=h,W=W,b=b)
  return(nn)
}


forward <- function(nn,inp){
  # Function forward(nn,inp) computes the node values of the network under the formula.
  #
  # input:
  #  - nn: a network list as returned by netup
  #  - inp: a vector of input values for the first layer
  # output:
  #  - nn: the update network list where the value for each node of h is calculated 
  
  # Extract h, W, and b from nn.
  h <- nn[[1]] 
  W <- nn[[2]]
  b <- nn[[3]] 
  n <- length(h) # Calculate the number of layers of the network
  h[[1]] <- inp # Set the first element of h is the input values of the network
  
  # Use for loop to calculate h in each layer
  for(i in 2:n){
    # The value of h is the maximum value between 0 and the calculated value.
    h[[i]] <- W[[i-1]]%*%h[[i-1]]+b[[i-1]]
    h[[i]][h[[i]]<0] <- 0
  }
  
  # Assign the new h to nn
  nn[[1]] <- h
  return(nn)
}


backward <- function(nn,k){
  # This function calculates the gradients for weights and offsets using back-propagation 
  # under formula, which is essential for training the neural network. It returns  
  # the derivatives of loss that are useful for future training process.
  #
  # input:
  #  - nn: a network list as returned by forward function
  #  - k: an output class, which is the label of the classification
  # output:
  #  - nn: the updated list of dh, dW and db
  
  # Extract h, W, and b from nn and initial dh, dW, db
  dh <- h <- nn$h 
  dW <- W <- nn$W
  db <- b <- nn$b 
  n1 <- length(h) # Calculate the number of layers of the network
  hL <- h[[n1]] # hL is the output of the network
  prob <- exp(hL)/sum(exp(hL)) # Compute the probility of the output
  # Define the true probability
  true <- numeric(length(prob))
  true[k] <- 1 
  dh[[n1]] <- prob-true # Compute the last layer of dh 
  # Calculate the gradient of each layer backwards
  for(i in (n1-1):1){
    db[[i]] <- dh[[i+1]] * (h[[i+1]] > 0) # Compute the db
    dh[[i]] <- t(W[[i]]) %*% db[[i]] # Compute dh
    dW[[i]] <- db[[i]] %*% t(h[[i]]) # Compute dW
  }
  
  # Return the results as a list nn
  nn <- list(dh=dh,dW=dW,db=db) 
  return(nn)}



train <- function(nn,inp,k,eta=.01,mb=10,nstep=10000) {
  # The function performs forward and backward propagation for each randomly chosen 
  # mini-batch rows given optimizaiton steps with its class vector k and then updates 
  # the network's weights and offsets based on the computed gradients.
  #
  # input:
  #  - nn: a network list as returned by netup
  #  - inp: an input matrix contains the training data features
  #  - k: an output class vector is the corresponding labels for inp
  #  - eta: learning rate is the step size
  #  - mb: the size of the randomly sample mini-batch for gradient
  #  - nstep: number of training steps for optimization
  # output:
  #  - nn: an output network after training
  
  # Optimize 10000 times using a for loop to do many things
  for (epoch in 1:nstep) {
    # Pick mb numbers randomly for getting small batches without replacement
    s <-sample(1:nrow(inp), mb, replace = FALSE)
    # Choose first mb rows from input matrix and get the corresponding labels
    sub_inp <- inp[s, ] 
    label <- k[s]
    
    # Construct an empty matrix for dW and db, they will become all zero after loop over one batch
    dW1 <- lapply(nn$W, function(w) matrix(0, nrow=nrow(w), ncol=ncol(w)))
    db1 <- lapply(nn$b, function(b) numeric(length(b)))
    
    # Loop over each data point in the mini-batch
    for (i in 1:mb) {
      # Apply forward and backward propagation to get derivatives
      nn <- forward(nn, as.numeric(sub_inp[i, ]))
      der <- backward(nn,label[i])
      dW <- der$dW
      db <- der$db
      # Update the gradients in dW1,db1
      dW1 <- lapply(seq_along(dW1), function(i) dW1[[i]] + dW[[i]])
      db1 <- lapply(seq_along(db1), function(i) db1[[i]] + db[[i]])
    }
    
    # Get the average of the gradients in the mini-batch
    dW <- lapply(dW1, function(x) x/mb)
    db <- lapply(db1, function(x) x/mb)
    
    # Update W and b according to gradients
    nn$W <- Map(function(w, dw) w - eta * dw, nn$W, dW)
    nn$b <- Map(function(b, db) b - eta * db, nn$b, db)
  }
  
  # Return trained neural network
  return(nn)
}

split_data <- function(dataset){
  # The function aims to split the whole dataset into training and testing data, 
  # each of them should include the rows with characteristics in matrix and its 
  # label vector. The testing data will start from row 5 and consists of every 
  # 5th row of the dataset.
  #
  # input:
  #  - dataset: input dataset with characteristics and labels, we assume the last 
  #             column should be its class vector
  #
  # output:
  #  - return a list containing training and testing characteristics and labels
  
  # Set the last label column to be numbers for classification
  dataset[ ,ncol(dataset)] <- as.numeric(dataset[ ,ncol(dataset)])
  
  # Create testing data index (every 5th row is used for testing)
  test_indices <- seq(5, nrow(dataset), by = 5)
  
  # Train and test data obtained by above index
  train_data <- dataset[-test_indices, ]
  test_data <- dataset[test_indices, ]
  
  # We do not need the feature names so delete them
  # Train feature rows matrix and train class vector
  train_x <- unname(train_data[ ,1:(ncol(train_data)-1)])
  train_y <- unname(train_data[ ,ncol(train_data)])
  # Test feature rows matrix and test class vector
  test_x <- unname(test_data[ ,1:(ncol(test_data)-1)])
  test_y <- unname(test_data[ ,ncol(test_data)])
  
  # Return the pre-processed data
  return(list(train_x=train_x, train_y=train_y, test_x=test_x, test_y=test_y))
}

output_prob <- function(nn,train_x){
  # This function calculates the probability of each node in the output layer 
  # in network for input data.
  #
  # input:
  #  - nn: a network we want to use for training data
  #  - train_x: a matrix in rows giving training characteristics
  #
  # output:
  #  - prob: a matrix in rows giving probability of output of the network
  
  # Compute the output of network for each train data
  output <- apply(train_x, 1, function(row) {
    nn1 <- forward(nn,row)
    return(nn1$h[[length(nn$h)]])
  })
  
  # Calculate the probability of train data 
  prob <- t(apply(exp(t(output)), 1, function(row) row / sum(row)))
  
}

calculate_loss <- function(nn,train_x,train_y){
  # The function calcultates the loss value for a network based on the formula of pk 
  # and the loss function 
  #
  # input:
  #  - nn: a network we want to use for training data
  #  - train_x: a matrix in rows giving training characteristics
  #  - train_y: a vector gives its labels
  #
  # output:
  #  - Loss: a value represents loss of this network output
  
  # Use output_prob function to calculate the probability
  prob <- output_prob(nn,train_x)

  # Calculate the loss
  Loss <- sum(sapply(1:nrow(prob), function(i) log(prob[i, train_y[i]])))
  Loss <- - Loss/nrow(prob)
  
  # Return this value
  return(Loss)
}


# Load iris dataset for training and testing
data(iris)
# Prapare training and testing features and labels data
train_x <- split_data(iris)$train_x
train_y <- split_data(iris)$train_y
test_x <- split_data(iris)$test_x
test_y <- split_data(iris)$test_y

# Train a 4-8-7-3 network
d <- c(4,8,7,3)
# Setup the initial network
nn <- netup(d)
# The loss value before training netwrok
print(paste("Loss before training is", calculate_loss(nn,train_x,train_y)))

# Calculate the time used for training our network
# Train our network using iris dataset
Rprof()
k <- train_y
inp <- train_x
nn <- train(nn,inp,k,eta=.01,mb=10,nstep=10000)
Rprof(NULL)
summaryRprof()

# The loss value after training netwrok
print(paste("Loss after training is", calculate_loss(nn,train_x,train_y)))
# The result shows the loss has been decreased a lot after training network 'nn'

# Initialize the vector for the prediction of test data
test_predict <- rep(0,length(test_x[,1]))

predict <- function(nn,test_x,test_y){
  # The function is used to classify the data by the network nn and calculates 
  # the misclassification rate of the prediction.
  #
  # input:
  #  - nn: a network we want to use for predicting data
  #  - test_x: a matrix in rows giving testing characteristics
  #  - test_y: a vector gives its labels
  #
  # output:
  #  - predict: the prediction of testing data using network nn
  #  - mis_rate: the misclassification rate of the prediction.
  
  # Compute the probability of output for each testing data
  prob <- output_prob(nn,test_x)
  # Choose the maximum probability output as the prediction class
  test_predict <- apply(prob, 1, which.max)
  # Compute the misclassification rate
  mis_rate <- 1-sum(test_predict == test_y)/length(test_y); mis_rate
  return(list(predict = test_predict, mis_rate = mis_rate))
}
print(paste("The misclassification rate is", predict(nn,test_x,test_y)$mis_rate))

# The misclassification rate is zero, which is a good example showing this netwok has worked
