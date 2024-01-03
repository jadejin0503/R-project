# Name: Xuan Jin
# Student number: S2477282 

##  Overview
#
# This code wants to write some R functions for smoothing x_i and y_i data for i= 1,...,n and x_i is a 
# vector including 2 values. 
#
# There are mainly 3 parts: 
#
# 1. Transform math problems into R language and set up essential values for the spline:
# Firstly, we randomly select k points from x if n large enough, otherwise k = n. k is normally large but
# it will over-fit model. Thus minimizing a weighted sum of lack of fit and wiggliness of f is necessary. 
# After rewrite the objective and constraint, the current task is to get X and S matrices by matrix 
# manipulations. To do it needs to find T and T star matrices, and get Z matrix by QR-decomposition of T star. 
# Then, E and E star should be obtained by eta function with x and xk. Finally, find X and S as preparation.
#
# 2. Fit the thin plate splines, and choose the smoothing parameter by GCV:
# Apply QR decomposition for X, use R and S to do eigen decomposition. And then search for the GCV minimizing 
# smoothing parameter. We will also calculate beta, mu and effective degrees of freedom values using the chosen 
# lambda. These values will be used in the next plotting part.
#
# 3. Plot the fitted thin plate spline:
# Call fitTPS function to get essential values for finding delta and alpha to predict. Write a function to fit our 
# spline and then we can get perspective and contour plots for the fitted spline to visualize our result.
#


eta <- function(q){
  # Define eta function that will be used in future calculations.
  #
  # input:
  #  - q: if q is greater than zero, than return our eta function, otherwise return zero.
  #
  # output:
  #  - depends on the value of input argument, then return our function or zero
  
  if(q>0)
    return((q**2)*log(q))
  else
    return(0)
}

getTPS1 <- function(x,k=100){
  # This function mainly wants to set up necessary matrices for re-parameterization used in the next part.
  # Follow the formula step by step to get our desired output matrices.
  #
  # input:
  #  - x: n*2 matrix
  #  - k: the number of basis function to use
  #
  # output:
  #  - xk: k*2 x star matrix
  #  - X: n*k X matrix
  #  - S: k*k S matrix
  #  - Z: k*(k-3) Z matrix

  
  # Find x star matrix by rules
  n <- nrow(x)
  # Just set k=n if n is not too large
  if (k >= n){ 
    k <- n 
    xk <- x } 
  # Otherwise, randomly sample k rows from x matrix as our x star matrix -- xk
  else {xk <- x[sample(1:n,k,replace = FALSE), ] }

  # Find T star and T matrices by x star and x matrices
  # They will be used to calculate Z and X matrices
  Ts <- cbind(1,xk) # T star
  Tr <- cbind(1,x) # T 
  Z <- qr.Q(qr(Ts),complete = TRUE)[,-(1:3)] # Get Z by QR-decomposition of T star matrix
  
  # Find E and E star matrix 
  # Firstly, create two matrices with desired dimensions
  E <- matrix(0, nrow=n, ncol= k) # E
  Es <- matrix( 0, nrow=k, ncol= k) # E star 
  
  # Use for loop to fill those matrices E and E star
  # In this part, for loop maybe less expensive than vectorization by two mapply() after trials
  for (i in 1:n){
    for (j in 1:k){
      r <- sqrt(sum((x[i,] - xk[j,])**2)) # Calculate distance before putting into eta function
      E[i,j] <- eta(r)
    }
  }
  r <- 0
  for (i in 1:k){
    for (j in 1:k){
      r <- sqrt(sum((xk[i,] - xk[j,])**2)) # Same method with above codes but different dimension
      Es[i,j] <- eta(r)
    }
  }
  
  # Calculate X and S matrices by formula
  X <- cbind(E%*%Z, Tr)
  S <- matrix(0,k,k)
  value <- t(Z)%*% (Es %*% Z)
  S[1:(k-3),1:(k-3)] <- value 
  
  # Output our result of a list with named items
  # Return Z matrix since future calculates need it and we do not want to recompute
  return(list(xk=xk,X=X,S=S,Z=Z))
}


fitTPS <- function(x,y,k=100,lsp=c(-5,5)){
  # This function will get values from the getTPS first, and apply QR and eigen decompositions for required transformation.
  # Then search for the best smooth parameter lambda with the smallest GCV value over a grid of 100 values evenly 
  # spaced on the log scale between (-5,5). Return an object of class tps including a list of desired named values.
  #
  # input:
  #  - x: n*2 matrix
  #  - y: the n vector of values to smooth
  #  - k: the number of basis function to use
  #  - lsp: the log lambda limits to search for evenly the optimal smoothing parameter values between which
  #
  # output:
  #  - beta: the best fit beta hat at the optimal lambda value
  #  - mu: the best fit mu hat at the optimal lambda value
  #  - medf: the effective degrees of freedom at the optimal lambda value
  #  - lambda: the vector of 100 lambda values we searched over
  #  - gcv: contain all GCV scores' vector
  #  - edf: contain all EDFs vector
  #  - xk: x star matrix will be used in the next part
  #  - Z: Z matrix will be used in the next part
  #  - k: the number of basis function to use
  
  # Call getTPS function to set up necessary values
  getTPS <- getTPS1(x,k=100)
  X <- getTPS$X
  S <- getTPS$S
  xk <-getTPS$xk
  Z <- getTPS$Z
  
  # Set up a grid of 100 values evenly spaced between lsp[1] and lsp[2]
  # Do exponential transformation for given lsp values
  lambda <- exp(seq(lsp[1],lsp[2],length=100))
  
  # QR-decomposition of X
  Q <- qr.Q(qr(X))
  R <- qr.R(qr(X))
  # Store R^(-1) used in the future to reduce operation
  R_inverse <- solve(R) 
  # Eigen-decomposition to find matrix U and D
  va <- t(R_inverse) %*% (S %*% R_inverse) # the value for decomposition
  va <- (t(va)+va)*.5  # force this value to be symmetric in case of problems
  ec <- eigen(va)
  # Define eigen-vector, eigen-values, and the diagonal matrix D with eigen-values in main diagonal
  U <- ec$vectors ; eigen_velue <- ec$values; D <- diag(eigen_velue)
  
  # Store some values for improving running time
  R_inverse_U <- R_inverse %*% U
  transpose_U_Q_y <- t(U) %*% (t(Q)%*% y)
  # Identity matrix
  diag_len <- diag(length(lambda))
  
  # Define edf and gcv score vectors with same length of lambda
  edf <- gcv <- lambda*0
  k <- ncol(D) ; n <-nrow(X)
  
  # For loop to search for the best lambda
  for (i in 1:length(lambda)) {
    # Store effective degrees of freedom
    edf[i] <- sum(1/(1+lambda[i]*eigen_velue))
    # Coefficient estimates for getting GCV value
    b.hat <- R_inverse_U %*% (solve(diag_len + lambda[i]*D) %*% transpose_U_Q_y)  
    m.hat <- X %*% b.hat    
    gcv[i] <- sum((y-m.hat^2))/(n-edf[i])^2  
  }
  
  # Fin the optimal lambda
  index <- which.min(gcv)
  spD <- lambda[index]*D
  # Calculate the corresponding values of beta hat, mu hat and medf by formula
  beta <- R_inverse_U %*% (solve(diag_len + spD) %*% transpose_U_Q_y)
  mu <- X %*% beta
  medf <- edf[index]
  
  # Create an list containing all desired results and let its class be tps
  results <- list( beta = beta , mu = mu , medf = medf , lambda = lambda, gcv = gcv, edf = edf, xk = xk, Z = Z, k = k)
  class(results) <- 'tps'

  return(results)
}


plot.tps <- function(tps,m=50){
  # This function input the result from function fitTPS with class tps and use estimated coefficients to fit spline,
  # After that, visualize the fitted thin plate spline by a perspective plot.
  #
  # input:
  #  - tps: an object of class tps containing all results from fitTPS function
  #  - m: an integer is the length of x1 and x2, the default size of our plot
  #
  # output:
  #  - A perspective plot for the fitted model
  
  # Call fitTPS to get needed values for alpha and delta without recompute them
  tps <- fitTPS(x,y,k=100,lsp=c(-5,5))
  k <- tps$k
  xk <- tps$xk
  Z <- tps$Z
  beta <- tps$beta
  # Set up alpha and delta by formula for fitting model
  alpha <- beta[(k-2):k]
  delta_z <- beta[1: (k-3)]
  delta <- Z %*% delta_z
  
  # Write a function for fitting spline
  fit_func <- function(x){
    # Use vectorization to simplify our function
  s <- vapply(1:k, function(j) delta[j]*eta(sqrt(sum((x - xk[j,])**2))), FUN.VALUE = numeric(1)) # This is the last term of formula should be summed up
  Y <- alpha[1]+ alpha[2]*x[1] + alpha[3]*x[2] + sum(s) 
  return(Y)
  }
  # Use vectorization to call fitted function and get fitted values for plotting
  m <- 50; x2 <- x1 <- seq(0,1,length=m)
  df <- expand.grid(1:m, 1:m)
  va <- mapply(function(i, j) fit_func(c(x1[i],x2[j])), df$Var1, df$Var2)
  M <- matrix(va,50,50)
  # Plot these values with x1 and x2
  persp(x1,x2, M,theta=30,phi=30, main = 'Perspective plot of fitted thin plate spline', zlab = 'fitted values of the thin plate spline') 
  contour(x1,x2, M, main = 'Contour plot of fitted thin plate spline')
}



# The running time of above codes is got by Rprof() which is less than one second, therefore it is efficient.
ff <- function(x) exp(-(x[,1]-.3)^2/.2^2-(x[,2] - .3)^2/.3^2)*.5 + 
  exp(-(x[,1]-.7)^2/.25^2 - (x[,2] - .8 )^2/.3^2)
n <- 500
x <- matrix(runif(n*2),n,2)
y <- ff(x) + rnorm(n)*.1 ## generate example data to fit		      

m <- 50;x2 <- x1 <- seq(0,1,length=m)
xp <- cbind(rep(x1,m),rep(x2,each=m))
contour(x1,x2,matrix(ff(xp),m,m))
persp(x1,x2,matrix(ff(xp),m,m),theta=30,phi=30)

#tp <- getTPS(x)
fit <- fitTPS(x,y,lsp=c(-10,8))
plot.tps(fit)





