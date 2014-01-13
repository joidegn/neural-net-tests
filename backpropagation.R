# My implementation of the backpropagation algorithm as given on page a of lecture notes
#   I think his definition of the derivatives do not correspond to the error function (by a factor of 1/T) which should not matter practically

#uses sigmoid activation functions for each layer in perceptron with one hidden layer (because I am too lazy for the general case)

sigmoid <- function(k) 1/(1+exp(-k))
sigmoid.deriv <- function(k) sigmoid(k)*(1-sigmoid(k))
output <- function(x, w1, w2) sigmoid(w2*sigmoid(x%*%w1))
output.deriv.w1 <- function(x, w1, w2) w2*t(x)%*%sigmoid.deriv(x%*%w1)%*%t(sigmoid.deriv(w2*sigmoid(x%*%w1)))
output.deriv.w2 <- function(x, w1, w2) t(sigmoid(x%*%w1)*sigmoid.deriv(w2*sigmoid(x%*%w1)))
squared.error <- function(y, yd) 1/nrow(y) * sum((y-yd)^2)
squared.error.deriv.y <- function(y, yd) 2/nrow(y) * (y-yd)
squared.error.deriv.w1 <- function(x, w1, w2, yd) t(t(squared.error.deriv.y(output(x, w1, w2), yd)) %*% t(output.deriv.w1(x, w1, w2)))
squared.error.deriv.w2 <- function(x, w1, w2, yd) t(squared.error.deriv.y(output(x, w1, w2), yd)) %*% t(output.deriv.w2(x, w1, w2))

backpropagation <- function(x, y, initial.w1=1:4, initial.w2=5, delta=0.1, threshold=1e-9) {   # run backprop until error changes less than threshold
    current.error <- squared.error(output(x, w1, w2), y)

    cat("initial error is: ", current.error, "\n")

    i <- 0
    while (current.error > threshold) {
        i <- i+1
        #browser()
        del.w2 <- as.numeric(-delta * squared.error.deriv.w2(x, w1, w2, y))  # gradient descent
        del.w1 <- as.numeric(-delta * squared.error.deriv.w1(x, w1, w2, y))  # gradient descent
        w1 <- w1 + del.w1
        w2 <- w2 + del.w2
        current.error <- squared.error(output(x, w1, w2), y)
        cat("iteration: ", i, "\tw1: ", w1, "\tw2: ", w2, "\ncurrent error: ", current.error, "\n")
    }

}


# some sample data with 4 cols:
x <- matrix(rnorm(10000), ncol=4)
# true model:  # take sin as a nonlinear function
betas <- 1:4
y <- sin(x%*%betas)
