# function examples for bayesian approach (5-bin via functions)

# function for bayesian classification
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))

apply(x,2, function(x) exp(-(x[]-mean(x)+sd(x))^2/2*sd(x)))
apply(x,2, function(x) exp(-(x[]-mean(x))^2/2*sd(x)))
apply(x,2, function(x) exp(-(x[]-mean(x)-sd(x))^2/2*sd(x)))

