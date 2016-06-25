start.time = Sys.time()

library(MASS)

computeCost = function(X, y, theta) {
	m = length(y)
	J = (t(X %*% theta - y) %*% (X %*% theta - y))/(2*m)
	return(J)
}

gradientDescent = function(X, y, theta, alpha, iterations) {
	m = length(y)
	J_history = rep(0, iterations)


	for (i in 1:iterations) {
		#cat(theta,"\n")
		theta <- theta - alpha*t(t(X %*% theta - y) %*% X)/m
		J_history[i] = computeCost(X, y, theta)
	}

	plot(1:iterations, J_history)
	return(theta)
}

normalEqn = function(X, y) {
	#return(ginv(X) %*% y)
	return( ginv( t(X) %*% X ) %*% t(X) %*% y )
}

A = scan("ex1data2.txt", sep=",")
A = matrix(A, nrow=length(A)/3, ncol=3, byrow=TRUE)
m = length(A[,1])
# plot(A[,1],A[,3], type='p')

X = A[,1:2]
mu = c(mean(X[,1]), mean(X[,2]))
sigma = c(sd(X[,1]), sd(X[,2]))
for(i in 1:2) {
	X[,i] = (X[,i] - mu[i])/sigma[i]
}

X = cbind(rep(1,m),X)
y = A[,3]
theta = as.matrix(rep(0,3))

iterations = 500
alpha = 0.05

computeCost(X, y, theta)

theta = gradientDescent(X, y, theta, alpha, iterations)

cat("\nTheta found by gradient descent: ", theta[1], theta[2], theta[3])
cat("\nPredicted price of a 1650 sq-ft, 3 br house (using gradient descent): ",
	theta[1] + theta[2]*(1650 - mu[1])/sigma[1] + theta[3]*(3 - mu[2])/sigma[2] , "\n")

theta = normalEqn(X, y)

cat("\nTheta found by normal equation: ", theta[1], theta[2], theta[3])
cat("\nPredicted price of a 1650 sq-ft, 3 br house (using normal equation): ",
	theta[1] + theta[2]*(1650 - mu[1])/sigma[1] + theta[3]*(3 - mu[2])/sigma[2] , "\n")

print(Sys.time() - start.time)

# lines(X[,2], X %*% theta, col="blue")