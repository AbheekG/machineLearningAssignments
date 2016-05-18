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

	#plot(1:iterations, J_history)
	return(theta)
}

A = scan("ex1data1.txt", sep=",")
A = matrix(A, nrow=length(A)/2, ncol=2, byrow=TRUE)
m = length(A[,1])
plot(A, type='p')

X = cbind(rep(1,m),A[,1])
y = A[,2]
theta = as.matrix(rep(0,2))

iterations = 1500
alpha = 0.01

computeCost(X, y, theta)

theta = gradientDescent(X, y, theta, alpha, iterations)

cat("\nTheta found by gradient descent: ", theta[1], theta[2], "\n")

lines(X[,2], X %*% theta, col="blue")