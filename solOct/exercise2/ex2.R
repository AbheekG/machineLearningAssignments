h = function(theta, X) {
	return(1/(1+exp(-X %*% theta)) )
}

computeCost = function(theta, X, y) {
	m = length(y)
	theta = as.matrix(theta)
	J = (1/(2*m)) * sum(ifelse(y == 1, -log(h(theta,X)), -log(1-h(theta,X))))
	return(J)
}


A = scan("ex2data1.txt", sep=",")
A = matrix(A, nrow=length(A)/3, ncol=3, byrow=TRUE)
m = length(A[,1])
n = 3

X = A[,1:2]
X = cbind(rep(1,m),X)
y = A[,3]

theta = rep(0,3)

result = optim(theta, computeCost, gr=NULL, X, y)
print(result)
theta = result$par

plot(X[,2], X[,3], xlab="Exam 1 score", ylab="Exam 2 score", col=ifelse(y == 1,'red','green'))

cat("\nTheta found by gradient descent: ", theta[1], theta[2], theta[3], "\n\n")

abline(-theta[1]/theta[3], -theta[2]/theta[3])