mapFeature = function(X1, X2) {
	degree = 6
	out = as.matrix(rep(1,length(X1)))

	for(i in 1:degree) {
		for(j in 0:i) {
			out = cbind(out, (X1^j) * (X2^(i-j)))
		}
	}
	return(out)
}

h = function(theta, X) {
	return(1/(1+exp(-X %*% theta)) )
}

computeCost = function(theta, X, y, lamda) {
	m = length(y)
	n = length(theta)
	theta = as.matrix(theta)
	J = (1/(2*m)) * ( sum(ifelse(y == 1, -log(h(theta,X)), -log(1-h(theta,X)))) + lamda*sum(theta[2:n,1]^2) )
	return(J)
}

gradient = function(theta, X, y, lamda) {
	lamda = rep(lamda, length(theta))
	lamda[1]=0
	return( t(X) %*% (h(X) - y) + lamda*theta )
}

predict = function(theta, X) {
	return(ifelse(X %*% as.matrix(theta)>0,1,0))
}

plotDecisionBoundary = function(theta, x1, x2, y1, y2) {
	n = 100
	X = seq(x1,x2, length.out=n)
	Y = seq(y1,y2, length.out=n)
	Z = matrix(rep(0,2*n*n), ncol = 2, nrow=n*n)
	for(i in 1:n) {
		for(j in 1:n) {
			Z[(i-1)*n+j,1] = X[i]
			Z[(i-1)*n+j,2] = Y[j]
		}
	}
	Z1 = predict(theta, mapFeature(Z[,1],Z[,2]))
	#plot(Z[,1], Z[,2], xlab="Exam 1 score", ylab="Exam 2 score", col=ifelse(Z1 == 1,'red','green'))
	Z = matrix(predict(theta, mapFeature(Z[,1], Z[,2])), nrow=n, ncol=n)
	contour(X,Y,Z, xlim=c(x1,x2), ylim=c(y1,y2))
}

A = scan("ex2data2.txt", sep=",")
A = matrix(A, nrow=length(A)/3, ncol=3, byrow=TRUE)
m = length(A[,1])

X = A[,1:2]
y = A[,3]

x1 = min(X[,1])
x2 = max(X[,1])
y1 = min(X[,2])
y2 = max(X[,2])
plot(X[,1], X[,2], xlab="X1", ylab="X2", col=ifelse(y == 1,'red','green'), xlim=c(x1,x2), ylim=c(y1,y2))
par(new=TRUE)

X = mapFeature(X[,1], X[,2])

initial_theta = rep(0, length(X[1,]))
lamda = 1
iterations = 23000

result = optim(initial_theta, computeCost, gradient, X, y, lamda ,control = list(maxit=iterations))
print(result)
theta = result$par

cat("\nTheta found by gradient descent: \n")
print(theta)

p = predict(theta, X)

plotDecisionBoundary(theta, x1, x2, y1, y2)