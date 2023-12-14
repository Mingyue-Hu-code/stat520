

##  Functions to implement GLS for beta with fixed theta and pseudo-likeliood for theta;

glsiter  <- function(par, g1fun, g2fun, derfun, theta, Y, X, J){
	iter <- 0
	repeat{
		iter <- iter + 1
		par  <- glsstep(par, g1fun, g2fun, derfun, theta , Y, X)
		if(iter == J){break}
	}
	sig2hat <- sum(((Y - g1fun(par, X) )^2)/(g2fun(par, X, theta)^2))/(length(Y) - length(par))
	Vhat <- vhatgls(par,   g2fun, derfun, theta,  X, sig2hat) 
	list(par, sig2hat, Vhat)
}

glsstep <- function(par, g1fun, g2fun, derfun, theta	, Y, X){
	D <- derfun(par,  X)
	W <- diag(1/g2fun(par,  X, theta)^2)
	par + solve(t(D)%*%W%*%D)%*%t(D)%*%W%*%(Y - g1fun(par,  X))
}


vhatgls <- function(par,  g2fun, derfun, theta,  X, sig2hat){
 	 D <- derfun(par,  X)
	 W <- diag(1/g2fun(par,  X, theta)^2)
	 sig2hat*solve(t(D)%*%W%*%D)
}

#####  Incorporate pseudo-likelihood:

plogthet <- function(theta, g1fun, g2fun, Y, X, par){
	sig2hattheta <- sum( ((Y - g1fun(par,  X) )^2)/g2fun(par, X, theta)^2 )/(length(Y) - length(par))	
	-length(Y)/2*log(sig2hattheta)  - 1/2*sum(log( g2fun(par, X, theta)^2 ) )
}

glspseudoiter <- function(par, g1fun, g2fun, derfun, theta, Y, X, int, J){
	theta  <- optimize(plogthet, interval = int, g1fun, g2fun, Y, X, par, maximum = TRUE)$maximum
	iter <- 0
	repeat{
		iter <- iter + 1
		par  <- glsstep(par, g1fun, g2fun, derfun, theta , Y, X)
		theta  <- optimize(plogthet, interval = int, g1fun, g2fun, Y, X, par, maximum = TRUE)$maximum
		if(iter == J){break}
	}
	sig2hattheta <- sum( ((Y - g1fun(par, X) )^2)/g2fun(par, X, theta)^2 )/(length(Y) - length(par))	
	list(par, sig2hattheta , theta)
}



