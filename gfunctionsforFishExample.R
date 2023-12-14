
####  gls functions for fish example:

g1fun <- function(par,  X){
	par[2]*X^par[1]
}

g2fun <- function(par,  X, theta){
	g1fun(par,  X )^theta
}

derfun <- function(par,   X){
	cbind( par[2]*X^par[1]*log(X), X^par[1] )
}

vhatmufun <- function(par, derfun, X, vcov){
	diag(derfun(par, X)%*%vcov%*%t(derfun(par, X)))
}


