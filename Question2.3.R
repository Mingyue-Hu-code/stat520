#Part2: Question 3b

#partial derivative of log likelihood with respect to beta
d.beta <- function(alpha, beta, y) {
  d <- digamma(alpha + beta) - digamma(alpha + 1 + beta + y) +digamma(beta + y) - digamma(beta) + 
    return(d)
}

d.alpha <- function (alpha, beta, y) {
  d <- digamma(alpha + beta) + digamma(alpha + 1) - digamma(alpha + 1 + beta + y) - digamma(alpha) + 
    return(d)
}

d.beta.2 <- function(alpha, beta, y){
  d <- trigamma(beta+y) - trigamma(alpha + 1 + beta + y) - trigamma(beta) + trigamma(alpha + beta)
  return(d)
}


d.alpha.2 <- function(alpha, beta, y) {
  d <- trigamma(alpha +1) - trigamma(alpha + 1 + beta + y) - trigamma(alpha) + trigamma(alpha + beta)
  return(d)
}


d.alpha.beta <- function(alpha, beta, y) {
  d <-  trigamma(alpha + beta) - trigamma(alpha + 1 + beta +y)
  return(d)
  
}

#partial derivative of log likelihood with respect to alpha




#observed information matrix function
obs.info <- function(alpha, beta, y) {
  mat.info <- matrix(nrow=2, ncol=2, 0)
  mat.info[1,1] <- mean(d.alpha(alpha, beta, y)^2)
  mat.info[1,2] <- mean(d.alpha(alpha, beta, y)*d.beta(alpha,beta, y))
  mat.info[2,1] <- mean(d.alpha(alpha, beta, y)*d.beta(alpha,beta, y))
  mat.info[2,2] <- mean(d.beta(alpha, beta, y)^2)
  return(mat.info)
  
}

obs.info.2 <- function(alpha, beta, y) {
  mat.info <- matrix(nrow=2, ncol=2, 0)
  mat.info[1,1] <- sum(d.alpha.2(alpha, beta, y))
  mat.info[1,2] <- sum(d.alpha.beta(alpha,beta,y))
  mat.info[2,1] <- sum(d.alpha.beta(alpha,beta,y))
  mat.info[2,2] <- sum(d.beta.2(alpha, beta, y))
  return(-1*mat.info)
  
}
