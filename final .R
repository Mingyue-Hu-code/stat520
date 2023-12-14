mydat<-read.csv("Kids1.csv")
plot(mydat$Weight,mydat$Height)
plot(log(mydat$Age),log(mydat$Height))
y<-mydat$Height
heightC<-cut(y,breaks = quantile(y,prob=c(0,seq(0.1,1,by=0.1))),include.lowest=TRUE) 
GroupMean<-tapply(y,heightC,mean) 
GroupSD<-tapply(y, heightC, sd) 
lm<-lm(Height~log(Weight)+log(Age),data=mydat) 
summary(lm) 

plot(GroupMean,GroupSD) 
abline(a=coef(lm)[1],b=coef(lm)[2]) 

plot(lm(formula = Height ~ log(Weight) + log(Age), data = mydat))

plot(sqrt(mydat$Weight), sqrt(mydat$Height))


# Read the data
data <- read.csv("emails.csv")

#initial
ac<-seq(from=0, to=1,by=0.01)
bc<-seq(from=0, to=1,by=0.01)
for (j in 1:101) {
  

for (i in 1:101){
loglike <- 
  n*log(gamma(ac[i]+1))+sum(log(gamma(B+bc[j])))-sum(log(gamma(ac[i]+bc[j]+B+1)))
+n*log(gamma(ac[i]+bc[j]))-n*log(gamma(ac[i]))-n*log(gamma(bc[j]))
}
  
}


# Define Newton-type algorithm to find the MLE of (alpha, beta0, beta1)
findMLE <- function(B)
{
  n <- length(A)
  
  # Log likelihood function
  loglike <- function(v)
  {
    a <- v[1]
    b <- v[2]
    
    loglike <- 
      n*log(gamma(a+1))+sum(log(gamma(B+b)))-sum(log(gamma(a+b+B+1)))+n*log(gamma(a+b))-n*log(gamma(a))-n*log(gamma(b))
    loglike
  }
  
  # Choosing start of (alpha, beta0, beta1) based on linear model
  ac<-seq(from=0, to=1,by=0.01)
  bc<-seq(from=0, to=1,by=0.01)
  
  
  # Find the estimate
  gamma <- nlm(loglike, c(alpha_s, beta_s0, beta_s1))
  round(gamma$estimate, 2)
}

# Print the MLE of (alpha, beta0, beta1)
est <- findMLE(species$Area, species$Species)
alpha <- est[1]
alpha
beta0 <- est[2]
beta0
beta1 <- est[3]
beta1

## Use Bootstrap to find the 95% interval for beta1

# Generate beta_i from MLE of beta0 and beta1
beta_i <- alpha*exp(-beta0)*species$Area^(-beta1)

# Define total steps of Bootstrap
n <- 1000

# Generate theta* from beta_i
theta_i <- vector()
for(i in 1:length(beta_i)){
  theta_i <- cbind(theta_i, rgamma(n, alpha, rate = beta_i[i]))
}

# Generate Y* from theta*
Yi <- matrix(0, 1000, 14)
for(i in 1:ncol(theta_i)){
  for(j in 1:nrow(theta_i)){
    Yi[j,i] <- rpois(1, theta_i[j,i])
  }
}

# Generate bootstrap data for beta1
bootstrap <- vector()
for(i in 1:nrow(Yi)){
  bootstrap[i] <- try(findMLE(species$Area, Yi[i,])[3], TRUE)
}
bootstrap <- as.numeric(bootstrap)

# 95% interval for beta1
c(sort(bootstrap)[n*.025], sort(bootstrap)[n*.975])