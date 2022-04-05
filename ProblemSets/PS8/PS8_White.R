set.seed(33)

col1<-rep(1,100000)
col2<-rnorm(100000)
col3<-rnorm(100000)
col4<-rnorm(100000)
col5<-rnorm(100000)
col6<-rnorm(100000)
col7<-rnorm(100000)
col8<-rnorm(100000)
col9<-rnorm(100000)
col10<-rnorm(100000)

X<-cbind(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10)
eps<-rnorm(100000, sd=.5)

beta<-c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
y<-X%*%beta + eps

betaOLS <- solve(t(X)%*%X)%*%t(X)%*%y

alpha<-.0000003
iter<-500

objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# -----------Gradient Descent------------
x.All<-vector("numeric", iter)

beta0<-runif(dim(X)[2])

for(i in 1:iter){
  beta0 <- beta0 - alpha*gradient(beta0, y, X)
  x.All[i] <- beta0
  print(beta0)
}

# -----------NLOPTR------------
library(nloptr)

beta0<-runif(dim(X)[2])
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)

# -----------Nelder-Mead-----------
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

res <- nloptr( x0=beta0,eval_f=objfun,opts=options,y=y,X=X)
print(res)

# -----------NLOPTR MLE------------
gradient <- function ( theta ,Y , X ) {
  grad<- as.vector ( rep (0 , length ( theta ) ) )
  beta<- theta[1:( length ( theta ) -1) ]
  sig<- theta[ length ( theta ) ]
  grad[1:( length ( theta ) -1) ] <- -t ( X ) % * % ( Y - X % * % beta ) / ( sig ^2)
  grad[ length ( theta ) ]<- dim ( X ) [1] /sig - crossprod (Y - X % * % beta ) / ( sig^3)
  return ( grad )
}
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
theta0 <- runif(dim(X)[2])
theta0 <- append(as.vector(summary(lm(y~X))$coefficients[,1]),runif(1))
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
print(result)

# -----------OLS-----------
modelsummary::modelsummary(lm(y~X-1),output = "latex")
