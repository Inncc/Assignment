require(ggplot2)

#gamma distribution
 ggplot(data=data.frame(z=seq(0, 7, length.out = 500)), aes(x=z)) + 
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1,scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3,scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8,scale=1/8))


# k distribution
dKI <- function(z, p_alpha, p_lambda, p_Looks, log=FALSE) {
  
  if(log==FALSE) {
    
    lLz <- p_lambda * p_Looks* z
    
    return((2*p_lambda*p_Looks/(gamma(p_alpha)*gamma(p_Looks))) *
             (lLz)^((p_alpha+p_Looks)/2-1) *
             besselK(x = 2*sqrt(lLz), nu = p_alpha-p_Looks)
    )
  }
  
}

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=2, p_lambda= 2, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=2, p_lambda=2, p_Looks=3)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=2, p_lambda=2, p_Looks=8))

# g0 distribution
fz <-function(z,a,r,L)
{
  L^L*gamma(L-a)*z^(L-1)/(r^a*gamma(L)*gamma(-a)*(r+L*z)^(L-a))
}
ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) + 
  stat_function(fun=fz, geom = "line", size=2, col="red", args = list(a=-5, r=4, L=1)) +
  stat_function(fun=fz, geom = "line", size=2, col="blue", args = list(a=-5, r=4, L=3)) +
  stat_function(fun=fz, geom = "line", size=2, col="black", args = list(a=-5, r=4, L=8))

