library(RSpectra)
library(png)
library(animation)
require(ggplot2)


fz <-function(z,a,r,L)
{
  L^L*gamma(L-a)*z^(L-1)/(r^a*gamma(L)*gamma(-a)*(r+L*z)^(L-a))
}
urbanpic <- readPNG("D:/sar/sar-urban.png")
r <- 0.299
g <- 0.587
b <- 0.114
R <- urbanpic[,,1]
G <- urbanpic[,,2]
B <- urbanpic[,,3]
urban <- r*R + g*G + b*B  
a <- hist(urban, breaks = seq(0,1,0.02), freq = F)
c <- matrix(unlist(a))
d <- c[102:151]
f <- rev(d)
inputdata <- data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=f)

ggplot(data=inputdata,aes(x=gray_value,y=as.numeric(gray_frequency)/16)) + 
  geom_bar(stat="identity") +
  stat_function(fun=fz, geom = "line", size=2, col="black", args = list(a=-4, r=1.7, L=60)) +
  stat_function(fun=fz, geom = "line", size=2, col="red", args = list(a=-7, r=1.65, L=4)) +
  stat_function(fun=fz, geom = "line", size=2, col="blue", args = list(a=-5, r=1.55, L=10)) +
  xlab("Grey Value") + ylab("Gray Histogram and G0 Densities")

