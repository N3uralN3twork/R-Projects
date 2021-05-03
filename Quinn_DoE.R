library(agricolae)
mu<- c(18.750,25.063,28.024)/3
n.sim<- 3
sigma2<- 2.5^2
g<-length(mu)
r=g
c=g
r
c

trt<-LETTERS[1:g]
outdesign <-design.lsd(trt,serie=2,seed=27)
lsd <- outdesign$book
lsd$trtn=as.numeric(lsd$trt)
results <- numeric(n.sim)
aux=0
for(i in 1:n.sim){for(i in 1:g^2){
    lsd$y[i] <- rnorm(1, mean = mu[lsd$trtn[i]], sd = sqrt(sigma2))
  }
}
data <- data.frame(lsd)
data

fit  <- aov(y ~ trt+row+col,data = data)
fit

aux1 = ifelse(summary(fit)[[1]][1, "Pr(>F)"] < 0.05,1,0)
aux=aux+aux1
aux

power=aux/n.sim
power







