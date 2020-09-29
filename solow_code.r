library(tidyverse)

s <- 0.25
d <- 0.05
n <- 0.02
a <- 0.4

k <- rep(NA,200)
y <- rep(NA,200)

k[1] <- 1
y[1] <- k[1]^a 

for (t in 2:200){
  
  y[t] <- k[t-1]^a 
  k[t] <- s*y[t]+(1-d-n)*k[t-1] 
  
}
kstar <- (s / (n + d))^(1 / (1 - a))

qplot(x=1:200, y=k, geom='line') + 
  labs(x="Time", y="Capital per Worker") +
  geom_hline(yintercept=kstar, linetype="dashed") +
  geom_text(x=1, y=0.95*kstar, label="kstar")

qplot(x=1:200, y=y, geom='line') + 
  labs(x="Time", y="Output per Worker") +
  geom_hline(yintercept=(kstar)^a, linetype="dashed")


####
SS <- (s/(n+d))^(1/(1-a))

ks[1] <- SS
ys[1] <- ks[1]^a

for (t in 2:200){
  
  ys[t] <- ks[t-1]^a 
  ks[t] <- s*ys[t]+(1-d-n)*ks[t-1] 
  
}

qplot(x=1:200, y=ks, geom='line') + 
  labs(x="Time", y="Capital per Worker") +
  geom_hline(yintercept=kstar, linetype="dashed") +
  geom_text(x=1, y=0.95*kstar, label="kstar")

qplot(x=1:200, y=ys, geom='line') + 
  labs(x="Time", y="Output per Worker") +
  geom_hline(yintercept=(kstar)^a, linetype="dashed")


####

k1 <- rep(NA,200)
y1 <- rep(NA,200)
s1 <- rep(0.3,200)

s1[1] <- 0.25
k1[1] <- 1
y1[1] <- k1[1]^a 

for (t in 2:200){
  
  y1[t] <- k1[t-1]^a 
  k1[t] <- s1[t-1]*y1[t]+(1-d-n)*k1[t-1] 
  
}

df.base <- data.frame(t=1:200,k=k,id=rep("base",200))
df.new <- data.frame(t=1:200,k=k1,id=rep("new",200))
df.both <- bind_rows(df.base,df.new)

df.both %>% 
  ggplot(aes(x = t, y = k, colour = id)) +
  geom_line() +
  ylim(0, 12.5)



#####

g <- 0.015

A <- rep(NA,200)

A[1] <- 1

k[1] <- 1
y[1] <- k[1]^a*A[1]^(1-a)

for (t in 2:200){
  
  A[t] <- A[t-1]+A[t-1]*g 
  y[t] <- A[t]*(s/(d+n+g))^(a/(1-a)) 
  k[t] <- s*y[t]+(1-d-n)*k[t-1] 
  
}

qplot(x=1:200, y=k, geom='line') + 
  labs(x="Time", y="Capital per Worker") +
  geom_hline(yintercept=kstar, linetype="dashed") +
  geom_text(x=1, y=0.95*kstar, label="kstar")


