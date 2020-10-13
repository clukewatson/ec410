rm(list = ls())
dev.off(dev.list()["RStudioGD"])

set.seed(3456)
N<- 10000

library(MASS)
library(tidyverse)

# Target parameters for univariate normal distributions
rho <- -0.6
mu0 <- 3; s1 <- 1
mu1 <- 4; s2 <- 2
# Parameters for bivariate normal distribution
mu <- c(mu0,mu1) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("bvn1_w0","bvn1_w1")

# Truth
mean(bvn1[,2]) - mean(bvn1[,1])

# Observed Data
beta.1 <- 1
v.i <- rnorm(N,1,1)
E.i <- as.numeric((beta.1*(bvn1[,2]-bvn1[,1])-v.i )>0)
w.observe <- E.i*bvn1[,2] + (1-E.i)*bvn1[,1]
mean(w.observe[E.i==1]) - mean(w.observe[E.i==0])

## Randomize Education Assignment
Random.E <- round(runif(N),0)
mean(Random.E)
w.randomize <- Random.E*bvn1[,2] + (1-Random.E)*bvn1[,1]
mean(w.randomize[Random.E==1]) - mean(w.randomize[Random.E==0])

df.1 <- data.frame(wage.obs =w.observe, edu.choice = E.i)

ggplot(df.1, aes(x=wage.obs, y=edu.choice)) +
  ylim(-0.2, 1.2) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  geom_vline(xintercept=mean(w.observe[E.i==1]), linetype="dashed") +
  geom_vline(xintercept=mean(w.observe[E.i==0]), linetype="dashed")


df.2.a <- data.frame(wage.true =bvn1[,2] , edu.choice = 1)
df.2.b <- data.frame(wage.true =bvn1[,1] , edu.choice = 0)
df.2 <- bind_rows(df.2.a,df.2.b)

ggplot(df.2, aes(x=wage.true, y=edu.choice)) +
  ylim(-0.1, 1.1) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  geom_vline(xintercept=mean(bvn1[,2]), linetype="dashed", colour="red") +
  geom_vline(xintercept=mean(bvn1[,1]), linetype="dotted", colour="red") +
  geom_vline(xintercept=mean(w.observe[E.i==1]), linetype="dashed", colour="blue") +
  geom_vline(xintercept=mean(w.observe[E.i==0]), linetype="dotted", colour="blue")


df.3 <- bind_rows(data.frame(wage =bvn1[,2] , edu.1 = "true"),
                  data.frame(wage =w.observe[E.i==1] , edu.1 = "observed"))
mu <- df.3 %>% group_by(edu.1) %>% summarise(grp.mean=mean(wage))
ggplot(df.3, aes(x=wage, color=edu.1)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=edu.1),
             linetype="dashed")

df.4 <- bind_rows(data.frame(wage =bvn1[,1] , edu.1 = "true"),
                  data.frame(wage =w.observe[E.i==0] , edu.1 = "observed"))
mu <- df.4 %>% group_by(edu.1) %>% summarise(grp.mean=mean(wage))
ggplot(df.4, aes(x=wage, color=edu.1)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=edu.1),
             linetype="dashed")

df.5 <- bind_rows(data.frame(wage =w.observe[E.i==1] , edu.1 = "Graduate"),
                  data.frame(wage =w.observe[E.i==0] , edu.1 = "Dropout"))
mu <- df.5 %>% group_by(edu.1) %>% summarise(grp.mean=mean(wage))
ggplot(df.5, aes(x=wage, color=edu.1)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=edu.1),
             linetype="dashed")

df.6 <- bind_rows(data.frame(wage =bvn1[,2] , edu.1 = "Graduate"),
                  data.frame(wage =bvn1[,1] , edu.1 = "Dropout"))
mu <- df.6 %>% group_by(edu.1) %>% summarise(grp.mean=mean(wage))
ggplot(df.6, aes(x=wage, color=edu.1)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=edu.1),
             linetype="dashed")


