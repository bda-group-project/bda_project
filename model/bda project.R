combats <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/combats.csv", header=T)
pokemon <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/pokemon.csv", header=T)
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
tests <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/tests.csv", header=T)

## Check speed vs. winner
get.attr <- function(pokemon_id, attribute) {
  idx <- which(pokemon$id == pokemon_id)
  attr_idx <- 
  return(pokemon$Speed[idx])
}

combats$First_speed <- sapply(combats$First_pokemon, get.speed)
combats$Second_speed <- sapply(combats$Second_pokemon, get.speed)

diffspeed.idx <- which(combats$First_speed != combats$Second_speed)
diffspeed_combats <- combats[diffspeed.idx,]
higherspeed.idx <- apply(diffspeed_combats[,4:5], 1, function(x) which(x==max(x)))
win.idx <- apply(diffspeed_combats[,1:3], 1, function(x) which(x[1:2]==x[3]))
mean(higherspeed.idx==win.idx)

## combine datasets

dim(combats);dim(pokemon)
combats_pok1 <- merge(x=combats, y=pokemon, by.x="First_pokemon", by.y="id", all.x=T)
combats_pok1pok2 <- merge(x=combats_pok1, y=pokemon, by.x="Second_pokemon", by.y="id", all.x=T)
combats_pok1pok2 <- combats_pok1pok2[,-(4:5)] # Delete First_speed, Second_speed
combats_pok1pok2 <- combats_pok1pok2[,-c(4:6, 15:17)] # Delete Name, Type1, Type2 for x, y 
combats_pok1pok2$Legendary.x <- ifelse(combats_pok1pok2$Legendary.x == "True", 1, 0)
combats_pok1pok2$Legendary.y <- ifelse(combats_pok1pok2$Legendary.y == "True", 1, 0)
combats_pok1pok2 <- combats_pok1pok2[,-c(which(colnames(combats_pok1pok2) %in% c('Generation.x', 'Generation.y')))]
dim(combats_pok1pok2)
# Take difference
diff <- combats_pok1pok2[,4:10] - combats_pok1pok2[,11:17]
combats_diff <- cbind(combats_pok1pok2[,1:3], diff)
colnames(combats_diff) <- c("Second_pokemon", "First_pokemon", "Winner", "HP.diff", "Attack.diff", 
                            "Defense.diff", "Sp.Atk.diff", "Sp.Def.diff", "Speed.diff", "Legendary.diff")
combats_diff$y <- ifelse(combats_diff$Winner == combats_diff$First_pokemon, 1, 0)
cor(combats_diff$y, combats_diff$Speed.diff, method='spearman')

##############################################
set.seed(123)
train_idx <- sample(dim(combats_diff)[1], dim(combats_diff)[1]/10)
combats_diff <- combats_diff[train_idx,]
save(train_idx, file="/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/train_idx.Rdata")

##############################################
## probit model
library(msm)
library(MASS)
combats_diff$intercept <- rep(1, dim(combats_diff)[1])

sample.onezi <- function(xi, yi, beta) {
  mu <- crossprod(as.numeric(xi), as.numeric(beta))
  if (yi==1) {
    zi <- rtnorm(1, mu, 1, 0, Inf)
  }
  else {
    zi <- rtnorm(1, mu, 1, -Inf, 0)
  }
  return(zi)
}

sample.beta <- function(x.mat, z.mat) {
  sigma.mat <- solve(t(x.mat) %*% x.mat)
  beta.hat <- sigma.mat %*% t(x.mat) %*% z.mat
  beta <- mvrnorm(n=1, mu=beta.hat, Sigma=sigma.mat)
  return(beta)
}

# Gibbs
gibbs <- function(NN, zi.init, beta.init) {
  mat <- matrix(0, nrow=NN, ncol=length(zi.init)+length(beta.init))
  zi <- zi.init
  beta <- beta.init
  mat[1, ] <- c(beta.init, zi.init)
  for (i in 2:(NN)) {
    z.mat <- as.matrix(apply(combats_diff, 1, function(data) sample.onezi(data[c(12, 4:10)], data[11], beta)))
    beta <- sample.beta(x.mat=as.matrix(combats_diff[,c(12, 4:10)]), z.mat=z.mat)
    mat[i,] <- c(beta, z.mat)
    #print(i)
  }
  return(mat)
}

set.seed(1234)
logistic_reg <- glm(y ~ HP.diff+Attack.diff+Defense.diff+Sp.Atk.diff+Sp.Def.diff+Speed.diff+Legendary.diff, 
                 family=binomial(link = "probit"), data=combats_diff)
mle <- summary(logistic_reg)$coeff
zi.init <- as.matrix(apply(combats_diff, 1, function(data) sample.onezi(data[c(12, 4:10)], data[11], mle[,1])))

t1 <- Sys.time()
result1 <- gibbs(NN=5000, zi.init, mle[,1])
beta_sample1 <- result1[,1:8]
t2 <- Sys.time()
t2 - t1




##############################################
## t-link model

sample.onezi.t <- function(xi, yi, beta, lambda_i) {
  mu <- crossprod(as.numeric(xi), as.numeric(beta))
  if (yi==1) {
    zi <- rtnorm(1, mu, lambda_i^(-0.5), 0, Inf)
  }
  else {
    zi <- rtnorm(1, mu, lambda_i^(-0.5), -Inf, 0)
  }
  return(zi)
}

sample.beta.t <- function(x.mat, z.mat, lambda_vec) {
  w.mat <- diag(lambda_vec[1:length(lambda_vec)])
  sigma.mat <- solve(t(x.mat) %*% w.mat %*% x.mat)
  beta.hat <- sigma.mat %*% t(x.mat) %*% w.mat %*% z.mat
  beta <- mvrnorm(n=1, mu=beta.hat, Sigma=sigma.mat)
  return(beta)
}

sample.onelambda.t <- function(xi, zi, beta, nu) {
  mu <- crossprod(as.numeric(xi), as.numeric(beta))
  lambda_i <- rgamma(1, shape=(nu+1)/2, scale=2/(nu+(zi-mu)^2))
  return(lambda_i)
}

log.cond.nu <- function(nu, lambda_vec, pi_nu) {
  log_c_nu <- -lgamma(nu)+(nu/2)*log(nu/2)
  N <- length(lambda_vec)
  log(pi_nu) + N*log_c_nu + sum((nu/2-1)*log(lambda_vec)-nu*lambda_vec/2)
}

sample.nu.t<-function(nu, lambda_vec, pi_nu=1, sigma=2.4, nu.known=T){
  if (nu.known) nu <- nu
  else {
    nu.star<-rtnorm(1,nu,sigma,0,Inf)
    ratio <- exp(log.cond.nu(nu.star,lambda_vec,pi_nu))/dtnorm(nu.star,nu,sigma,0,Inf)/
      (exp(log.cond.nu(nu,lambda_vec,pi_nu))/dtnorm(nu,nu.star,sigma,0,Inf))
    ratio <- min(1,ratio,na.rm=T)
    u <- runif(1)
    if(u <= ratio) {nu<-nu.star}
  }
  return(nu)
}

# Gibbs
gibbs <- function(NN, zi.init, beta.init, lambda.init, nu.init, pi.nu=1, nu_known = T) {
  mat <- matrix(0, nrow=NN, ncol=length(beta.init)+length(nu.init))
  zi <- zi.init
  beta <- beta.init
  lambda <- lambda.init
  nu <- nu.init
  mat[1, ] <- c(beta.init, nu.init)
  for (i in 2:(NN)) {
    z.mat <- as.matrix(apply(cbind(combats_diff,lambda), 1, function(data) sample.onezi.t(data[c(12, 4:10)], data[11], beta, data[13])))
    beta <- sample.beta.t(x.mat=as.matrix(combats_diff[,c(12, 4:10)]), z.mat=z.mat, lambda_vec=lambda)
    lambda <- as.matrix(apply(cbind(combats_diff,z.mat), 1, function(data) sample.onelambda.t(data[c(12, 4:10)], data[13], beta, nu)))
    nu <- sample.nu.t(nu, lambda, pi_nu, sigma=2.4, nu.known=nu_known)
    mat[i,] <- c(beta, nu)
    #print(i)
  }
  return(mat)
}

lambda.init.t <- rgamma(dim(combats_diff)[1], shape=4, scale=1/4)
zi.init.t <- as.matrix(apply(cbind(combats_diff,lambda.init.t), 1, function(data) sample.onezi.t(data[c(12, 4:10)], data[11], mle[,1], data[13])))

t3 <- Sys.time()
result2 <- gibbs(NN=5000, zi.init.t, mle[,1], lambda.init.t, 8, pi.nu=1, nu_known=T)
beta_sample2 <- result2[,1:8]
t4 <- Sys.time()
t4 - t3

### Correlation
library(ComplexHeatmap)
library(circlize)
col_fun <- colorRamp2(c(-1, 0, 1), c("green", "white", "red"))

Heatmap(cor(combats_pok1pok2), col=col_fun)


















