combats <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/combats.csv", header=T)
pokemon <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/pokemon.csv", header=T, na.strings="")
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
tests <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/data/tests.csv", header=T)

## Check speed vs. winner
get.speed <- function(pokemon_id) {
  idx <- which(pokemon$id == pokemon_id)
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

## Type chart
type_chart <- read.csv("/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/pokemon-chart/chart.csv", header=T, row.names=1)
type_factor <- function(att1, att2, def1, def2) {
  att.v <- c(att1, att2)
  def.v <- c(def1, def2)
  temp_mat <- matrix(0,2,2)
  for (i in 1:2) {
    for (j in 1:2) {
      row.idx <- match(att.v[i], rownames(type_chart))
      col.idx <- match(def.v[j], colnames(type_chart))
      temp_mat[i, j] <- ifelse(is.na(row.idx)|is.na(col.idx), NA, type_chart[row.idx, col.idx])
    }
  }
  att.def.factors <- apply(temp_mat, 1, function(x) ifelse(is.na(x[1])&is.na(x[2]), NA, prod(x, na.rm=T)))
  return(max(att.def.factors, na.rm=T))
}

colidx_1vs2 <- match(c("Type.1.x", "Type.2.x", "Type.1.y", "Type.2.y"), colnames(combats_pok1pok2))
colidx_2vs1 <- match(c("Type.1.y", "Type.2.y", "Type.1.x", "Type.2.x"), colnames(combats_pok1pok2))
combats_pok1pok2$type.factor.x <- apply(combats_pok1pok2[, colidx_1vs2], 1, function(vec) type_factor(vec[1],vec[2],vec[3],vec[4]))
combats_pok1pok2$type.factor.y <- apply(combats_pok1pok2[, colidx_2vs1], 1, function(vec) type_factor(vec[1],vec[2],vec[3],vec[4]))

#
combats_pok1pok2 <- combats_pok1pok2[,-c(4:6, 15:17)] # Delete Name, Type1, Type2 for x, y 
combats_pok1pok2$Legendary.x <- ifelse(combats_pok1pok2$Legendary.x == "True", 1, 0)
combats_pok1pok2$Legendary.y <- ifelse(combats_pok1pok2$Legendary.y == "True", 1, 0)
combats_pok1pok2 <- combats_pok1pok2[,-c(which(colnames(combats_pok1pok2) %in% c('Generation.x', 'Generation.y')))]
dim(combats_pok1pok2)
# Take difference
diff <- combats_pok1pok2[,c(4:10,18)] - combats_pok1pok2[,c(11:17,19)]
combats_diff <- cbind(combats_pok1pok2[,1:3], diff)
colnames(combats_diff) <- c("Second_pokemon", "First_pokemon", "Winner", "HP.diff", "Attack.diff", 
                            "Defense.diff", "Sp.Atk.diff", "Sp.Def.diff", "Speed.diff", "Legendary.diff", "Type.Factor.diff")
combats_diff$y <- ifelse(combats_diff$Winner == combats_diff$First_pokemon, 1, 0)
combats_diff$intercept <- rep(1, dim(combats_diff)[1])
cor(combats_diff$y, combats_diff$Speed.diff, method='spearman')

##############################################
set.seed(123)
train_idx <- sample(dim(combats_diff)[1], dim(combats_diff)[1]/50) #1000 samples for training
test_idx <- sample(setdiff(1:dim(combats_diff)[1], train_idx), dim(combats_diff)[1]/100) #500 samples for testing
combats_diff_train <- combats_diff[train_idx,]
combats_diff_test <- combats_diff[test_idx,]
write.csv(combats_diff_train, "/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/combats_diff_train.csv")
write.csv(combats_diff_train, "/Users/pl/Documents/SMU/2020 Fall/STAT 6390 - Bayesian Methods and Data Analysis/Project/combats_diff_test.csv")

##############################################
## probit model
library(msm)
library(MASS)

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
    z.mat <- as.matrix(apply(combats_diff_train, 1, function(data) sample.onezi(data[c(13, 4:11)], data[12], beta)))
    beta <- sample.beta(x.mat=as.matrix(combats_diff_train[,c(13, 4:11)]), z.mat=z.mat)
    mat[i,] <- c(beta, z.mat)
    #print(i)
  }
  return(mat[,1:9])
}

set.seed(1234)
glm_reg <- glm(y ~ HP.diff+Attack.diff+Defense.diff+Sp.Atk.diff+Sp.Def.diff+Speed.diff+Legendary.diff+Type.Factor.diff, 
                 family=binomial(link = "probit"), data=combats_diff_train)
summary(glm_reg)
mle <- summary(glm_reg)$coeff
zi.init <- as.matrix(apply(combats_diff_train, 1, function(data) sample.onezi(data[c(13, 4:11)], data[12], rep(0,9))))

beta.init.list <- list(mle[,1],
                       c(-1,-1,-1,0,0,0,1,1,1),
                       c(0,0,0,1,1,1,-1,-1,-1),
                       c(1,1,1,-1,-1,-1,0,0,0),
                       rep(0,9),
                       rep(1,9),
                       rep(-1,9))
result1 <- vector("list", length = length(beta.init.list))

t1 <- Sys.time()

for (i in 1:length(beta.init.list)) {
  beta.init <- beta.init.list[[i]]
  zi.init <- as.matrix(apply(combats_diff_train, 1, function(data) sample.onezi(data[c(13, 4:11)], data[12], beta.init)))
  result1[[i]] <- gibbs(NN=5000, zi.init, beta.init)
}

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
    z.mat <- as.matrix(apply(cbind(combats_diff_train,lambda), 1, function(data) sample.onezi.t(data[c(13, 4:11)], data[12], beta, data[14])))
    beta <- sample.beta.t(x.mat=as.matrix(combats_diff_train[,c(13, 4:11)]), z.mat=z.mat, lambda_vec=lambda)
    lambda <- as.matrix(apply(cbind(combats_diff_train,z.mat), 1, function(data) sample.onelambda.t(data[c(13, 4:11)], data[14], beta, nu)))
    nu <- sample.nu.t(nu, lambda, pi_nu, sigma=2.4, nu.known=nu_known)
    mat[i,] <- c(beta, nu)
    #print(i)
  }
  return(mat)
}

nu <- 8


result2 <- vector("list", length = length(beta.init.list))

t3 <- Sys.time()

for (i in 1:length(beta.init.list)) {
  beta.init.t <- beta.init.list[[i]]
  lambda.init.t <- rgamma(dim(combats_diff_train)[1], shape=nu/2, scale=2/nu)
  zi.init.t <- as.matrix(apply(cbind(combats_diff_train,lambda.init.t), 1, function(data) sample.onezi.t(data[c(13, 4:11)], data[12], beta.init.t, data[14])))
  result2[[i]] <- gibbs(NN=5000, zi.init.t, beta.init.t, lambda.init.t, nu, pi.nu=1, nu_known=T)
}

t4 <- Sys.time()
t4 - t3

### Correlation
library(ComplexHeatmap)
library(circlize)
col_fun <- colorRamp2(c(-1, 0, 1), c("green", "white", "red"))

Heatmap(cor(combats_pok1pok2), col=col_fun)


















