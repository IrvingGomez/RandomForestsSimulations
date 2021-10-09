rm(list = ls())

setwd("/home/dmitri/Documents/Research/Codes/Random_Forests_Simulations/Datasets/Training/")

# library the sigmoid function
library(sigmoid)

friedman1 <- function(x){
  10*sin(pi*x[1]*x[2])+20*(x[3]-0.5)^2+10*x[4]+5*x[5]
}

#####################################
##                                 ##
##  generate the training samples  ##
##                                 ##
#####################################

ntrain <- 200
set.seed(111)

for (i in 1:100){
  
  multiu <- replicate(5, runif(ntrain, 0, 1))
  multiy <- apply(multiu, 1, friedman1)
  
  x1 <- multiu[,1]
  x2 <- multiu[,2]
  x3 <- multiu[,3]
  x4 <- multiu[,4]
  x5 <- multiu[,5]
  
  ##########
  ##      ##
  ## MCAR ##
  ##      ##
  ##########
  
  dat <- multiu
  
  x1_miss <- sample(1:ntrain, 0.2*ntrain)
  x3_miss <- sample(1:ntrain, 0.1*ntrain)
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA


  datmiss <- dat
  x4_miss <- sample(1:ntrain, 0.6*ntrain)
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("MCAR/MCAR_X4_60_", i, ".rds"))

  ##########
  ##      ##
  ## MAR1 ##
  ##      ##
  ##########
  
  dat <- multiu
  
  tot <- ntrain*(ntrain+1)/2
  
  x1_miss <- sample(1:ntrain, 0.2*ntrain, prob = rank(x2)/tot)
  x3_miss <- sample(1:ntrain, 0.1*ntrain, prob = rank(x5)/tot)
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- sample(1:ntrain, 0.6*ntrain, prob = rank(x5)/tot)
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("MAR1/MAR1_X4_60_", i, ".rds"))

  ##########
  ##      ##
  ## MAR2 ##
  ##      ##
  ##########
  
  dat <- multiu
  
  med2 <- median(x2)
  med5 <- median(x5)
  
  prob2 <- rep(1, ntrain)
  prob5 <- prob2
  
  prob2[x2 >= med2] <- 0.9/sum(x2 >= med2)
  prob2[x2 < med2] <- 0.1/sum(x2 < med2)
  
  prob5[x5 >= med5] <- 0.9/sum(x5 >= med5)
  prob5[x5 < med5] <- 0.1/sum(x5 < med5)
  
  x1_miss <- sample(1:ntrain, 0.2*ntrain, prob = prob2)
  x3_miss <- sample(1:ntrain, 0.1*ntrain, prob = prob5)
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- sample(1:ntrain, 0.6*ntrain, prob = prob5)
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("MAR2/MAR2_X4_60_", i, ".rds"))

  ##########
  ##      ##
  ## MAR3 ##
  ##      ##
  ##########
  
  dat <- multiu
  
  x1_miss <- rev(order(x2))[1:(0.2*ntrain)]
  x3_miss <- rev(order(x5))[1:(0.1*ntrain)]
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- rev(order(x5))[1:(0.6*ntrain)]
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("MAR3/test_MAR3_X4_60_", i, ".rds"))

  ##########
  ##      ##
  ## MAR4 ##
  ##      ##
  ##########
  
  dat <- multiu
  
  x1_miss <- c(rev(order(x2))[1:(0.1*ntrain)], order(x2)[1:(0.1*ntrain)])
  x3_miss <- c(rev(order(x5))[1:(0.05*ntrain)], order(x5)[1:(0.05*ntrain)])
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- c(rev(order(x5))[1:(0.6/2*ntrain)], order(x5)[1:(0.6/2*ntrain)])
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("MAR4/test_MAR4_X4_60_", i, ".rds"))

  #########
  ##     ##
  ## LOG ##
  ##     ##
  #########
  
  dat <- multiu
  
  prob1 <- sigmoid(-0.5+x2+x3+x4+x5)
  prob3 <- sigmoid(-0.5+x1+x2+x4+x5)
  prob4 <- sigmoid(-0.5+x1+x2+x3+x5)
  
  x1_miss <- sample(1:ntrain, 0.2*ntrain, prob = prob1)
  x3_miss <- sample(1:ntrain, 0.1*ntrain, prob = prob3)
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- sample(1:ntrain, 0.6*ntrain, prob = prob4)
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("LOG/test_LOG_X4_60", i, ".rds"))

  ##########
  ##      ##
  ## DEPY ##
  ##      ##
  ##########
  
  dat <- multiu
  
  pp <- rep(0.4, ntrain)
  pp[multiy >= 13] <- 0.1
  
  x1_miss <- sample(1:ntrain, 0.2*ntrain, prob = pp)
  x3_miss <- sample(1:ntrain, 0.1*ntrain, prob = pp)
  
  dat[x1_miss, 1] <- NA
  dat[x3_miss, 3] <- NA

  datmiss <- dat
  x4_miss <- sample(1:ntrain, 0.6*ntrain, prob = pp)
  datmiss[x4_miss, 4] <- NA
  
  d <- data.frame(cbind(multiy, datmiss))
  colnames(d) <- c('Y', 'X1', 'X2', 'X3', 'X4', 'X5')
  saveRDS(d, paste0("DEPY/test_DEPY_X4_60_", i, ".rds"))

}