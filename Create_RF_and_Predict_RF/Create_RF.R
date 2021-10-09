rm(list=ls())

library(dplyr)

# library with many list operators
library(rlist)

# libraries to parallelize
library(foreach)
library(doParallel)
library(parallel)

source("Codes/Source/random_forests_with_missing_values.R")

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

# MCAR
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr",
                             "foreach", "doParallel",
                             "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/MCAR/MCAR_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                          y = dat[,1],
                          ntree = 50,
                          mtry = 1,
                          replace = FALSE,
                          an = 127,
                          nodesize = 5,
                          tn = Inf,
                          X_space = matrix(rep(c(0,1),5), ncol = 5))
                }

saveRDS(RF, file="RF_MCAR.rds")

# MAR1
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/MAR1/MAR1_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_MAR1.rds")

# MAR2
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/MAR2/MAR2_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_MAR2.rds")

# MAR3
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/MAR3/MAR3_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_MAR3.rds")

# MAR4
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/MAR4/MAR4_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_MAR4.rds")

# LOG
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/LOG/LOG_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_LOG.rds")

# DEPY
RF <- foreach(i = 1:100,
              .packages = c("rlist", "magrittr", "foreach", "doParallel", "parallel"),
              .export=ls(envir=globalenv())) %dopar% {
                
                direct <- paste0("Datasets/Training/DEPY/DEPY_X4_60_", i, ".rds")
                dat <- readRDS(direct)
                miss_regRF(X = dat[,2:6],
                           y = dat[,1],
                           ntree = 50,
                           mtry = 1,
                           replace = FALSE,
                           an = 127,
                           nodesize = 5,
                           tn = Inf,
                           X_space = matrix(rep(c(0,1),5), ncol = 5))
              }

saveRDS(RF, file="RF_DEPY.rds")