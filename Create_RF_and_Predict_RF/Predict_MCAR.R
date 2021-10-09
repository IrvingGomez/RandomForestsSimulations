rm(list=ls())

library(dplyr)

# library with many list operators
library(rlist)

# libraries to parallelize
library(foreach)
library(doParallel)
library(parallel)

# cluster
numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

source("Source/random_forests_with_missing_values.R")
RF <- readRDS("RF_MCAR.rds")

# Testing set with 0% missing
test_comp <- readRDS("Datasets/Testing/COMP/test_COMP.rds")
test_XX <- test_comp[,2:6]

test_yy_COMP <- foreach(i = 1:100,
                        .packages = c("rlist", "magrittr",
                                      "foreach", "doParallel",
                                      "parallel"),
                        .combine = rbind,
                        .export=ls(envir=globalenv())) %dopar% {
                          
                          aux <- foreach(j = 1:2000,
                                         .packages = c("rlist", "magrittr",
                                                       "foreach", "doParallel",
                                                       "parallel"),
                                         .combine = c,
                                         .export=ls(envir=globalenv())) %dopar% {
                                           
                                           miss_RF.pred(x = test_XX[j,],
                                                           forest = RF[[i]])
                                           
                                         }
                          aux
                        }

saveRDS(test_yy_COMP, file="test_yy_COMP.rds")

#Testing set with 5% - 95% missing
missingness <- c(5,10,20,40,60,80,90,95)

for(k in missingness){
  ruta <- "Datasets/Testing/MCAR/"
  
  test_df <- readRDS(paste0(ruta, "test_MCAR_X4_", k, "_.rds"))
  test_XX <- test_df[,2:6]
  
  test_yy <- foreach(i = 1:100,
                          .packages = c("rlist", "magrittr",
                                        "foreach", "doParallel",
                                        "parallel"),
                          .combine = rbind,
                          .export=ls(envir=globalenv())) %dopar% {
                            
                            aux <- foreach(j = 1:2000,
                                           .packages = c("rlist", "magrittr",
                                                         "foreach", "doParallel",
                                                         "parallel"),
                                           .combine = c,
                                           .export=ls(envir=globalenv())) %dopar% {
                                             
                                             miss_RF.pred(x = test_XX[j,],
                                                        forest = RF[[i]])
                                             
                                           }
                            aux
                          }
  
  saveRDS(test_yy, file=paste0("test_yy_MCAR_", k, ".rds"))
}



stopCluster(cl)