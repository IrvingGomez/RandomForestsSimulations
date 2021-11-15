rm(list = ls())

# We read the estimated y_values
test_comp_y_hat <- readRDS("test_yy_COMP.rds")
test_LOG_5_y_hat <- readRDS("test_yy_LOG_5.rds")
test_LOG_10_y_hat <- readRDS("test_yy_LOG_10.rds")
test_LOG_20_y_hat <- readRDS("test_yy_LOG_20.rds")
test_LOG_40_y_hat <- readRDS("test_yy_LOG_40.rds")
test_LOG_60_y_hat <- readRDS("test_yy_LOG_60.rds")
test_LOG_80_y_hat <- readRDS("test_yy_LOG_80.rds")
test_LOG_90_y_hat <- readRDS("test_yy_LOG_90.rds")
test_LOG_95_y_hat <- readRDS("test_yy_LOG_95.rds")

# We read the real y_values
test_comp <- readRDS("COMP/test_COMP.rds")
test_LOG_5 <- readRDS("LOG/test_LOG_X4_5_.rds")
test_LOG_10 <- readRDS("LOG/test_LOG_X4_10_.rds")
test_LOG_20 <- readRDS("LOG/test_LOG_X4_20_.rds")
test_LOG_40 <- readRDS("LOG/test_LOG_X4_40_.rds")
test_LOG_60 <- readRDS("LOG/test_LOG_X4_60_.rds")
test_LOG_80 <- readRDS("LOG/test_LOG_X4_80_.rds")
test_LOG_90 <- readRDS("LOG/test_LOG_X4_90_.rds")
test_LOG_95 <- readRDS("LOG/test_LOG_X4_95_.rds")

test_comp_y <- test_comp[,1]
test_LOG_5_y <- test_LOG_5[,1]
test_LOG_10_y <- test_LOG_10[,1]
test_LOG_20_y <- test_LOG_20[,1]
test_LOG_40_y <- test_LOG_40[,1]
test_LOG_60_y <- test_LOG_60[,1]
test_LOG_80_y <- test_LOG_80[,1]
test_LOG_90_y <- test_LOG_90[,1]
test_LOG_95_y <- test_LOG_95[,1]

Err0 <- Err5 <- Err10 <- Err20 <- Err40 <- Err60 <- Err80 <- Err90 <- Err95 <- matrix(rep(0,200000), ncol = 2000)

for (j in 1:2000){
  Err0[,j] <- test_comp_y_hat[,j]-test_comp_y[j]
  Err5[,j] <- test_LOG_5_y_hat[,j]-test_LOG_5_y[j]
  Err10[,j] <- test_LOG_10_y_hat[,j]-test_LOG_10_y[j]
  Err20[,j] <- test_LOG_20_y_hat[,j]-test_LOG_20_y[j]
  Err40[,j] <- test_LOG_40_y_hat[,j]-test_LOG_40_y[j]
  Err60[,j] <- test_LOG_60_y_hat[,j]-test_LOG_60_y[j]
  Err80[,j] <- test_LOG_80_y_hat[,j]-test_LOG_80_y[j]
  Err90[,j] <- test_LOG_90_y_hat[,j]-test_LOG_90_y[j]
  Err95[,j] <- test_LOG_95_y_hat[,j]-test_LOG_95_y[j]
}

for (i in c(0,5,10,20,40,60,80,90,95)){
  nam <- paste0("Err", i) # name of the df with the errors
  bb <- paste0("bias", i) # name of the vector with the bias
  mm <- paste0("MSE", i) # name of the vector with the MSE
  ee <- get(nam)
  assign(bb, apply(ee, 1, mean))
  assign(mm, apply(ee^2, 1, mean))
}

saveRDS(Err0, file="Error0.rds")
saveRDS(Err5, file="Error5.rds")
saveRDS(Err10, file="Error10.rds")
saveRDS(Err20, file="Error20.rds")
saveRDS(Err40, file="Error40.rds")
saveRDS(Err60, file="Error60.rds")
saveRDS(Err80, file="Error80.rds")
saveRDS(Err90, file="Error90.rds")
saveRDS(Err95, file="Error95.rds")

saveRDS(bias0, file="bias0.rds")
saveRDS(bias5, file="bias5.rds")
saveRDS(bias10, file="bias10.rds")
saveRDS(bias20, file="bias20.rds")
saveRDS(bias40, file="bias40.rds")
saveRDS(bias60, file="bias60.rds")
saveRDS(bias80, file="bias80.rds")
saveRDS(bias90, file="bias90.rds")
saveRDS(bias95, file="bias95.rds")

saveRDS(MSE0, file="MSE0.rds")
saveRDS(MSE5, file="MSE5.rds")
saveRDS(MSE10, file="MSE10.rds")
saveRDS(MSE20, file="MSE20.rds")
saveRDS(MSE40, file="MSE40.rds")
saveRDS(MSE60, file="MSE60.rds")
saveRDS(MSE80, file="MSE80.rds")
saveRDS(MSE90, file="MSE90.rds")
saveRDS(MSE95, file="MSE95.rds")







