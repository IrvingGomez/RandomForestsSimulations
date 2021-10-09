rm(list=ls())

setwd("~/Documents/CIMAT/1_Doctorado/PhD_Dissertation/Codigos/Analysis")

library(randomForest)
library(rlist)
library(ggplot2)
library(hrbrthemes)

Imp <- list()

set.seed(111)
for (i in 1:100){
  direct <- paste0("~/Documents/CIMAT/1_Doctorado/PhD_Dissertation/Codigos/Datasets/Fix_miss/comp_", i, ".rds")
  dat    <- readRDS(direct)
  
  RF <- randomForest(x = dat[,2:6], y = dat[,1],
                      ntree = 50, mtry = 1, replace = FALSE,
                      sampsize = 127, nodesize = 5,
                      importance = TRUE)
  
  Imp <- list.append(Imp, RF$importance)
}

IncPure <- IncMSE <- c()

for (i in 1:100){
IncMSE <- cbind(IncMSE, Imp[[i]][,1])
IncPure <- cbind(IncPure, Imp[[i]][,2])
}

# mean of the variable importance measures
MSE_df1 <- data.frame(IncMSE)
Pure_df1 <- data.frame(IncPure)

#colnames(Pure_df1) <- colnames(MSE_df1) <- c(rep("",100))

MSE_df <- data.frame(
  val = c(as.numeric(MSE_df1[1,]),
          as.numeric(MSE_df1[2,]),
          as.numeric(MSE_df1[3,]),
          as.numeric(MSE_df1[4,]),
          as.numeric(MSE_df1[5,])
          ),
  grp = rep(rownames(MSE_df1), each = 100)
)

Pure_df <- data.frame(
  val = c(as.numeric(Pure_df1[1,]),
          as.numeric(Pure_df1[2,]),
          as.numeric(Pure_df1[3,]),
          as.numeric(Pure_df1[4,]),
          as.numeric(Pure_df1[5,])
  ),
  grp = rep(rownames(Pure_df1), each = 100)
)

# We make the violin plots

png("Varb_import_1.png", height = 960, width = 960, res = 150)

ggplot(MSE_df, aes(x=grp, y=val, fill=grp)) +
  ggtitle("Importance of Variables by % of Increase in MSE") +
  geom_violin(width=0.75) +
  geom_boxplot(width=0.1, color="black", alpha=0.7) +
  scale_fill_manual(values = c("deeppink4", "dodgerblue4",
                               "deeppink4", "deeppink4",
                               "dodgerblue4")) +
#  geom_jitter(size=1, color="#9A703EFF", width=0.1) +
  theme_ipsum() +
  xlab("") +
  ylab("Increase in MSE (%)") +
  theme(
    legend.position="none",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 16)
  )

dev.off()

png("Varb_import_2.png", height = 960, width = 960, res = 150)

ggplot(Pure_df, aes(x=grp, y=val, fill=grp)) +
  ggtitle("Importance of Variables by Increase in Node Purity") +
  geom_violin(width=0.75) +
  geom_boxplot(width=0.1, color="black", alpha=0.7) +
  scale_fill_manual(values = c("deeppink4", "dodgerblue4",
                               "deeppink4", "deeppink4",
                               "dodgerblue4")) +
#  geom_jitter(size=1, color="#9A703EFF", width=0.1) +
  theme_ipsum() +
  xlab("") +
  ylab("Increase in Node Purity") +
  theme(
    legend.position="none",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 16)
  )

dev.off()
