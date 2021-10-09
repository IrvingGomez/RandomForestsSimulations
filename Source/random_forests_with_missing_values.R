library(dplyr)

# library with many list operators
library(rlist)

# libraries to parallelize
library(foreach)
library(doParallel)
library(parallel)


# The criterion CART with my modification to deal with missing data,
# for an specific place and variable
miss_CART <- function(X, y, h, z){
  
  inAL <- c()  # observed values in AL
  inAR <- c()  # observed values in AR
  ih_notmiss <- which(!is.na(X[,h]))
  ih_miss <- which(is.na(X[,h]))
  
  for (i in ih_notmiss){
    cond <- X[i,h] <= z
    if(cond){
      inAL <- c(inAL, i)
    }
    else{
      inAR <- c(inAR, i)
    }
  }
  
  yAL <- y[inAL]  # responses in AL
  yAR <- y[inAR]  # responses in AR
  
  yAL_bar <- mean(yAL)
  yAR_bar <- mean(yAR)
  
  if(length(ih_miss) == 0){
    
    nAL <- length(yAL)
    nAR <- length(yAR)
    nA  <- nAL + nAR
    
    max_cart <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
    assign_L <- inAL
    assign_R <- inAR
    
    out <- list(max_cart, assign_L, assign_R)
    names(out) <- c('cart', 'obs_L', 'obs_R')
  }
  else{
    y_miss <- y[ih_miss]
    miss_dat <- cbind(y_miss, ih_miss) %>% as.data.frame
    
    if (yAL_bar < yAR_bar){
      miss_dat <- miss_dat[with(miss_dat, order(y_miss)),]
    }else{
      miss_dat <- miss_dat[with(miss_dat, order(-y_miss)),]
    }
    
    y_miss <- miss_dat[,1]
    nih <- length(y_miss)
    
    # everything to the right
    inAL_wmiss <- inAL
    inAR_wmiss <- c(inAR, miss_dat[,2])
    
    yAL <- y[inAL_wmiss]
    yAR <- y[inAR_wmiss]
    
    yAL_bar <- mean(yAL)
    yAR_bar <- mean(yAR)
    
    nAL <- length(yAL)
    nAR <- length(yAR)
    nA  <- nAL + nAR

    max_cart <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
    assign_L <- inAL_wmiss
    assign_R <- inAR_wmiss
    
    # assigning missing observations
    if (nih > 1) {
      for (i in 1:(nih-1)){
        
        inAL_wmiss <- c(inAL, miss_dat[1:i,2])
        inAR_wmiss <- c(inAR, miss_dat[(i+1):nih,2])
        
        yAL <- y[inAL_wmiss]
        yAR <- y[inAR_wmiss]
        
        yAL_bar <- mean(yAL)
        yAR_bar <- mean(yAR)
        
        nAL <- length(yAL)
        nAR <- length(yAR)
        
        posib <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
        
        if (posib >= max_cart){
          max_cart <- posib
          assign_L <- inAL_wmiss
          assign_R <- inAR_wmiss
        }
      }
    }
    
    # everything to the left
    inAL_wmiss <- c(inAL, miss_dat[,2])
    inAR_wmiss <- inAR
    
    yAL <- y[inAL_wmiss]
    yAR <- y[inAR_wmiss]
    
    yAL_bar <- mean(yAL)
    yAR_bar <- mean(yAR)
    
    nAL <- length(yAL)
    nAR <- length(yAR)
    nA  <- nAL + nAR
    
    posib <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
    
    if (posib >= max_cart){
      max_cart <- posib
      assign_L <- inAL_wmiss
      assign_R <- inAR_wmiss
    }
    
    out <- list(max_cart, assign_L, assign_R)
    names(out) <- c('cart', 'obs_L', 'obs_R')
  }
  
  return(out)
}

miss_CART_l <- Vectorize(miss_CART, vectorize.args = 'z')

# Validate data
valid_data <- function(X, y, random = TRUE,
                       mtry = if(random) max(floor(ncol(X)/3),1) else ncol(X),
                       X_space = NULL){
  
  if (dim(X)[1] != length(y)){
    stop("X and y have incompatible lengths.")
  }
  
  if (class(X)!="data.frame"){X <- data.frame(X)}
  
  X.row.names <- rownames(X)
  X.col.names <- colnames(X)
  p           <- ncol(X)      # dimension of the predictor space
  
  if(!is.null(X_space)){
    if (class(X_space)!="data.frame"){
      X_space <- data.frame(X_space)
      if (p!=ncol(X_space)){
        X_space <- NULL
        warning("Number of columns in X and X-space does not coincide,
            X_space will be built with the data.")
      }else{
        if (colnames(X_space)!=X.col.names){
          colnames(X_space) <- X.col.names
          warning("Column names of X-space and column names of X does not match,
                column names of X are kept.")
        }
      }
    }
  }
  
  # which variables are completely missing
  # we have no info about this variables, so they will be removed
  incomp <- apply(is.na(X),2,sum)
  h_miss <- which(incomp==nrow(X))
  
  if(length(h_miss)>0){
    X <- X[,-h_miss]
    warning("There were completely missing Variables in a tree,
            they were removed.")
    
    if (class(X)!="data.frame"){
      X           <- data.frame(X)
      colnames(X) <- X.col.names[-h_miss]
      rownames(X) <- X.row.names
      X.col.names <- X.col.names[-h_miss]
    }
    
    if(!is.null(X_space)){
      X_space <- X_space[,-h_miss]
      if (class(X_space)!="data.frame"){
        X_space           <- data.frame(X_space)
        colnames(X_space) <- X.col.names
      }
    }
    
    p <- ncol(X) # dimension of the predictor space with observed variables
  }
  
  # which observations are incomplete
  incomp         <- apply(is.na(X),1,sum)
  i_miss         <- which(incomp==p)          # Completely missing observations
  i_notcomp_miss <- setdiff(1:nrow(X),i_miss) # Not completely missing observations
  i_incomp       <- which(incomp!=0)          # Partially missing observations
  
  if(length(i_incomp)>0){
    if(length(i_miss)>0){
      warning("There were completely missing Observations in a tree,
              they were kept.")
    }else{
      warning("There were missing Observations in a tree.")
    }
  }
  
  # Eliminating observations out of the X_space if X_space is given,
  # or constructing the X_space
  if(!is.null(X_space)){
    in_X_space <- c()
    for (i in i_notcomp_miss){
      hi_notmiss <- which(!is.na(X[i,]))
      in_X_space <- c(in_X_space,
                      (X_space[1,hi_notmiss]<=X[i,hi_notmiss] &&
                         X[i,hi_notmiss]<=X_space[2,hi_notmiss]))
    }
    
    if(sum(in_X_space)!=length(i_notcomp_miss)){
      # upload the number of points in X_space
      # you should not be here unless there is something
      # wrong with the points and the X-space
      
      X <- X[in_X_space,]
      y <- y[in_X_space]
      
      if (class(X)!="data.frame"){
        X           <- data.frame(X)
        colnames(X) <- X.col.names
        rownames(X) <- X.row.names[in_X_space]
        X.row.names <- rownames(X)
      }
      
      warning("There were some points out of the X-space in a tree,
              they were removed.")
    }
  }else{
    minima <- c()
    maxima <- c()
    for (j in 1:p){
      # which observations are observed in the jth variable
      ij_notmiss <- which(!is.na(X[,j]))
      minima <- c(minima, X[ij_notmiss,j] %>% min)
      maxima <- c(maxima, X[ij_notmiss,j] %>% max)
    }
    X_space <- c(minima, maxima) %>% matrix(nrow=2, byrow=TRUE)
    colnames(X_space) <- X.col.names
  }
  
  if(mtry < 1 || mtry > p){
    warning("Invalid mtry in a tree: it was reset to a valid range.")
    mtry <- max(1, min(p, floor(mtry)))
  }
  
  out <- list(X, y, p, X.col.names, mtry, X_space)
  names(out) <- c('X', 'y', 'p', 'X.col.names', 'mtry', 'X_space')
  return(out)
}

# A random regression tree
miss_regTree_RF <- function(X, y, random = TRUE,
                            mtry = if(random) max(floor(ncol(X)/3),1) else ncol(X),
                            nodesize = 5, tn = Inf, X_space = NULL){
  
  # Validate the data
  valid <- valid_data(X, y, random, mtry, X_space)
  
  X <- valid$X
  y <- valid$y
  p <- valid$p
  X.col.names <- valid$X.col.names
  mtry <- valid$mtry
  X_space <- valid$X_space
  
  # Initialize variables
  h_vec    <- c()     # variables split
  z_vec    <- c()     # places split
  dev_vec  <- c()     # MSE of the nodes
  cart_vec <- c()     # CART criterion if the node was split
  L1_vec   <- c()     # L1 part of the CART criterion
  L2_vec   <- c()     # L2 part of the CART criterion
  L3_vec   <- c()     # L3 part of the CART criterion
  L4_vec   <- c()     # L4 part of the CART criterion
  pred     <- c()     # predicted value at nodes
  N        <- c()     # number of points belonging or assigned to each node
  probs    <- c(1)    # probability of being on left/right for missing values
  nodeID   <- c(1)
  contnode <- 1
  
  # Current partitions
  P  <- list(X_space)
  XP <- list(X)
  yP <- list(y)
  
  # Final partitions
  # 1) The predictor space
  # 2) The observations
  # 3) The responses
  
  Pfinal <- list()
  Xfinal <- list()
  yfinal <- list()
  
  # current number of terminal nodes
  tn_curr <- 1
  
  while(!length(P)==0){
    A     <- P[[1]]
    XA    <- XP[[1]]
    yA    <- yP[[1]]
    ncur  <- nodeID[contnode] # current node
    
    X.row.names <- rownames(XA)
    nA  <- nrow(XA)
    
    # upload some the story-teller variables
    N       <- c(N, nA)
    pred    <- c(pred, mean(yA))
    #X_whole <- list.append(X_whole, XA)
    dev     <- (yA-mean(yA))^2 %>% mean
    dev_vec <- c(dev_vec, dev)
    
    # if there are less than nodesize points in A or
    # all y_i in A are equal or
    # all X_i in A are equal or
    # we have more or equal terminal nodes than tn
    if(nA <= nodesize || length(unique(yA)) == 1 || tn_curr >= tn ||
       all(apply(XA, 2,
                 function(x) length(unique(x[!is.na(x)])) == 1) == TRUE)){
      
      P    <- list.remove(P,1)
      XP   <- list.remove(XP,1)
      yP   <- list.remove(yP,1)
      
      Pfinal    <- list.append(Pfinal, A)
      Xfinal    <- list.append(Xfinal, XA)
      yfinal    <- list.append(yfinal, yA)
      
      h_vec    <- c(h_vec, NaN)
      z_vec    <- c(z_vec, NaN)
      cart_vec <- c(cart_vec, NaN)
      
      L1_vec <- c(L1_vec, NaN)
      L2_vec <- c(L2_vec, NaN)
      L3_vec <- c(L3_vec, NaN)
      L4_vec <- c(L4_vec, NaN)
      
      contnode <- contnode+1
      tn_curr <- tn_curr+1
    }
    else{
      
      best_cart <- 0
      best_z    <- 0
      best_h    <- 0
      assi_L    <- NaN
      assi_R    <- NaN
      
      if (random){
        # features are candidates only if they have at least two different observed values
        cand <- c()
        
        for (j in 1:p){
          ij_notmiss <- which(!is.na(XA[,j]))
          XA_j       <- unique(XA[ij_notmiss,j])
          nn         <- length(XA_j)
          if(nn > 1){
            cand <- c(cand,j)
          }
        }
        
        if (length(cand) == 1){
          h_select <- cand
        }else{
          if (mtry > length(cand)){
            mtry <- length(cand)
          }
          h_select <- sample(cand, size = mtry, replace = FALSE)
        }
        
        for (j in h_select){
          # we only take observations with the jth variable observed
          ij_notmiss <- which(!is.na(XA[,j]))
          XA_sort    <- sort(XA[ij_notmiss,j])
          XA_sort    <- unique(XA_sort)
          nn         <- length(XA_sort)
          
          # candidates places to split
          zs  <- (XA_sort[1:(nn-1)]+XA_sort[2:nn])/2
          
          # CART in the candidates for the jth variable
          aux <- miss_CART_l(XA, yA, j, zs)
          cart  <- aux[1,] %>% unlist %>% max
          
          if (cart > best_cart){
            place <- aux[1,] %>% unlist %>% which.max
            
            best_cart <- cart
            best_z <- zs[place]
            best_h <- X.col.names[j]
            best_j <- j
            assi_L <- aux[2,place] %>% unlist
            assi_R <- aux[3,place] %>% unlist
          }
        }
      }else{
        for (j in 1:p){
          # we only take observations with the jth variable observed
          ij_notmiss <- which(!is.na(XA[,j]))
          XA_sort    <- sort(XA[ij_notmiss,j])
          XA_sort    <- unique(XA_sort)
          nn         <- length(XA_sort)
          # if all (observed in the jth varibale) points are the same,
          # there is nothing to split
          if(nn > 1){
            # candidates places to split
            zs  <- (XA_sort[1:(nn-1)]+XA_sort[2:nn])/2
            
            # CART in the candidates for the jth variable
            aux <- miss_CART_l(XA, yA, j, zs)
            cart  <- aux[1,] %>% unlist %>% max
            
            if (cart > best_cart){
              place <- aux[1,] %>% unlist %>% which.max
              
              best_cart <- cart
              best_z <- zs[place]
              best_h <- X.col.names[j]
              best_j <- j
              assi_L <- aux[2,place] %>% unlist
              assi_R <- aux[3,place] %>% unlist
            }
          }
        }
      }
      
      if(best_cart == 0){
        P    <- list.remove(P,1)
        XP   <- list.remove(XP,1)
        yP   <- list.remove(yP,1)
        
        Pfinal    <- list.append(Pfinal, A)
        Xfinal    <- list.append(Xfinal, XA)
        yfinal    <- list.append(yfinal, yA)
        
        h_vec    <- c(h_vec, NA)
        z_vec    <- c(z_vec, NA)
        cart_vec <- c(cart_vec, NA)
        
        L1_vec <- c(L1_vec, NA)
        L2_vec <- c(L2_vec, NA)
        L3_vec <- c(L3_vec, NA)
        L4_vec <- c(L4_vec, NA)
        
        contnode <- contnode+1
        tn_curr <- tn_curr+1
      }
      else{
        h_vec    <- c(h_vec, best_h)
        z_vec    <- c(z_vec, best_z)
        cart_vec <- c(cart_vec, best_cart)
        
        nodeID   <- c(nodeID, 2*ncur, 2*ncur+1)
        contnode <- contnode+1
        
        AL <- A -> AR
        AL[2,best_j] <- best_z
        AR[1,best_j] <- best_z
        
        P <- list.append(P, AL)
        P <- list.append(P, AR)
        P <- list.remove(P, 1)
        
        XAL <- as.data.frame(XA[assi_L,])
        colnames(XAL) <-  X.col.names
        rownames(XAL) <-  X.row.names[assi_L]
        
        XAR <- as.data.frame(XA[assi_R,])
        colnames(XAR) <-  X.col.names
        rownames(XAR) <-  X.row.names[assi_R]
        
        XP <- list.append(XP, XAL)
        XP <- list.append(XP, XAR)
        XP <- list.remove(XP,1)
        
        yP <- list.append(yP, yA[assi_L])
        yP <- list.append(yP, yA[assi_R])
        yP <- list.remove(yP,1)
        
        # Calculate the probabilities of assign a missing to the left or to the right
        miss_le <- sum(is.na(XAL[,best_j]))
        miss_ri <- sum(is.na(XAR[,best_j]))
        miss_to <- miss_le+miss_ri
        if(miss_to != 0){
          probs <- c(probs, miss_le/miss_to, miss_ri/miss_to) 
        }else{
          probs <- c(probs, 0, 0)
        }
        
        # Calculate the L1, L2, L3 and L4 parts of the main criterion
        obss_le <- sum(!is.na(XAL[,best_j]))
        obss_ri <- sum(!is.na(XAR[,best_j]))
        obss_to <- obss_le+obss_ri
        
        yA_le <- yA[assi_L]
        yA_ri <- yA[assi_R]
        
        yA_miss_le <- yA_le[is.na(XAL[,best_j])]
        yA_miss_ri <- yA_ri[is.na(XAR[,best_j])]
        yA_miss    <- c(yA_miss_le, yA_miss_ri)
        
        yA_obss_le <- yA_le[!is.na(XAL[,best_j])]
        yA_obss_ri <- yA_ri[!is.na(XAR[,best_j])]
        yA_obss    <- c(yA_obss_le, yA_obss_ri)
        
        L1 <- (obss_le*obss_ri)/(obss_to^2)*(obss_to/(obss_to+miss_to))*
          (mean(yA_obss_le)-mean(yA_obss_ri))^2
        
        L2 <- (miss_le*miss_ri)/(miss_to^2)*(miss_to/(obss_to+miss_to))*
          (mean(yA_miss_le)-mean(yA_miss_ri))^2
        
        L3 <- obss_to/(obss_to+miss_to)*(mean(yA_obss)-mean(yA))^2-
          obss_le/(obss_to+miss_to)*(mean(yA_obss_le)-mean(yA_le))^2-
          obss_ri/(obss_to+miss_to)*(mean(yA_obss_ri)-mean(yA_ri))^2
        
        L4 <- miss_to/(obss_to+miss_to)*(mean(yA_miss)-mean(yA))^2-
          miss_le/(obss_to+miss_to)*(mean(yA_miss_le)-mean(yA_le))^2-
          miss_ri/(obss_to+miss_to)*(mean(yA_miss_ri)-mean(yA_ri))^2
        
        L1_vec <- c(L1_vec, L1)
        L2_vec <- c(L2_vec, L2)
        L3_vec <- c(L3_vec, L3)
        L4_vec <- c(L4_vec, L4)
        
        tn_curr <- tn_curr+1
      }
    }
  }
  
  terminal <- is.na(cart_vec)
  y_pred   <- pred[is.na(cart_vec)]
  z_vec    <- round(z_vec,3)
  dev_vec  <- round(dev_vec,3)
  cart_vec <- round(cart_vec,3)
  
  L1_vec <- round(L1_vec,3)
  L2_vec <- round(L2_vec,3)
  L3_vec <- round(L3_vec,3)
  L4_vec <- round(L4_vec,3)
  
  Ssplits <- cbind(nodeID, h_vec, z_vec, N, dev_vec, round(pred,3),
                   cart_vec, L1_vec, L2_vec, L3_vec, L4_vec, terminal, probs)
  Ssplits <- as.data.frame(Ssplits)
  colnames(Ssplits) <- c('node', 'Split_h', 'Split_z', 'size',
                         'MSE', 'ypred',
                         'CART', 'L1', 'L2', 'L3', 'L4', 'leaf', 'Prob_miss')
  
  out <- list(Pfinal, X_space, Xfinal, yfinal, y_pred, Ssplits)
  names(out) <- c('Partition', 'X_space', 'X.Partition',
                  'y.Partition', 'y.Predict', 'Structure')
  return(out)
}

# Regresison random forest using the modified CART criterion
miss_regRF <- function(X, y, ntree = 100,
                       mtry = max(floor(ncol(X)/3),1),
                       replace = FALSE,
                       an = if(replace) nrow(X) else ceiling(0.632*nrow(X)),
                       nodesize = 5, tn = Inf, X_space = NULL){
  
  # Validate the data
  valid <- valid_data(X, y, random=TRUE, mtry, X_space)
  
  X           <- valid$X
  y           <- valid$y
  p           <- valid$p
  X.col.names <- valid$X.col.names
  mtry        <- valid$mtry
  X_space     <- valid$X_space
  n           <- nrow(X)
  
  B <- foreach(j = 1:ntree, .combine = cbind) %dopar% {
    sel <- sample(1:n, size = an, replace)
    sel <- sort(sel)
    ob  <- !((1:n) %in% sel) # out of bag observations
    as.numeric(ob)
  }
  
  if(ntree == 1){B <- matrix(B, ncol =1)}
  
  trees <- foreach(j = 1:ntree,
                   .packages = c('dplyr', 'rlist', 'foreach'),
                   .export = ls(envir=globalenv())) %dopar% {
                     sel <- which(B[,j] == 0)
                     miss_regTree_RF(X[sel,], y[sel], random=TRUE,
                                     mtry, nodesize, tn, X_space)}
  
  oob.err <- c()
  
  param <- list(ntree, mtry, replace, an, nodesize, tn)
  names(param) <- c('ntree', 'mtry', 'replace', 'an', 'nodesize', 'tn')
  
  out <- list(trees, B, X_space, oob.err, X, y, param)
  names(out) <- c('trees', 'OOBmatrix', 'X_space', 'OOB.err',
                  'X.keep', 'y.keep', 'RF.param')
  return(out)
}

# Prediction in a tree, stochastically
miss_tree.pred <- function(x, tree){
  
  X_space <- tree$X_space
  pX      <- length(x)
  ptree   <- dim(X_space)[2]
  
  if(pX!=ptree){
    stop("The dimension of x and tree are different.")
  }
  
  if(anyNA(x)){
    h_notmiss <- which(!is.na(x))
  }else{
    h_notmiss <- 1:pX
  }
  
  for(i in h_notmiss){
    if(X_space[1,i]>x[i] || X_space[2,i]<x[i]){
      stop("x is out of the space cover by tree.")
    }
  }
  
  features  <- colnames(X_space)
  Structure <- tree$Structure
  nodes     <- as.numeric(Structure$node)
  leaves    <- as.logical(Structure$leaf)
  ypred     <- as.numeric(Structure$ypred)
  Split_h   <- Structure$Split_h
  Split_z   <- as.numeric(Structure$Split_z)
  
  # If we want to predict a complete observation
  if(!anyNA(x)){
    j <- 1
    i <- which(nodes==j)
    h <- which(features == Split_h[i])
    
    terminal <- (leaves[i]==TRUE)
    
    while(!terminal){
      z <- Split_z[i]
      if (x[h]<z){
        j <- 2*j
        i <- which(nodes==j)
        h <- which(features == Split_h[i])
        terminal <- (leaves[i]==TRUE)
      }else{
        j <- 2*j+1
        i <- which(nodes==j)
        h <- which(features == Split_h[i])
        terminal <- (leaves[i]==TRUE)
      }
    }
  }
  else{
    # drop as far as it will go
    probs     <- as.numeric(Structure$Prob_miss)
    
    j <- 1
    i <- which(nodes==j)
    h <- which(features == Split_h[i])
    
    terminal <- (leaves[i]==TRUE)
    
    while(!terminal){
      if(h %in% h_notmiss){
        z <- Split_z[i]
        if (x[h]<z){
          j = 2*j
          i <- which(nodes==j)
          h <- which(features == Split_h[i])
          terminal <- (leaves[i]==TRUE)
        }else{
          j = 2*j+1
          i <- which(nodes==j)
          h <- which(features == Split_h[i])
          terminal <- (leaves[i]==TRUE)
        }
      }else{
        prob_le <- probs[which(nodes==2*j)]
        prob_ri <- probs[which(nodes==2*j+1)]
        prob_tot <- prob_le+prob_ri
        if(prob_tot == 0){
          terminal <- TRUE
        }
        else{
          sel <- rbinom(1, 1, prob_ri)
          if(sel == 1){
            j <- 2*j+1
            i <- which(nodes==j)
            h <- which(features == Split_h[i])
            terminal <- (leaves[i]==TRUE)
          }else{
            j <- 2*j
            i <- which(nodes==j)
            h <- which(features == Split_h[i])
            terminal <- (leaves[i]==TRUE)
          }
        }
      }
    }
  }
  
  out <- list(ypred[i], j)
  names(out) <- c('y_pred', 'node')
  return(out)
}

# Prediction in a forest, stochastically
miss_RF.pred <- function(x, forest){
  trees <- forest$trees
  pred <- foreach(k = 1:length(trees),
                  .combine = c,
                  .export=ls(envir=globalenv())) %dopar% {
                    miss_tree.pred(x, trees[[k]])$y_pred
                  }
  
  out <- mean(pred)
  return(out)
}

# OOB error
miss_oob.err <- function(forest){
  
  OOB   <- forest$OOBmatrix
  trees <- forest$trees
  X     <- forest$X.keep
  y     <- forest$y.keep
  
  n     <- dim(X)[1]
  
  y.pred <- foreach (k = 1:length(trees),
                     .combine = cbind,
                     .export = ls(envir = globalenv())) %dopar% {
                       
                       oob    <- which(OOB[,k]==1)
                       aux    <- rep(NA,n)
                       
                       for(i in oob){
                         aux[i] <- miss_tree.pred(X[i,], trees[[k]])$y_pred
                       }
                       
                       aux }
  
  oob.err <- foreach (k = 1:length(trees),
                      .combine = c) %dopar% {
                        err <- c()
                        for (i in 1:n){
                          i_in_tree <- which(OOB[i,1:k] == 1)
                          if(length(i_in_tree) != 0) {
                            err <- c(err, (mean(y.pred[i, i_in_tree]) - y[i])^2)
                          }
                        }
                        mean(err)
                      }
  
  return(oob.err)
}

# Add k trees to the forest
increase <- function(forest, k = 1){
  
  OOB      <- forest$OOBmatrix
  trees    <- forest$trees
  X        <- forest$X.keep
  y        <- forest$y.keep
  X_space  <- forest$X_space
  P        <- list(X_space)
  
  param    <- forest$RF.param
  
  mtry     <- param$mtry
  nodesize <- param$nodesize
  tn       <- param$tn
  an       <- param$an
  replace  <- param$replace
  
  n <- dim(X)[1]
  ntree <- dim(OOB)[2]
  
  if (k == 1){
    sel <- sample(1:n, size = an, replace)
    sel <- sort(sel)
    ob  <- !((1:n) %in% sel) # out of bag observations
    
    OOB <- cbind(OOB, as.vector(ob))
    
    new.trees <- miss_regTree_RF(as.data.frame(X[sel,]),
                               y[sel], random=TRUE,
                               mtry, nodesize, tn, P)
  }
  else{
    new.trees <- list()
    
    for (i in 1:k){
      sel <- sample(1:n, size = an, replace)
      sel <- sort(sel)
      ob  <- !((1:n) %in% sel) # out of bag observations
      
      OOB <- cbind(OOB, as.vector(ob))
    }
    
    new.trees <- foreach (i = 1:k,
                          .export = ls(envir = globalenv())) %dopar% {
                            
                            sel <- which(OOB[,ntree+i] == 0)
                            tree <- miss_regTree_RF(as.data.frame(X[sel,]),
                                                  y[sel], random=TRUE,
                                                  mtry, nodesize, tn, P)
                            tree}
  }
  
  out <- list(new.trees, OOB)
  names(out) <- c('new.trees', 'new.OOB')
  
  return(out)
}

# Remove trees with the rem indexes from the forest
decrease <- function(forest, rem = 1){
  
  # rem: which trees to remove from the forest
  OOB   <- forest$OOBmatrix
  trees <- forest$trees
  
  if (unique(dim(OOB)[2] < rem) != FALSE){
    stop('Not possible to remove the trees')}
  
  new.trees <- list.remove(trees, range = rem)
  new.OOB   <- OOB[,-rem]
  
  out <- list(new.trees, new.OOB)
  names(out) <- c('new.trees', 'new.OOB')
  
  return(out)
}

# function to calculate the connectivity in a tree
miss_tree.conect <- function(x = NULL, y = NULL, as.numeric=TRUE, tree){
  # as.numeric = TRUE: rownames must be treated as numeric
  
  # Get the observations in the tree
  Xp     <- tree$X.Partition
  X_tree <- do.call(rbind, Xp)
  if (as.numeric){
    obs <- sort(as.numeric(rownames(X_tree)))
  }else{
    obs <- sort(rownames(X_tree))
  }
  n      <- length(obs)
  
  if(is.null(x)){
    conect <- matrix(rep(0,n*n), nrow=n)
    
    X_tree <- Xp[[1]]
    n_node <- dim(X_tree)[1]
    
    if (as.numeric){
      r <- sort(as.numeric(rownames(X_tree)))
    }else{
      r <- sort(rownames(X_tree))
    }
    
    if(n_node != 1){
      for(i in 1:(n_node-1)){
        for(j in (i+1):n_node){
          k <- which(obs == r[i])
          l <- which(obs == r[j])
          conect[k,l] <- 1
          conect[l,k] <- conect[k,l]
        }
      }
    }
    
    if(length(Xp)>1){
      for(w in 2:length(Xp)){
        X_tree <- Xp[[w]]
        n_node <- dim(X_tree)[1]
        r      <- rownames(X_tree)
        if(n_node != 1){
          for(i in 1:(n_node-1)){
            for(j in (i+1):n_node){
              k <- which(obs == r[i])
              l <- which(obs == r[j])
              conect[k,l] <- 1
              conect[l,k] <- conect[k,l]
            }
          }
        }
      }
    }
    conect <- conect+diag(n)
    out <- conect
  }
  else{
    Struct <- tree$Structure
    leaves_node <- Struct$node[Struct$leaf == TRUE]
    conect <- rep(0, n)
    final_node <- miss_tree.pred(x, tree)$node
    
    if(final_node %in% leaves_node){
      leaf <- which(leaves_node == final_node)
      Part <- tree$X.Partition[[leaf]]
      
      if (as.numeric){
        rX_node <- sort(as.numeric(rownames(Part)))
      }else{
        sort(rownames(Part))
      }
      conect[which(obs %in% rX_node)] <- 1
      
      out <- conect
    }else{
      stop("Observation did not ended in a final node")
    }
  }
  return(out)
}

# This function has been parallelized
miss_RF.proximity <- function(x = NULL, forest){
  
  # obs:
  # 'all',   calculate the proximity with all the points.
  # 'oob',   use only the OOB observations.
  # 'inbag'  use only points used in the construction of trees
  
  forest = miss_iris_random_forest
  
  OOB   <- forest$OOBmatrix
  trees <- forest$trees
  
  if (is.null(x)){
    out <- foreach (k = 1:length(trees),
                    .export = ls(envir = globalenv())) %dopar% {
                      
                      Proxim <- matrix(rep(0, dim(OOB)[1]*dim(OOB)[1]),
                                       nrow = dim(OOB)[1])
                      
                      Conect <- miss_tree.conect(tree = trees[[k]])
                      
                      sel <- which(OOB[,k]==0)
                      for (i in 1:length(sel)){
                        for (j in 1:length(sel)){
                          
                          #  How many trees are the ith and jth obs. inbag
                          n_inbag <- ((OOB[sel[i],]==0) * (OOB[sel[j],]==0)) %>% sum
                          Proxim[sel[i], sel[j]] <- Conect[i,j]/n_inbag
                        }
                      }
                      Proxim
                    }
    out <- Reduce('+', out)
  }
  return(out)
}