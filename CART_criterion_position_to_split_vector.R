# library with many list operators
library(rlist)

X <- iris$Sepal.Length

miss_percent <- 0.2
size_miss <- length(X) * miss_percent
n <- length(X)

# Introduce missing values in the data
set.seed(11)
places_miss <- sample(n, size_miss)
X[places_miss] <- NaN
y <- iris$Petal.Width


inAL <- c()
inAR <- c()
i_notmiss <- which(!is.na(X))
i_miss <- which(is.na(X))

X_sort <- sort(X[i_notmiss])
X_sort <- unique(X_sort)
nn     <- length(X_sort)

# Candidate places to split
zs <- (X_sort[1:(nn-1)]+X_sort[2:nn])/2

assign_obs_left  <- list()
assign_obs_right <- list()

# Order the y for the observations with missing value in X
y_miss <- y[i_miss]
miss_dat <- data.frame(cbind(y_miss, i_miss))
miss_dat <- miss_dat[with(miss_dat, order(y_miss)),]

y_miss <- miss_dat[,1]
n_miss <- length(y_miss)

# Assign all the points to the right
inAL_wmiss <- inAL
inAR_wmiss <- c(inAR, miss_dat[,2])

assign_obs_left  <- list.append(assign_obs_left,  inAL_wmiss)
assign_obs_right <- list.append(assign_obs_right, inAR_wmiss)

yAL <- y[inAL_wmiss]
yAR <- y[inAR_wmiss]

yAL_bar <- mean(yAL)
yAR_bar <- mean(yAR)

nAL <- length(yAL)
nAR <- length(yAR)
nA  <- nAL + nAR

# Formula to calculate the CART criterion
posib <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2

# Let's split the vector of missing y and assign
if (n_miss > 1) {
  for (i in 1:(n_miss-1)){
    
    inAL_wmiss <- c(inAL, miss_dat[1:i,2])
    inAR_wmiss <- c(inAR, miss_dat[(i+1):n_miss,2])
    
    assign_obs_left  <- list.append(assign_obs_left,  inAL_wmiss)
    assign_obs_right <- list.append(assign_obs_right, inAR_wmiss)
    
    yAL <- y[inAL_wmiss]
    yAR <- y[inAR_wmiss]
    
    yAL_bar <- mean(yAL)
    yAR_bar <- mean(yAR)
    
    nAL <- length(yAL)
    nAR <- length(yAR)
    
    # formula to calculate the CART criterion
    cart <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
    posib <- c(posib, cart)
  }
}

# Assign all the points to the left
inAL_wmiss <- c(inAL, miss_dat[,2])
inAR_wmiss <- inAR

assign_obs_left  <- list.append(assign_obs_left,  inAL_wmiss)
assign_obs_right <- list.append(assign_obs_right, inAR_wmiss)

yAL <- y[inAL_wmiss]
yAR <- y[inAR_wmiss]

yAL_bar <- mean(yAL)
yAR_bar <- mean(yAR)

nAL <- length(yAL)
nAR <- length(yAR)
nA  <- nAL + nAR

# Formula to calculate the CART criterion
cart <- (nAL*nAR)/(nA*nA)*(yAL_bar-yAR_bar)^2
posib <- c(posib, cart)

png("CART_criterion_position_to_split_Y_miss.png", height = 720, width = 1280, res = 150)
plot(1:(length(posib)), posib, type='b',
     xlab  = 'Position to split the ordered Y vector',
     ylab  = 'CART criterion',
     pch   = 19,
     #main = 'CART criterion as a function of the position to split'
     )
dev.off()


