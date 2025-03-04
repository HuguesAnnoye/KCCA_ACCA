#Packages 
library(MASS)
library(BEAMM.KCCAACCA)

#Create training set
set.seed(46)
Y11 <- rlnorm(1000,0,1)
X11 <- rnorm(1000,0,1)
X12 <- rnorm(1000,0,1) + Y11
X13 <- 4*X12 +  2 *X11 + rnorm(1000,0,1)
X14 <- rnorm(1000,0,10)
sig <- rbind(c(1,0.5,0.5),c(0.5,1,0.5),c(0.5,0.5,1))
X15 <- mvrnorm(1000,mu=c(0,1,2),Sigma=sig)

X <- cbind(X11,X12,X13,X14,X15)
colnames(X) <- paste0("CV_",1:7)


Y12 <- rnorm(1000,0,1) - log(Y11+1)
Y13 <- 3*X12 + 4 *log(Y11+1) + rnorm(1000,0,1)
Y14 <- 5*X13 + 2*X14 + log(Y11+1)  + rnorm(1000,0,1)
Y15 <- 5*X12 + 2*X11 + Y12 + rnorm(1000,0,1)
Y16 <- 5*X11 + 2*X13 + Y13
Y17 <- 3*X15[,1] + 2*X14 + log(Y11+1) + rnorm(1000,0,1)
Y <- cbind(Y11,Y13,Y14,Y15,Y16)
colnames(Y) <- paste0("NCV_",1:5)

#Create test set
Y21 <- rlnorm(1000,0,1)
X21 <- rnorm(1000,0,1)
X22 <- rnorm(1000,0,1) + Y21
X24 <- rnorm(1000,0,10)
sig <- rbind(c(1,0.5,0.5),c(0.5,1,0.5),c(0.5,0.5,1))
X25 <- mvrnorm(1000,mu=c(0,1,2),Sigma=sig)
X23 <- 4*X22 +  2 *X21 + rnorm(1000,0,1)
X2 <- cbind(X21,X22,X23,X24,X25)
colnames(X2) <- paste0("CV_",1:7)


Y22 <- rnorm(1000,0,1) - log(Y21+1)
Y23 <- 3*X22 + 4 * log(Y21+1) + rnorm(1000,0,1)
Y24 <- 5*X23 + 2*X24 + log(Y21+1)  + rnorm(1000,0,1)
Y25 <- 5*X22 + 2*X21 + Y22 + rnorm(1000,0,1)
Y26 <- 5*X21 + 2*X23 + Y23
Y27 <- 3*X25[,1] + 2*X24 + log(Y21+1) + rnorm(1000,0,1)
Y2 <- cbind(Y21,Y23,Y24,Y25,Y26)
colnames(Y2) <- paste0("NCV_",1:5)

#Charge the parameters tuned before. 
load("~/resultat/25_novembre_2024/ACCA/param.RDATA")

#Perform ACCA
options_ACCA <-statMatch.ACCA.options(
  print.details = TRUE,
  scaling = c("z-score", "min-max", "no"),
  tuning_type = c("random"),
  d = 2L,
  p1_lat_X_min = 3L,
  p1_lat_X_max = 7L,
  p1_lat_X_step = 1L,
  p2_lat_X_min = 4L,
  p2_lat_X_max = 5L,
  p2_lat_X_step = 1L,
  p1_lat_Y_min = 3L,
  p1_lat_Y_max = 3L,
  p1_lat_Y_step = 1L,
  p2_lat_Y_min = 4L,
  p2_lat_Y_max = 5L,
  p2_lat_Y_step = 1L,
  p1_h = seq(0.001, 0.5, by = 0.001),
  p2_h = seq(0.001, 0.5, by = 0.001),
  p1_nlayers = 2L,
  p2_nlayers = 2L,
  p1_epochs = 200L,
  p2_epochs = 200L,
  p1_batch_size = 32L,
  p2_batch_size = 32L,
  p1_u_min = 5L,
  p1_u_max = 50L,
  p1_u_step = 5L,
  p2_u_min = 5L,
  p2_u_max = 50L,
  p2_u_step = 10L,
  p1_lr_min = -4,
  p1_lr_max = -2,
  p2_lr_min = -4,
  p2_lr_max = -2,
  p1_penL1_min = -6,
  p1_penL1_max = -2,
  p2_penL1_min = -6,
  p2_penL1_max = -2,
  p1_penL2_min = -6,
  p1_penL2_max = -2,
  p2_penL2_min = -6,
  p2_penL2_max = -2,
  p1_n_combs = 1L,
  p2_n_combs = 200L,
  n_fold = 5L,
  seed_phase1 = NULL,
  seed_phase2 = NULL,
  p1_man_params = param_1,
  p2_man_params = param_2,
)


df.rec <- data.frame(X2)
df.don <- data.frame(cbind(X,Y))
t1 <- Sys.time()
res1 <- statMatch(
  df.rec=df.rec,
  df.don=df.don,
  names.CV = colnames(X),
  names.NCV = colnames(Y),
  zero.constraints = NULL,
  don.weights = NULL,
  opts = options_ACCA,
  method = "ACCA"
)
t2 <- Sys.time()
temps <- t2 - t1
print(temps)

#Save Results
save(list=ls(), file="~/resultat/25_novembre_2024/ACCA/Essais_ACCA_lognorm2.RDATA")
