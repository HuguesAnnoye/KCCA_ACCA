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


Y12 <- rnorm(1000,0,1) - log(Y11)
Y13 <- 3*X12 + 4 *log(Y11) + rnorm(1000,0,1)
Y14 <- 5*X13 + 2*X14 + log(Y11)  + rnorm(1000,0,1)
Y15 <- 5*X12 + 2*X11 + Y12 + rnorm(1000,0,1)
Y16 <- 5*X11 + 2*X13 + Y13
Y17 <- 3*X15[,1] + 2*X14 + log(Y11) + rnorm(1000,0,1)
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

Y22 <- rnorm(1000,0,1) - log(Y21)
Y23 <- 3*X22 + 4 * log(Y21) + rnorm(1000,0,1)
Y24 <- 5*X23 + 2*X24 + log(Y21)  + rnorm(1000,0,1)
Y25 <- 5*X22 + 2*X21 + Y22 + rnorm(1000,0,1)
Y26 <- 5*X21 + 2*X23 + Y23
Y27 <- 3*X25[,1] + 2*X24 + log(Y21) + rnorm(1000,0,1)
Y2 <- cbind(Y21,Y23,Y24,Y25,Y26)
colnames(Y2) <- paste0("NCV_",1:5)

#Perform KCCA
options_KCCA <-statMatch.KCCA.options(
  print.details = TRUE,
  print.details_tuning = FALSE,
  scaling = c("z-score", "min-max", "no"),
  tuning_type = c("two h", "one h", "random"),
  type_predict = c("loop", "matrix"),
  exactMatch = FALSE,
  names.CV_Cat_prio = NULL,
  n_fold = 5L,
  par = TRUE,
  par_fold = FALSE,
  nc = 5L,
  d = c(2L,3L,4L),
  rot = FALSE,
  p1_kernel_predict = c("alea", "epan", "gauss"),
  p1_kernel_predict_tuning = NULL,
  p2_kernel_predict = c("gauss", "epan", "alea"),
  p1_objmethod = c("wRMSE", "wsRMSE", "wMCR", "wCE"),
  p2_objmethod = c("wsRMSE", "wRMSE", "wCE"),
  p1_h = NULL,
  p1_hmax = 1,
  p1_hmin = 0.01,
  p1_hstep = 0.05,
  p1_hx = NULL,
  p1_hxmax = 0.001,
  p1_hxmin = 0.0001,
  p1_hxstep = 0.002,
  p1_hy = NULL,
  p1_hymax = NULL,
  p1_hymin = NULL,
  p1_hystep = NULL,
  p1_g = 2e-05,
  p1_gmax = 1e-04,
  p1_gmin = 1e-05,
  p1_gstep = 1e-05,
  p2_h = NULL,
  p2_hmax = 0.5,
  p2_hmin = 0.001,
  p2_hstep = 0.002,
  p2_hx = NULL,
  p2_hxmax = 0.0017,
  p2_hxmin = 0.0005,
  p2_hxstep = 0.0002,
  p2_hy = NULL,
  p2_hymax = 0.0010,
  p2_hymin = 0.0003,
  p2_hystep = 0.0002,
  p2_g = 2e-05,
  p2_gmax = 1e-04,
  p2_gmin = 1e-05,
  p2_gstep = 1e-05,
  p1_n_combs = 10L,
  p2_n_combs = 10L,
  p1_man_params = NULL,
  p2_man_params = NULL
)

df.rec <- data.frame(X2)
df.don <- data.frame(cbind(X,Y))
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
t1 <- Sys.time()
res1 <- statMatch(
  df.rec=df.rec,
  df.don=df.don,
  names.CV = colnames(X),
  names.NCV = colnames(Y),
  zero.constraints = NULL,
  don.weights = NULL,
  opts = options_KCCA,
  method = c("KCCA", "superOM", "ACCA", "CCA", "PCA", "MLP")
)
t2 <- Sys.time()
temps <- t2 - t1
print(temps)
#Save Results
save(list=ls(), file="~/resultat/25_novembre_2024/KCCA/Essais_KCCA_lognorm1.RDATA")
