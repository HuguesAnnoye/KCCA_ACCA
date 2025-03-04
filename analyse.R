#Packages 
library(tidyverse)
library(dplyr)
library(xtable)
library(BEAMM.KCCAACCA)

# Load Result
# source("try_article_ACCA_Beta.R)
load("~/resultat/25_novembre_2024/ACCA/Essais_ACCA_Beta.RDATA")

#Analyse function
compute_results <- function(match, actual, names.NCV, names.ZC = NULL, wt = NULL) {
  if (is.null(wt)) wt <- rep(1,nrow(actual))
  names.NCV.cat <- names.NCV[unlist(lapply(actual %>% dplyr::select(all_of(names.NCV)), is.factor))]
  names.NCV.cont <- setdiff(names.NCV, names.NCV.cat)
  results <- NULL
  for (name in names.NCV) {
    if (is.factor(match[,name])) {
      errors <- match[,name] != actual[,name]
      tmp <- c(name, "wMCR", BEAMM.KCCAACCA:::wtd_mean_rcpp(errors, wt))
    } else {
      var_ACTUAL <- BEAMM.KCCAACCA:::wtd_var_rcpp(actual[,name], wt)
      if (var_ACTUAL == 0) var_ACTUAL <- 1
      errors <- match[,name] - actual[,name]
      tmp <- c(name, "wsMAE", BEAMM.KCCAACCA:::wtd_mean_rcpp(abs(errors), wt)/sqrt(var_ACTUAL))
      tmp <- rbind(tmp, c(name, "RwsMSE", sqrt(BEAMM.KCCAACCA:::wtd_mean_rcpp(errors^2, wt)/var_ACTUAL)))
    }
    results <- rbind(results, tmp)
  }
  if (!is.null(names.ZC)) {
    for (name in names.ZC) {
      tmp_ZC <- match[,paste0("ZC_", name)] == 1
      tmp_true0 <- actual[,name] == 0
      errors <- tmp_ZC != tmp_true0
      tmp <- c(paste0("ZC_", name), "MCR", BEAMM.KCCAACCA:::wtd_mean_rcpp(errors, wt))
      results <- rbind(results, tmp)
    }
  }
  if (length(names.NCV.cont) > 0) {
    results <- rbind(results, c("Total (cont.)", "RwsMSE", BEAMM.KCCAACCA:::compute_obj_function(
      BEAMM.KCCAACCA:::df2mtx(actual[, names.NCV.cont]), BEAMM.KCCAACCA:::df2mtx(match[, names.NCV.cont]), wt)
    ))
  }
  if (length(names.NCV.cat) > 0) {
    results <- rbind(results, c("Total (cat.)", "RwsMSE", BEAMM.KCCAACCA:::compute_obj_function(
      BEAMM.KCCAACCA:::df2mtx(actual[, names.NCV.cat]), BEAMM.KCCAACCA:::df2mtx(match[, names.NCV.cat]), wt)
    ))
  }
  results <- rbind(results, c("Total", "RwsMSE", BEAMM.KCCAACCA:::compute_obj_function(
    BEAMM.KCCAACCA:::df2mtx(actual[, names.NCV]), BEAMM.KCCAACCA:::df2mtx(match[, names.NCV]), wt)
  ))
  results <- rbind(results, c("Total", "Multivariate R^2 (Cohen, 2013)",
                              BEAMM.KCCAACCA::compute_multi_Rsquare(match, actual, names.NCV, wt,type = "book")))
  results <- rbind(results, c("Total", "Multivariate R^2 (Jones, 2019)",
                              BEAMM.KCCAACCA::compute_multi_Rsquare(match, actual, names.NCV, wt,type = "R")))
  results <- as.data.frame(results)
  rownames(results) <- NULL
  names(results) <- c("variable", "error", "value")
  results$value <- as.numeric(results$value)
  return(results)
}

# Values for ACCA
result_ACCA <- compute_results(cbind(df.rec, Y2),res1$final,names.NCV=colnames(Y2))


# Load results for KCCA
# source("try_article_KCCA_Beta.R)
load("~/resultat/25_novembre_2024/KCCA/Essais_KCCA_Beta.RDATA")

# Values for KCCA
result_KCCA <- compute_results(cbind(df.rec, Y2),res1$final,names.NCV=colnames(Y2))


# Final results
res_fin <- cbind(result_KCCA[,1:2],KCCA=result_KCCA$value, ACCA=result_ACCA_ter$value)
xtable(res_fin)
save(res_fin, file="~/resultat/25_novembre_2024/Res/res_fin_Beta.RDATA")
