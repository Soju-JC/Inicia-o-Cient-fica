########################### Packages ##############################
rm(list = ls())
loadlibrary <- function(x){
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies = T)
    if(!require(x,character.only = TRUE)) 
      stop("Package not found")
  }
}

packages <- c(
  "tidyverse", 
  "readxl", 
  "janitor",
  "skimr", 
  "lubridate", 
  "summarytools",
  "magrittr", 
  "knitr", 
  "viridis",
  "cowplot",
  "tidyr", 
  "reshape2", 
  "VIM", 
  "mice",
  "MASS",
  "pROC"
)

lapply(packages, loadlibrary)

#Load the database
dados <- read_excel("dados_igs_completas_5avals.xlsx")
# View(dados)

######### Separating training sample and test sample  #############

# sample(1:1000,1)
set.seed(441)
id <- distinct(dados[,"id"],id)
n <- nrow(id) # Total sample size 
n.treino <- floor(0.7*n) # 70% training sample (calibration)
n.teste <- n-n.treino # 30% test sample (prediction)
obs.treino <- sample(1:n,n.treino) # Selected rows from id 

id.treino <- id[obs.treino,] # Selected id for training sample
id.teste <- id[-obs.treino,] # Selected id for test sample

dados.treino <- dados[dados$id %in% id.treino$id,] # Training sample
dados.teste <- dados[!(dados$id %in% id.treino$id),] # Test sample

########################## data format ############################
# dados.treino

mc_long_treino <- 
  dados.treino[,!(colnames(dados) %in% c("medida_colo","num_contra","num_contra_imp"))]

mc_wide_treino <- pivot_wider(
  mc_long_treino,
  names_from = ig_aval_sem,
  values_from = medida_colo_imp
)

nc_long_treino <- 
  dados.treino[,!(colnames(dados) %in% c("medida_colo","num_contra","medida_colo_imp"))]


nc_wide_treino <- pivot_wider(
  nc_long_treino,
  names_from = ig_aval_sem,
  values_from = num_contra_imp
)

# dados.teste

mc_long_teste <- 
  dados.teste[,!(colnames(dados) %in% c("medida_colo","num_contra","num_contra_imp"))]

mc_wide_teste <- pivot_wider(
  mc_long_teste,
  names_from = ig_aval_sem,
  values_from = medida_colo_imp
)

nc_long_teste <- 
  dados.teste[,!(colnames(dados) %in% c("medida_colo","num_contra","medida_colo_imp"))]


nc_wide_teste <- pivot_wider(
  nc_long_teste,
  names_from = ig_aval_sem,
  values_from = num_contra_imp
)

################## lf() -  Regression splines ####################
require(refund)

### Cervical measurements:

# Data for model construction
X_mc <- mc_wide_treino[, colnames(mc_wide_treino) %in% c("sem24_26",
                                                      "sem27_28",
                                                      "sem29_30",
                                                      "sem31_32",
                                                      "sem33_34")]

X_mc <- as.matrix(X_mc)
X_mc <- I(X_mc)
mc_wide_treino <- cbind(mc_wide_treino, X_mc)

# Data for making predictions 
mc_pred <- mc_wide_teste[, colnames(mc_wide_teste) %in% c("sem24_26", 
                                                          "sem27_28", 
                                                          "sem29_30", 
                                                          "sem31_32", 
                                                          "sem33_34")]
mc_pred <- as.matrix(mc_pred)

## Models, AIC, predictions:

# Obs: fit_mc_ps.3.t for example, means fit for model of 
#cervical measurements (mc, nc for contractions), with bs = ps,
#k = 3 and fx = TRUE.

# Error occurs 
fit_mc_ps.3.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 3, fx = TRUE), 
      data = mc_wide_treino
      )


fit_mc_ps.3.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 3, fx = FALSE), 
      data = mc_wide_treino
      )

# Works fine
fit_mc_ps.4.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 4, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_ps.4.t)

pred_mc_ps.4.t <- predict(fit_mc_ps.4.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )


fit_mc_ps.4.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 4, fx = FALSE),
      data = mc_wide_treino
      )

AIC(fit_mc_ps.4.f)

pred_mc_ps.4.f <- predict(fit_mc_ps.4.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_ps.5.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 5, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_ps.5.t)

pred_mc_ps.5.t <- predict(fit_mc_ps.5.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_ps.5.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "ps", k = 5, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_ps.5.f)

pred_mc_ps.5.f <- predict(fit_mc_ps.5.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )



fit_mc_tp.3.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 3, fx = TRUE),
      data = mc_wide_treino
      )

AIC(fit_mc_tp.3.t)

pred_mc_tp.3.t <- predict(fit_mc_tp.3.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )


fit_mc_tp.3.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 3, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_tp.3.f)

pred_mc_tp.3.f <- predict(fit_mc_tp.3.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )


fit_mc_tp.4.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 4, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_tp.4.t)

pred_mc_tp.4.t <- predict(fit_mc_tp.4.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_tp.4.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 4, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_tp.4.f)

pred_mc_tp.4.f <- predict(fit_mc_tp.4.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_tp.5.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 5, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_tp.5.t)

pred_mc_tp.5.t <- predict(fit_mc_tp.5.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_tp.5.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "tp", k = 5, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_tp.5.f)

pred_mc_tp.5.f <- predict(fit_mc_tp.5.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )



fit_mc_cr.3.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 3, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.3.t)

pred_mc_cr.3.t <- predict(fit_mc_cr.3.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_cr.3.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 3, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.3.f)

pred_mc_cr.3.f <- predict(fit_mc_cr.3.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_cr.4.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 4, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.4.t)

pred_mc_cr.4.t <- predict(fit_mc_cr.4.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_cr.4.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 4, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.4.f)

pred_mc_cr.4.f <- predict(fit_mc_cr.4.f , 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_cr.5.t <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 5, fx = TRUE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.5.t)

pred_mc_cr.5.t <- predict(fit_mc_cr.5.t, 
                          newdata = mc_pred,
                          type=c("response")
                          )

fit_mc_cr.5.f <- 
  pfr(igp_parto ~ lf(X_mc, bs = "cr", k = 5, fx = FALSE), 
      data = mc_wide_treino
      )

AIC(fit_mc_cr.5.f)

pred_mc_cr.5.f <- predict(fit_mc_cr.5.f, 
                          newdata = mc_pred,
                          type=c("response")
                          )


### Contraction measurements:

# Data for model construction
X_nc <- nc_wide_treino[, colnames(nc_wide_treino) %in% c("sem24_26",
                                                         "sem27_28",
                                                         "sem29_30",
                                                         "sem31_32",
                                                         "sem33_34")]

X_nc <- as.matrix(X_nc)
X_nc <- I(X_nc)
nc_wide_treino <- cbind(nc_wide_treino, X_nc)

# Data for making predictions 
nc_pred <- nc_wide_teste[, colnames(nc_wide_teste) %in% c("sem24_26", 
                                                          "sem27_28", 
                                                          "sem29_30", 
                                                          "sem31_32", 
                                                          "sem33_34")]
nc_pred <- as.matrix(nc_pred)

## Models, AIC, predictions:

# Error occurs 
fit_nc_ps.3.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 3, fx = TRUE), 
      data = nc_wide_treino
      )

fit_nc_ps.3.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 3, fx = FALSE), 
      data = nc_wide_treino
      )

# Works fine
fit_nc_ps.4.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 4, fx = TRUE),
      data = nc_wide_treino
      )

AIC(fit_nc_ps.4.t)

pred_nc_ps.4.t <- predict(fit_nc_ps.4.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )


fit_nc_ps.4.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 4, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_ps.4.f)

pred_nc_ps.4.f <- predict(fit_nc_ps.4.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_ps.5.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 5, fx = TRUE),
      data = nc_wide_treino
      )

AIC(fit_nc_ps.5.t)

pred_nc_ps.5.t <- predict(fit_nc_ps.5.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_ps.5.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "ps", k = 5, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_ps.5.f)

pred_nc_ps.5.f <- predict(fit_nc_ps.5.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )



fit_nc_tp.3.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 3, fx = TRUE), 
      data = nc_wide_treino
      )

AIC(fit_nc_tp.3.t)

pred_nc_tp.3.t <- predict(fit_nc_tp.3.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )


fit_nc_tp.3.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 3, fx = FALSE),
      data = nc_wide_treino
      )

AIC(fit_nc_tp.3.f)

pred_nc_tp.3.f <- predict(fit_nc_tp.3.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )


fit_nc_tp.4.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 4, fx = TRUE),
      data = nc_wide_treino
      )

AIC(fit_nc_tp.4.t)

pred_nc_tp.4.t <- predict(fit_nc_tp.4.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_tp.4.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 4, fx = FALSE),
      data = nc_wide_treino
      )

AIC(fit_nc_tp.4.f)

pred_nc_tp.4.f <- predict(fit_nc_tp.4.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_tp.5.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 5, fx = TRUE), 
      data = nc_wide_treino
      )

AIC(fit_nc_tp.5.t)

pred_nc_tp.5.t <- predict(fit_nc_tp.5.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_tp.5.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "tp", k = 5, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_tp.5.f)

pred_nc_tp.5.f <- predict(fit_nc_tp.5.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )



fit_nc_cr.3.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 3, fx = TRUE), 
      data = nc_wide_treino
      )

AIC(fit_nc_cr.3.t)

pred_nc_cr.3.t <- predict(fit_nc_cr.3.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_cr.3.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 3, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_cr.3.f)

pred_nc_cr.3.f <- predict(fit_nc_cr.3.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_cr.4.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 4, fx = TRUE),
      data = nc_wide_treino
      )

AIC(fit_nc_cr.4.t)

pred_nc_cr.4.t <- predict(fit_nc_cr.4.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_cr.4.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 4, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_cr.4.f)

pred_nc_cr.4.f <- predict(fit_nc_cr.4.f , 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_cr.5.t <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 5, fx = TRUE), 
      data = nc_wide_treino
      )

AIC(fit_nc_cr.5.t)

pred_nc_cr.5.t <- predict(fit_nc_cr.5.t, 
                          newdata = nc_pred,
                          type=c("response")
                          )

fit_nc_cr.5.f <- 
  pfr(igp_parto ~ lf(X_nc, bs = "cr", k = 5, fx = FALSE), 
      data = nc_wide_treino
      )

AIC(fit_nc_cr.5.f)

pred_nc_cr.5.f <- predict(fit_nc_cr.5.f, 
                          newdata = nc_pred,
                          type=c("response")
                          )


############ Linear model for the evaluations mean #############
############      and the last evaluation       ################

### Cervical measurements:

# Data for model construction
mc_wide_treino$evaluations_mean <- 
  rowMeans(subset(mc_wide_treino, select = c(sem24_26,
                                            sem27_28,
                                            sem29_30,
                                            sem31_32,
                                            sem33_34)))


# Data for making predictions 
mc_wide_teste$evaluations_mean <- 
  rowMeans(subset(mc_wide_teste, select = c(sem24_26,
                                             sem27_28,
                                             sem29_30,
                                             sem31_32,
                                             sem33_34)))

## models and predictions
fit_mc_evaluation_mean <- 
  lm(igp_parto ~ evaluations_mean, data = mc_wide_treino)

summary(fit_mc_evaluation_mean)

pred_mc_evaluation_mean <- predict(fit_mc_evaluation_mean, 
                                   newdata = mc_wide_teste, 
                                   type=c("response")
                                   )


fit_mc_last_evaluation <- 
  lm(igp_parto ~ sem33_34, data = mc_wide_treino)

summary(fit_mc_last_evaluation)

pred_mc_last_evaluation <- predict(fit_mc_last_evaluation, 
                                   newdata = mc_wide_teste, 
                                   type=c("response")
                                   )

### Contraction measurements:

# Data for model construction
nc_wide_treino$evaluations_mean <- 
  rowMeans(subset(nc_wide_treino, select = c(sem24_26,
                                             sem27_28,
                                             sem29_30,
                                             sem31_32,
                                             sem33_34)))


# Data for making predictions 
nc_wide_teste$evaluations_mean <- 
  rowMeans(subset(nc_wide_teste, select = c(sem24_26,
                                            sem27_28,
                                            sem29_30,
                                            sem31_32,
                                            sem33_34)))

## models and predictions
fit_nc_evaluation_mean <- 
  lm(igp_parto ~ evaluations_mean, data = nc_wide_treino)

summary(fit_nc_evaluation_mean)

pred_nc_evaluation_mean <- predict(fit_nc_evaluation_mean, 
                                   newdata = nc_wide_teste, 
                                   type=c("response")
                                   )


fit_nc_last_evaluation <- 
  lm(igp_parto ~ sem33_34, data = nc_wide_treino)

summary(fit_nc_last_evaluation)

pred_nc_last_evaluation <- predict(fit_nc_last_evaluation, 
                                   newdata = nc_wide_teste, 
                                   type=c("response")
                                   )

#=======================================================================================
