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

########################## data format ############################
# dados.treino

mc_long <- 
  dados[,!(colnames(dados) %in% c("medida_colo","num_contra","num_contra_imp"))]

mc_wide <- pivot_wider(
  mc_long,
  names_from = ig_aval_sem,
  values_from = medida_colo_imp
)

mc_x <- mc_wide[,colnames(mc_wide) %in% c("sem24_26",
                                          "sem27_28",
                                          "sem29_30",
                                          "sem31_32",
                                          "sem33_34")]
mc_x <- as.matrix(mc_x)
mc_x <- I(mc_x)

nc_long <- 
  dados[,!(colnames(dados) %in% c("medida_colo","num_contra","medida_colo_imp"))]


nc_wide <- pivot_wider(
  nc_long,
  names_from = ig_aval_sem,
  values_from = num_contra_imp
)

nc_x <- nc_wide[,colnames(nc_wide) %in% c("sem24_26",
                                          "sem27_28",
                                          "sem29_30",
                                          "sem31_32",
                                          "sem33_34")]

nc_x <- as.matrix(nc_x)
nc_x <- I(nc_x)

dados_wide <- mc_wide[,!(colnames(mc_wide) %in% c("sem24_26",
                                                "sem27_28",
                                                "sem29_30",
                                                "sem31_32",
                                                "sem33_34"))]

dados_wide$mc_x <- mc_x
dados_wide$nc_x <- nc_x

################## lf() -  Regression splines ####################
##### for training sample and predictions for test sample #####
require(refund)

# sample(1:1000,1)
set.seed(441)

N <- length(dados_wide$igp_parto)
y <- dados_wide$igp_parto
test <- sample(1:263,79)

medida_colo <-  matrix(NA, 263,5)
for (i in 1:263) medida_colo[i,] = dados_wide$mc_x[i, ] # changes class from AsIs to matrix

num_contra <-  matrix(NA, 263,5)
for (i in 1:263) num_contra[i,] = dados_wide$nc_x[i, ] # changes class from AsIs to matrix


### Cervical measurements:


## Models, AIC, predictions, EQM:

# Obs: fit_mc_ps.3.t for example, means fit for model of 
#cervical measurements (mc, nc for contractions), with bs = ps,
#k = 3 and fx = TRUE.

# Error occurs 
# ps
fit_mc_ps.3.t <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )


fit_mc_ps.3.f <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

# Works fine
fit_mc_ps.4.t <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.4.t)

pred_mc_ps.4.t <- 
  predict(fit_mc_ps.4.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.4.t - dados_wide[test,]$igp_parto)^2) # EQM


fit_mc_ps.4.f <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 4, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.4.f)

pred_mc_ps.4.f <- 
  predict(fit_mc_ps.4.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_ps.5.t <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.5.t)

pred_mc_ps.5.t <- 
  predict(fit_mc_ps.5.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_ps.5.f <- 
  pfr(y ~ lf(medida_colo, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.5.f)

pred_mc_ps.5.f <- 
  predict(fit_mc_ps.5.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.5.f - dados_wide[test,]$igp_parto)^2) # EQM

# tp

fit_mc_tp.3.t <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 3, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.3.t)

pred_mc_tp.3.t <- 
  predict(fit_mc_tp.3.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.3.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_tp.3.f <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.3.f)

pred_mc_tp.3.f <- 
  predict(fit_mc_tp.3.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.3.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_tp.4.t <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.4.t)

pred_mc_tp.4.t <- 
  predict(fit_mc_tp.4.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.4.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_tp.4.f <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.4.f)

pred_mc_tp.4.f <- 
  predict(fit_mc_tp.4.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_tp.5.t <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.5.t)

pred_mc_tp.5.t <- 
  predict(fit_mc_tp.5.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_tp.5.f <- 
  pfr(y ~ lf(medida_colo, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.5.f)

pred_mc_tp.5.f <- 
  predict(fit_mc_tp.5.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.5.f - dados_wide[test,]$igp_parto)^2) # EQM

# cr

fit_mc_cr.3.t <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.3.t)

pred_mc_cr.3.t <- 
  predict(fit_mc_cr.3.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.3.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_cr.3.f <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.3.f)

pred_mc_cr.3.f <- 
  predict(fit_mc_cr.3.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.3.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_cr.4.t <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.4.t)

pred_mc_cr.4.t <- 
  predict(fit_mc_cr.4.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.4.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_cr.4.f <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.4.f)

pred_mc_cr.4.f <- 
  predict(fit_mc_cr.4.f , 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_cr.5.t <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.5.t)

pred_mc_cr.5.t <- 
  predict(fit_mc_cr.5.t, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_mc_cr.5.f <- 
  pfr(y ~ lf(medida_colo, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.5.f)

pred_mc_cr.5.f <- 
  predict(fit_mc_cr.5.f, 
          newdata = list(medida_colo = medida_colo[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.5.f - dados_wide[test,]$igp_parto)^2) # EQM

### Contraction measurements:

## Models, AIC, predictions:

# Error occurs 
# ps

fit_nc_ps.3.t <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

fit_nc_ps.3.f <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

# Works fine
fit_nc_ps.4.t <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.4.t)

pred_nc_ps.4.t <- 
  predict(fit_nc_ps.4.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.4.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_ps.4.f <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.4.f)

pred_nc_ps.4.f <- 
  predict(fit_nc_ps.4.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_ps.5.t <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 5, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.5.t)

pred_nc_ps.5.t <- 
  predict(fit_nc_ps.5.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_ps.5.f <- 
  pfr(y ~ lf(num_contra, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.5.f)

pred_nc_ps.5.f <-
  predict(fit_nc_ps.5.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.5.f - dados_wide[test,]$igp_parto)^2) # EQM

# tp

fit_nc_tp.3.t <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.3.t)

pred_nc_tp.3.t <- 
  predict(fit_nc_tp.3.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.3.t - dados_wide[test,]$igp_parto)^2) # EQM


fit_nc_tp.3.f <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 3, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.3.f)

pred_nc_tp.3.f <- 
  predict(fit_nc_tp.3.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.3.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_tp.4.t <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.4.t)

pred_nc_tp.4.t <-
  predict(fit_nc_tp.4.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.4.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_tp.4.f <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 4, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.4.f)

pred_nc_tp.4.f <- 
  predict(fit_nc_tp.4.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_tp.5.t <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.5.t)

pred_nc_tp.5.t <- 
  predict(fit_nc_tp.5.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_tp.5.f <- 
  pfr(y ~ lf(num_contra, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.5.f)

pred_nc_tp.5.f <- 
  predict(fit_nc_tp.5.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.5.f - dados_wide[test,]$igp_parto)^2) # EQM

# cr

fit_nc_cr.3.t <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.3.t)

pred_nc_cr.3.t <- 
  predict(fit_nc_cr.3.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.3.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_cr.3.f <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.3.f)

pred_nc_cr.3.f <- 
  predict(fit_nc_cr.3.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.3.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_cr.4.t <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.4.t)

pred_nc_cr.4.t <- 
  predict(fit_nc_cr.4.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.4.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_cr.4.f <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.4.f)

pred_nc_cr.4.f <- 
  predict(fit_nc_cr.4.f , 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.4.f - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_cr.5.t <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.5.t)

pred_nc_cr.5.t <- 
  predict(fit_nc_cr.5.t, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.5.t - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_cr.5.f <- 
  pfr(y ~ lf(num_contra, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.5.f)

pred_nc_cr.5.f <- 
  predict(fit_nc_cr.5.f, 
          newdata = list(num_contra = num_contra[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.5.f - dados_wide[test,]$igp_parto)^2) # EQM

############ Linear model for the evaluations mean #############
############      and the last evaluation       ################

### Cervical measurements:

# Data for model construction
dados_wide$mc_evaluations_mean <- rowMeans(dados_wide$mc_x)
dados_wide$mc_last_evaluation <- as.vector(dados_wide$mc_x[,"sem33_34"])
mc_treino <- dados_wide[-test,]

# Data for making predictions 
mc_teste <- dados_wide[test,]

## models and predictions
fit_mc_evaluation_mean <- 
  lm(igp_parto ~ mc_evaluations_mean, data = mc_treino)

summary(fit_mc_evaluation_mean)

pred_mc_evaluation_mean <- predict(fit_mc_evaluation_mean, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_evaluation_mean - dados_wide[test,]$igp_parto)^2) # EQM


fit_mc_last_evaluation <- 
  lm(igp_parto ~ mc_last_evaluation, data = mc_treino)

summary(fit_mc_last_evaluation)

pred_mc_last_evaluation <- predict(fit_mc_last_evaluation, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_last_evaluation - dados_wide[test,]$igp_parto)^2) # EQM

### Contraction measurements:

# Data for model construction
dados_wide$nc_evaluations_mean <- rowMeans(dados_wide$nc_x)
dados_wide$nc_last_evaluation <- as.vector(dados_wide$nc_x[,"sem33_34"])
nc_treino <- dados_wide[-test,]

# Data for making predictions 
nc_teste <- dados_wide[test,]

## models and predictions
fit_nc_evaluation_mean <- 
  lm(igp_parto ~ nc_evaluations_mean, data = nc_treino)

summary(fit_nc_evaluation_mean)

pred_nc_evaluation_mean <- predict(fit_nc_evaluation_mean, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_evaluation_mean - dados_wide[test,]$igp_parto)^2) # EQM

fit_nc_last_evaluation <- 
  lm(igp_parto ~ nc_last_evaluation, data = nc_treino)

summary(fit_nc_last_evaluation)

pred_nc_last_evaluation <- predict(fit_nc_last_evaluation, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_last_evaluation - dados_wide[test,]$igp_parto)^2) # EQM
