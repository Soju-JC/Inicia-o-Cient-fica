#Construction of models by lf() for the data WITH the transformation of the
#response variable, considering three different methods of data imputation. 

################################# Packages ####################################
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
dados <- read_excel("dados_igs_completas_5avals_3imput.xlsx")
# View(dados)

############################### data format ###################################
#Note: Adapting the format of the functional variables for analysis,
#where these variables had 3 different data imputation methods used,
#_imp, _compl, and _compl_soIG. 

############################## _imp ##############################
mc_long_imp <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl", 
                                  "medida_colo_compl_soIG",
                                  "num_contra", 
                                  "num_contra_compl",
                                  "num_contra_compl_soIG", 
                                  "num_contra_imp"
                                  ))]

mc_wide_imp <- 
  pivot_wider(
    mc_long_imp, 
    names_from = ig_aval_sem, 
    values_from = medida_colo_imp
    )

mc_x_imp <-
  mc_wide_imp[,colnames(mc_wide_imp) %in% c("sem24_26", 
                                            "sem27_28",
                                            "sem29_30",
                                            "sem31_32",
                                            "sem33_34"
                                            )]
mc_x_imp <- as.matrix(mc_x_imp)
mc_x_imp <- I(mc_x_imp)

nc_long_imp <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl", 
                                  "medida_colo_compl_soIG", 
                                  "medida_colo_imp",
                                  "num_contra", 
                                  "num_contra_compl", 
                                  "num_contra_compl_soIG"
                                  ))]


nc_wide_imp <- 
  pivot_wider(
    nc_long_imp,
    names_from = ig_aval_sem,
    values_from = num_contra_imp
    )

nc_x_imp <- 
  nc_wide_imp[,colnames(nc_wide_imp) %in% c("sem24_26", 
                                            "sem27_28",
                                            "sem29_30",
                                            "sem31_32",
                                            "sem33_34"
                                            )]

nc_x_imp <- as.matrix(nc_x_imp)
nc_x_imp <- I(nc_x_imp)

############################## _compl ###########################
mc_long_compl <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl_soIG", 
                                  "medida_colo_imp",
                                  "num_contra", 
                                  "num_contra_compl",
                                  "num_contra_compl_soIG", 
                                  "num_contra_imp"
                                  ))]

mc_wide_compl <- 
  pivot_wider(
    mc_long_compl, 
    names_from = ig_aval_sem, 
    values_from = medida_colo_compl
  )

mc_x_compl <-
  mc_wide_compl[,colnames(mc_wide_compl) %in% c("sem24_26", 
                                                "sem27_28",
                                                "sem29_30",
                                                "sem31_32",
                                                "sem33_34"
                                                )]
mc_x_compl <- as.matrix(mc_x_compl)
mc_x_compl <- I(mc_x_compl)

nc_long_compl <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl", 
                                  "medida_colo_compl_soIG", 
                                  "medida_colo_imp",
                                  "num_contra", 
                                  "num_contra_compl_soIG", 
                                  "num_contra_imp"
                                  ))]


nc_wide_compl <- 
  pivot_wider(
    nc_long_compl,
    names_from = ig_aval_sem,
    values_from = num_contra_compl
  )

nc_x_compl <- 
  nc_wide_compl[,colnames(nc_wide_compl) %in% c("sem24_26", 
                                                "sem27_28",
                                                "sem29_30",
                                                "sem31_32",
                                                "sem33_34"
                                                )]

nc_x_compl <- as.matrix(nc_x_compl)
nc_x_compl <- I(nc_x_compl)

########################## _compl_soIG ##########################
mc_long_compl_soIG <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl", 
                                  "medida_colo_imp",
                                  "num_contra", 
                                  "num_contra_compl",
                                  "num_contra_compl_soIG", 
                                  "num_contra_imp"
                                  ))]

mc_wide_compl_soIG <- 
  pivot_wider(
    mc_long_compl_soIG, 
    names_from = ig_aval_sem, 
    values_from = medida_colo_compl_soIG
  )

mc_x_compl_soIG <-
  mc_wide_compl_soIG[,colnames(mc_wide_compl_soIG) %in% c("sem24_26", 
                                                          "sem27_28",
                                                          "sem29_30",
                                                          "sem31_32",
                                                          "sem33_34"
                                                          )]
mc_x_compl_soIG <- as.matrix(mc_x_compl_soIG)
mc_x_compl_soIG <- I(mc_x_compl_soIG)

nc_long_compl_soIG <- 
  dados[,!(colnames(dados) %in% c("medida_colo", 
                                  "medida_colo_compl", 
                                  "medida_colo_compl_soIG", 
                                  "medida_colo_imp",
                                  "num_contra", 
                                  "num_contra_compl", 
                                  "num_contra_imp"
                                  ))]


nc_wide_compl_soIG <- 
  pivot_wider(
    nc_long_compl_soIG,
    names_from = ig_aval_sem,
    values_from = num_contra_compl_soIG
  )

nc_x_compl_soIG <- 
  nc_wide_compl_soIG[,colnames(nc_wide_compl_soIG) %in% c("sem24_26", 
                                                          "sem27_28",
                                                          "sem29_30",
                                                          "sem31_32",
                                                          "sem33_34"
                                                          )]

nc_x_compl_soIG <- as.matrix(nc_x_compl_soIG)
nc_x_compl_soIG <- I(nc_x_compl_soIG)

## All variables in the desired format in 1 dataframe:
dados_wide <- 
  mc_wide_imp[,!(colnames(mc_wide_imp) %in% c("sem24_26", 
                                              "sem27_28",
                                              "sem29_30",
                                              "sem31_32",
                                              "sem33_34"
                                              ))]

dados_wide$mc_x_imp <- mc_x_imp
dados_wide$nc_x_imp <- nc_x_imp
dados_wide$mc_x_compl <- mc_x_compl
dados_wide$nc_x_compl <- nc_x_compl
dados_wide$mc_x_compl_soIG <- mc_x_compl_soIG
dados_wide$nc_x_compl_soIG <- nc_x_compl_soIG

################# Function for the following models plots ####################
plot_ajuste <- function(fitJJ){ 
  plot(
    fitJJ,
    ylab = expression(paste(beta(t))), 
    xlab = "t", 
    xlim = c(0, 1), 
    xaxt = "n",
    cex.lab = 1,
    cex.axis = 1
  )
  
  axis(side = 1, 
       at = seq(0, 1, 0.25),
       lty = 1, 
       labels = seq(0, 1, 0.25)
  )
}

##############################################################################
#################### Models of data with _imp imputation #####################
##############################################################################

################## lf() -  Regression splines ####################
##### for training sample and predictions for test sample #####
require(refund)

# sample(1:1000,1)
set.seed(441)

# Transformed Y:
dados_wide$igp_parto_transformado <- 
  log((max(dados_wide$igp_parto) - dados_wide$igp_parto) + 1)  

y <- dados_wide$igp_parto_transformado
N <- length(dados_wide$igp_parto_transformado)
test <- sample(1:263,79)

medida_colo_imp <-  matrix(NA, 263,5)
for (i in 1:263) medida_colo_imp[i,] = dados_wide$mc_x_imp[i, ] # changes class from AsIs to matrix

num_contra_imp <-  matrix(NA, 263,5)
for (i in 1:263) num_contra_imp[i,] = dados_wide$nc_x_imp[i, ] # changes class from AsIs to matrix


                    ###### Cervical measurements ######


## Models, AIC, predictions, EQM, plots:

# Obs: fit_mc_ps.3.t for example, means fit for model of 
#cervical measurements (mc, nc for contractions), with bs = ps,
#k = 3 and fx = TRUE.

######################### ps ###############################
# Error occurs 

fit_mc_ps.3.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )


fit_mc_ps.3.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

# Works fine
fit_mc_ps.4.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.4.t)

pred_mc_ps.4.t <- 
  predict(fit_mc_ps.4.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.4.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 4, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.4.f)

pred_mc_ps.4.f <- 
  predict(fit_mc_ps.4.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.5.t)

pred_mc_ps.5.t <- 
  predict(fit_mc_ps.5.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_ps.5.f)

pred_mc_ps.5.f <- 
  predict(fit_mc_ps.5.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_mc_tp.3.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 3, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.3.t)

pred_mc_tp.3.t <- 
  predict(fit_mc_tp.3.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.3.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.3.f)

pred_mc_tp.3.f <- 
  predict(fit_mc_tp.3.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.4.t)

pred_mc_tp.4.t <- 
  predict(fit_mc_tp.4.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.4.f)

pred_mc_tp.4.f <- 
  predict(fit_mc_tp.4.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.5.t)

pred_mc_tp.5.t <- 
  predict(fit_mc_tp.5.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_tp.5.f)

pred_mc_tp.5.f <- 
  predict(fit_mc_tp.5.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_mc_cr.3.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.3.t)

pred_mc_cr.3.t <- 
  predict(fit_mc_cr.3.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.3.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.3.f)

pred_mc_cr.3.f <- 
  predict(fit_mc_cr.3.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.4.t)

pred_mc_cr.4.t <- 
  predict(fit_mc_cr.4.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.4.f)

pred_mc_cr.4.f <- 
  predict(fit_mc_cr.4.f , 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.t <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.5.t)

pred_mc_cr.5.t <- 
  predict(fit_mc_cr.5.t, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.f <- 
  pfr(y ~ lf(medida_colo_imp, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_mc_cr.5.f)

pred_mc_cr.5.f <- 
  predict(fit_mc_cr.5.f, 
          newdata = list(medida_colo_imp = medida_colo_imp[test,]), 
          type = c("response")
          )

mean((pred_mc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()


                   ###### Contraction measurements ######

## Models, AIC, predictions, EQM, plots:

######################### ps ###############################
# Error occurs 

fit_nc_ps.3.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

fit_nc_ps.3.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

# Works fine
fit_nc_ps.4.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.4.t)

pred_nc_ps.4.t <- 
  predict(fit_nc_ps.4.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.4.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.4.f)

pred_nc_ps.4.f <- 
  predict(fit_nc_ps.4.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 5, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.5.t)

pred_nc_ps.5.t <- 
  predict(fit_nc_ps.5.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_ps.5.f)

pred_nc_ps.5.f <-
  predict(fit_nc_ps.5.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_nc_tp.3.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.3.t)

pred_nc_tp.3.t <- 
  predict(fit_nc_tp.3.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.3.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 3, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.3.f)

pred_nc_tp.3.f <- 
  predict(fit_nc_tp.3.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.4.t)

pred_nc_tp.4.t <-
  predict(fit_nc_tp.4.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 4, fx = FALSE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.4.f)

pred_nc_tp.4.f <- 
  predict(fit_nc_tp.4.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.5.t)

pred_nc_tp.5.t <- 
  predict(fit_nc_tp.5.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_tp.5.f)

pred_nc_tp.5.f <- 
  predict(fit_nc_tp.5.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_nc_cr.3.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.3.t)

pred_nc_cr.3.t <- 
  predict(fit_nc_cr.3.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.3.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.3.f)

pred_nc_cr.3.f <- 
  predict(fit_nc_cr.3.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 4, fx = TRUE),
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.4.t)

pred_nc_cr.4.t <- 
  predict(fit_nc_cr.4.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.4.f)

pred_nc_cr.4.f <- 
  predict(fit_nc_cr.4.f , 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.t <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.5.t)

pred_nc_cr.5.t <- 
  predict(fit_nc_cr.5.t, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.f <- 
  pfr(y ~ lf(num_contra_imp, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
      )

AIC(fit_nc_cr.5.f)

pred_nc_cr.5.f <- 
  predict(fit_nc_cr.5.f, 
          newdata = list(num_contra_imp = num_contra_imp[test,]), 
          type = c("response")
          )

mean((pred_nc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()


################### Linear model for the evaluations mean ####################
###################      and the last evaluation       #######################

                    ###### Cervical measurements ######

#Data for model construction
dados_wide$mc_evaluations_mean <- 
  rowMeans(dados_wide$mc_x_imp)

dados_wide$mc_last_evaluation <-
  as.vector(dados_wide$mc_x_imp[,"sem33_34"])

mc_treino <- dados_wide[-test,]

#Data for making predictions 
mc_teste <- dados_wide[test,]

#Models and predictions

                           #### Mean ####

fit_mc_evaluation_mean <- 
  lm(igp_parto_transformado ~ mc_evaluations_mean, data = mc_treino)

summary(fit_mc_evaluation_mean)

pred_mc_evaluation_mean <- predict(fit_mc_evaluation_mean, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                     #### Last evaluation ####

fit_mc_last_evaluation <- 
  lm(igp_parto_transformado ~ mc_last_evaluation, data = mc_treino)

summary(fit_mc_last_evaluation)

pred_mc_last_evaluation <- predict(fit_mc_last_evaluation, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                   ###### Contraction measurements ######

#Data for model construction
dados_wide$nc_evaluations_mean <- 
  rowMeans(dados_wide$nc_x_imp)

dados_wide$nc_last_evaluation <- 
  as.vector(dados_wide$nc_x_imp[,"sem33_34"])

nc_treino <- dados_wide[-test,]

#Data for making predictions 
nc_teste <- dados_wide[test,]

#Models and predictions

                             #### Mean ####

fit_nc_evaluation_mean <- 
  lm(igp_parto_transformado ~ nc_evaluations_mean, data = nc_treino)

summary(fit_nc_evaluation_mean)

pred_nc_evaluation_mean <- predict(fit_nc_evaluation_mean, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                       #### Last evaluation ####

fit_nc_last_evaluation <- 
  lm(igp_parto_transformado ~ nc_last_evaluation, data = nc_treino)

summary(fit_nc_last_evaluation)

pred_nc_last_evaluation <- predict(fit_nc_last_evaluation, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

##############################################################################
################### Models of data with _compl imputation ####################
##############################################################################

################## lf() -  Regression splines ####################
##### for training sample and predictions for test sample #####

# sample(1:1000,1)

medida_colo_compl <-  matrix(NA, 263,5)
for (i in 1:263) medida_colo_compl[i,] = dados_wide$mc_x_compl[i, ] # changes class from AsIs to matrix

num_contra_compl <-  matrix(NA, 263,5)
for (i in 1:263) num_contra_compl[i,] = dados_wide$nc_x_compl[i, ] # changes class from AsIs to matrix


                    ###### Cervical measurements ######


## Models, AIC, predictions, EQM:

# Obs: fit_mc_ps.3.t for example, means fit for model of 
#cervical measurements (mc, nc for contractions), with bs = ps,
#k = 3 and fx = TRUE.

######################### ps ###############################
# Error occurs 

fit_mc_ps.3.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )


fit_mc_ps.3.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

# Works fine
fit_mc_ps.4.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.4.t)

pred_mc_ps.4.t <- 
  predict(fit_mc_ps.4.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.4.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 4, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.4.f)

pred_mc_ps.4.f <- 
  predict(fit_mc_ps.4.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.5.t)

pred_mc_ps.5.t <- 
  predict(fit_mc_ps.5.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.5.f)

pred_mc_ps.5.f <- 
  predict(fit_mc_ps.5.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_mc_tp.3.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 3, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.3.t)

pred_mc_tp.3.t <- 
  predict(fit_mc_tp.3.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.3.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.3.f)

pred_mc_tp.3.f <- 
  predict(fit_mc_tp.3.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.4.t)

pred_mc_tp.4.t <- 
  predict(fit_mc_tp.4.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.4.f)

pred_mc_tp.4.f <- 
  predict(fit_mc_tp.4.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.5.t)

pred_mc_tp.5.t <- 
  predict(fit_mc_tp.5.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.5.f)

pred_mc_tp.5.f <- 
  predict(fit_mc_tp.5.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_mc_cr.3.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.3.t)

pred_mc_cr.3.t <- 
  predict(fit_mc_cr.3.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.3.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.3.f)

pred_mc_cr.3.f <- 
  predict(fit_mc_cr.3.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.4.t)

pred_mc_cr.4.t <- 
  predict(fit_mc_cr.4.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.4.f)

pred_mc_cr.4.f <- 
  predict(fit_mc_cr.4.f , 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.t <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.5.t)

pred_mc_cr.5.t <- 
  predict(fit_mc_cr.5.t, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.f <- 
  pfr(y ~ lf(medida_colo_compl, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.5.f)

pred_mc_cr.5.f <- 
  predict(fit_mc_cr.5.f, 
          newdata = list(medida_colo_compl = medida_colo_compl[test,]), 
          type = c("response")
  )

mean((pred_mc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()


                 ###### Contraction measurements ######

## Models, AIC, predictions, EQM, plots:

######################### ps ###############################
# Error occurs 

fit_nc_ps.3.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

fit_nc_ps.3.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

# Works fine
fit_nc_ps.4.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.4.t)

pred_nc_ps.4.t <- 
  predict(fit_nc_ps.4.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.4.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.4.f)

pred_nc_ps.4.f <- 
  predict(fit_nc_ps.4.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 5, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.5.t)

pred_nc_ps.5.t <- 
  predict(fit_nc_ps.5.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.5.f)

pred_nc_ps.5.f <-
  predict(fit_nc_ps.5.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_nc_tp.3.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.3.t)

pred_nc_tp.3.t <- 
  predict(fit_nc_tp.3.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.3.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 3, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.3.f)

pred_nc_tp.3.f <- 
  predict(fit_nc_tp.3.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.4.t)

pred_nc_tp.4.t <-
  predict(fit_nc_tp.4.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 4, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.4.f)

pred_nc_tp.4.f <- 
  predict(fit_nc_tp.4.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.5.t)

pred_nc_tp.5.t <- 
  predict(fit_nc_tp.5.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.5.f)

pred_nc_tp.5.f <- 
  predict(fit_nc_tp.5.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_nc_cr.3.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.3.t)

pred_nc_cr.3.t <- 
  predict(fit_nc_cr.3.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.3.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.3.f)

pred_nc_cr.3.f <- 
  predict(fit_nc_cr.3.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.4.t)

pred_nc_cr.4.t <- 
  predict(fit_nc_cr.4.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.4.f)

pred_nc_cr.4.f <- 
  predict(fit_nc_cr.4.f , 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.t <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.5.t)

pred_nc_cr.5.t <- 
  predict(fit_nc_cr.5.t, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.f <- 
  pfr(y ~ lf(num_contra_compl, bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.5.f)

pred_nc_cr.5.f <- 
  predict(fit_nc_cr.5.f, 
          newdata = list(num_contra_compl = num_contra_compl[test,]), 
          type = c("response")
  )

mean((pred_nc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

################### Linear model for the evaluations mean ####################
###################      and the last evaluation       #######################

                   ###### Cervical measurements ######

#Data for model construction
dados_wide$mc_evaluations_mean <- 
  rowMeans(dados_wide$mc_x_compl)

dados_wide$mc_last_evaluation <- 
  as.vector(dados_wide$mc_x_compl[,"sem33_34"])

mc_treino <- dados_wide[-test,]

#Data for making predictions 
mc_teste <- dados_wide[test,]

#Models and predictions

                            #### Mean ####

fit_mc_evaluation_mean <- 
  lm(igp_parto_transformado ~ mc_evaluations_mean, data = mc_treino)

summary(fit_mc_evaluation_mean)

pred_mc_evaluation_mean <- predict(fit_mc_evaluation_mean, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                      #### Last evaluation ####

fit_mc_last_evaluation <- 
  lm(igp_parto_transformado ~ mc_last_evaluation, data = mc_treino)

summary(fit_mc_last_evaluation)

pred_mc_last_evaluation <- predict(fit_mc_last_evaluation, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                  ###### Contraction measurements ######

#Data for model construction
dados_wide$nc_evaluations_mean <- 
  rowMeans(dados_wide$nc_x_compl)

dados_wide$nc_last_evaluation <- 
  as.vector(dados_wide$nc_x_compl[,"sem33_34"])

nc_treino <- dados_wide[-test,]

#Data for making predictions 
nc_teste <- dados_wide[test,]

#Models and predictions

                             #### Mean ####

fit_nc_evaluation_mean <- 
  lm(igp_parto_transformado ~ nc_evaluations_mean, data = nc_treino)

summary(fit_nc_evaluation_mean)

pred_nc_evaluation_mean <- predict(fit_nc_evaluation_mean, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                      #### Last evaluation ####

fit_nc_last_evaluation <- 
  lm(igp_parto_transformado ~ nc_last_evaluation, data = nc_treino)

summary(fit_nc_last_evaluation)

pred_nc_last_evaluation <- predict(fit_nc_last_evaluation, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

##############################################################################
################# Models of data with _compl_soIG imputation #################
##############################################################################

################## lf() -  Regression splines ####################
##### for training sample and predictions for test sample #####

medida_colo_compl_soIG  <-  matrix(NA, 263,5)
for (i in 1:263) medida_colo_compl_soIG [i,] = dados_wide$mc_x_compl_soIG [i, ] # changes class from AsIs to matrix

num_contra_compl_soIG  <-  matrix(NA, 263,5)
for (i in 1:263) num_contra_compl_soIG [i,] = dados_wide$nc_x_compl_soIG [i, ] # changes class from AsIs to matrix


                   ###### Cervical measurements ######


## Models, AIC, predictions, EQM, plots:

# Obs: fit_mc_ps.3.t for example, means fit for model of 
#cervical measurements (mc, nc for contractions), with bs = ps,
#k = 3 and fx = TRUE.

######################### ps ###############################
# Error occurs 

fit_mc_ps.3.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )


fit_mc_ps.3.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

# Works fine
fit_mc_ps.4.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.4.t)

pred_mc_ps.4.t <- 
  predict(fit_mc_ps.4.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.4.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 4, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.4.f)

pred_mc_ps.4.f <- 
  predict(fit_mc_ps.4.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.5.t)

pred_mc_ps.5.t <- 
  predict(fit_mc_ps.5.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_ps.5.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_ps.5.f)

pred_mc_ps.5.f <- 
  predict(fit_mc_ps.5.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_mc_tp.3.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 3, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.3.t)

pred_mc_tp.3.t <- 
  predict(fit_mc_tp.3.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.3.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.3.f)

pred_mc_tp.3.f <- 
  predict(fit_mc_tp.3.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.4.t)

pred_mc_tp.4.t <- 
  predict(fit_mc_tp.4.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.4.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.4.f)

pred_mc_tp.4.f <- 
  predict(fit_mc_tp.4.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.5.t)

pred_mc_tp.5.t <- 
  predict(fit_mc_tp.5.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_tp.5.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_tp.5.f)

pred_mc_tp.5.f <- 
  predict(fit_mc_tp.5.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_mc_cr.3.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.3.t)

pred_mc_cr.3.t <- 
  predict(fit_mc_cr.3.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.3.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.3.f)

pred_mc_cr.3.f <- 
  predict(fit_mc_cr.3.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 4, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.4.t)

pred_mc_cr.4.t <- 
  predict(fit_mc_cr.4.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.4.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.4.f)

pred_mc_cr.4.f <- 
  predict(fit_mc_cr.4.f , 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.t <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.5.t)

pred_mc_cr.5.t <- 
  predict(fit_mc_cr.5.t, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_mc_cr.5.f <- 
  pfr(y ~ lf(medida_colo_compl_soIG , bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_mc_cr.5.f)

pred_mc_cr.5.f <- 
  predict(fit_mc_cr.5.f, 
          newdata = list(medida_colo_compl_soIG  = medida_colo_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_mc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()


                   ###### Contraction measurements ######

## Models, AIC, predictions, EQM, plots:

######################### ps ###############################
# Error occurs 

fit_nc_ps.3.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

fit_nc_ps.3.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

# Works fine
fit_nc_ps.4.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.4.t)

pred_nc_ps.4.t <- 
  predict(fit_nc_ps.4.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_ps.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.4.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.4.f)

pred_nc_ps.4.f <- 
  predict(fit_nc_ps.4.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_ps.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 5, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.5.t)

pred_nc_ps.5.t <- 
  predict(fit_nc_ps.5.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_ps.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_ps.5.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "ps", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_ps.5.f)

pred_nc_ps.5.f <-
  predict(fit_nc_ps.5.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_ps.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### tp ###############################

fit_nc_tp.3.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.3.t)

pred_nc_tp.3.t <- 
  predict(fit_nc_tp.3.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.3.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 3, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.3.f)

pred_nc_tp.3.f <- 
  predict(fit_nc_tp.3.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.4.t)

pred_nc_tp.4.t <-
  predict(fit_nc_tp.4.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.4.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 4, fx = FALSE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.4.f)

pred_nc_tp.4.f <- 
  predict(fit_nc_tp.4.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.5.t)

pred_nc_tp.5.t <- 
  predict(fit_nc_tp.5.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_tp.5.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "tp", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_tp.5.f)

pred_nc_tp.5.f <- 
  predict(fit_nc_tp.5.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_tp.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

######################### cr ###############################

fit_nc_cr.3.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 3, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.3.t)

pred_nc_cr.3.t <- 
  predict(fit_nc_cr.3.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.3.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.3.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 3, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.3.f)

pred_nc_cr.3.f <- 
  predict(fit_nc_cr.3.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.3.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 4, fx = TRUE),
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.4.t)

pred_nc_cr.4.t <- 
  predict(fit_nc_cr.4.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.4.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.4.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 4, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.4.f)

pred_nc_cr.4.f <- 
  predict(fit_nc_cr.4.f , 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.4.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.t <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 5, fx = TRUE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.5.t)

pred_nc_cr.5.t <- 
  predict(fit_nc_cr.5.t, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.5.t - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()

############################################################

fit_nc_cr.5.f <- 
  pfr(y ~ lf(num_contra_compl_soIG , bs = "cr", k = 5, fx = FALSE), 
      subset = (1:N)[-test]
  )

AIC(fit_nc_cr.5.f)

pred_nc_cr.5.f <- 
  predict(fit_nc_cr.5.f, 
          newdata = list(num_contra_compl_soIG  = num_contra_compl_soIG [test,]), 
          type = c("response")
  )

mean((pred_nc_cr.5.f - dados_wide[test,]$igp_parto_transformado)^2) # EQM

plot_ajuste()


################### Linear model for the evaluations mean ####################
###################      and the last evaluation       #######################

                    ###### Cervical measurements ######

#Data for model construction
dados_wide$mc_evaluations_mean <- 
  rowMeans(dados_wide$mc_x_compl_soIG )

dados_wide$mc_last_evaluation <- 
  as.vector(dados_wide$mc_x_compl_soIG [,"sem33_34"])

mc_treino <- dados_wide[-test,]

#Data for making predictions 
mc_teste <- dados_wide[test,]

#Models and predictions

                            #### Mean ####

fit_mc_evaluation_mean <- 
  lm(igp_parto_transformado ~ mc_evaluations_mean, data = mc_treino)

summary(fit_mc_evaluation_mean)

pred_mc_evaluation_mean <- predict(fit_mc_evaluation_mean, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                      #### Last evaluation ####

fit_mc_last_evaluation <- 
  lm(igp_parto_transformado ~ mc_last_evaluation, data = mc_treino)

summary(fit_mc_last_evaluation)

pred_mc_last_evaluation <- predict(fit_mc_last_evaluation, 
                                   newdata = mc_teste, 
                                   type = c("response")
                                   )

mean((pred_mc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                   ###### Contraction measurements ######

#Data for model construction
dados_wide$nc_evaluations_mean <-
  rowMeans(dados_wide$nc_x_compl_soIG )

dados_wide$nc_last_evaluation <- 
  as.vector(dados_wide$nc_x_compl_soIG [,"sem33_34"])

nc_treino <- dados_wide[-test,]

#Data for making predictions 
nc_teste <- dados_wide[test,]

#Models and predictions

                             #### Mean ####

fit_nc_evaluation_mean <- 
  lm(igp_parto_transformado ~ nc_evaluations_mean, data = nc_treino)

summary(fit_nc_evaluation_mean)

pred_nc_evaluation_mean <- predict(fit_nc_evaluation_mean, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_evaluation_mean - dados_wide[test,]$igp_parto_transformado)^2) # EQM

                       #### Last evaluation ####

fit_nc_last_evaluation <- 
  lm(igp_parto_transformado ~ nc_last_evaluation, data = nc_treino)

summary(fit_nc_last_evaluation)

pred_nc_last_evaluation <- predict(fit_nc_last_evaluation, 
                                   newdata = nc_teste, 
                                   type = c("response")
                                   )

mean((pred_nc_last_evaluation - dados_wide[test,]$igp_parto_transformado)^2) # EQM

##############################################################################
