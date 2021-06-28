#Study of regression models with scalar response variable and functional 
#predictors using the fda and refund packages. The data was collected
#from 336 preagnant woman during the prenatal evaluations for a study
#from the Hospital das Clínicas da Faculdade de Medicina da Universidade 
#de Sao Paulo (FMUSP).

#Script using data with the time grid of 11 evaluations of the functional 
#predictors, it takes from week 24 to 34 week by week and was made imputation
#of the missing data with the MICE method.

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
  "mice"
  )

lapply(packages, loadlibrary)

#labels for some plots
axis_labels <- c(
  "sem24_26", 
  "sem27_28", 
  "sem29_30", 
  "sem31_32", 
  "sem33_34")

axis_labels <- as.factor(axis_labels)

########################## Treatment ############################
dados <- read_excel("dados_igs_completas_5avals_1imput.xlsx")
# View(dados)

medida_colo_long <- dados[,c("id", "igp_parto", "ig_aval_sem", "medida_colo_imp")]

medida_colo_wide <- pivot_wider(
  medida_colo_long,
  names_from = ig_aval_sem,
  values_from = medida_colo_imp
)

num_contra_long <- dados[,c("id", "igp_parto", "ig_aval_sem", "num_contra_imp")]

num_contra_wide <- pivot_wider(
  num_contra_long,
  names_from = ig_aval_sem,
  values_from = num_contra_imp
)

############# Exploratory analysis of functional predictors  ################

require(fda)

#Mean of cervical measurements: 
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[, 3:7])
colofd <- smooth.basis(
  avaliacoes,
  colo_transpose, 
  colofdPar
)$fd

colo_mean <- mean.fd(colofd)

#With plot
dev.new(width=6, height=3)
plot(
  colo_mean,
  ylim = c(24,40),
  axes = FALSE,
  xlab = as.expression(bquote(bold("Semana de gestação"))),
  ylab = as.expression(bquote(bold("Medida do colo (média)"))),
  cex.lab= 1.5,
  cex.axis= 1.5,
  type = "l",
  lwd = 3
)
axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 24:40, lty = 1, labels = 24:40)
box(col = "black")

#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(colo_mean, 
                 newdata = axis_labels)
)

dev.new(width=6, height=3)
ggplot(data=df, aes(x=x, y=mean, group = 1)) +
  geom_line() +
  labs(x="Semana de gestação", y="Medida do colo (média)") +
  scale_y_continuous(breaks = 24:40, limits = c(24,40)) +
  theme_bw()

#Mean of contraction measurements: 
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda=lambdac)
contra_transpose <- t(num_contra_wide[, 3:7])
contrafd <- smooth.basis(
  avaliacoes,
  contra_transpose,
  contrafdPar
)$fd

contra_mean <- mean.fd(contrafd)

#With plot
dev.new(width=6, height=3)
plot(
  contra_mean,
  ylim = c(0,5),
  axes = FALSE,
  xlab = as.expression(bquote(bold("Semana de gestação"))),
  ylab = as.expression(bquote(bold("Número de contrações (média)"))),
  cex.lab= 1.5,
  cex.axis= 1.5,
  type = "l",
  lwd = 3
)
axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 0:5, lty = 1, labels = 0:5)
box(col = "black")

#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(contra_mean, 
                 newdata = axis_labels)
)

dev.new(width=6, height=3)
ggplot(data=df, aes(x=x, y=mean, group = 1)) +
  geom_line() +
  labs(x="Semana de gestação", y="Número de contrações (média)") +
  scale_y_continuous(breaks = 0:5, limits = c(0,5)) +
  theme_bw()

#Standard deviation of cervical measurements:
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[, 3:7])
colofd <- smooth.basis(
  avaliacoes,
  colo_transpose, 
  colofdPar
)$fd

colo_sd <- sd.fd(colofd)

#With plot
dev.new(width=6, height=3)
plot(
  colo_sd,
  ylim = c(5,10),
  axes = FALSE,
  xlab = as.expression(bquote(bold("Semana de gestação"))),
  ylab = as.expression(bquote(bold("Desvio padrão"))),
  cex.lab= 1.5,
  cex.axis= 1.5,
  type = "l",
  lwd = 3,
  col="red"
)
axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 5:10, lty = 1, labels = 5:10)
box(col = "black")


#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(colo_sd, 
                 newdata = axis_labels)
)

dev.new(width=6, height=3)
ggplot(data=df, aes(x=x, y=mean, group = 1)) +
  geom_line() +
  labs(x="Semana de gestação", y="Desvio padrão") +
  scale_y_continuous(breaks = 5:10, limits = c(5,10)) +
  theme_bw()

#Standard deviation of contraction measurements:
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots 
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda=lambdac)
contra_transpose <- t(num_contra_wide[, 3:7])
contrafd <- smooth.basis(
  avaliacoes,
  contra_transpose,
  contrafdPar
)$fd

contra_sd <- sd.fd(contrafd)

#With plot
dev.new(width=6, height=3)
plot(
  contra_sd,
  ylim = c(0,5),
  axes = FALSE,
  xlab = as.expression(bquote(bold("Semana de gestação"))),
  ylab = as.expression(bquote(bold("Desvio padrão"))),
  cex.lab= 1.5,
  cex.axis= 1.5,
  type = "l",
  lwd = 3,
  col="red"
)
axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 0:5, lty = 1, labels = 0:5)
box(col = "black")


#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(contra_sd, 
                 newdata = axis_labels)
)

dev.new(width=6, height=3)
ggplot(data=df, aes(x=x, y=mean, group = 1)) +
  geom_line() +
  labs(x="Semana de gestação", y="Desvio padrão") +
  scale_y_continuous(breaks = 0:5, limits = c(0,5)) +
  theme_bw()


#Mean of cervical measurements and 1/2 +- deviation:
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[, 3:7])
colofd <- smooth.basis(
  avaliacoes,
  colo_transpose, 
  colofdPar
)$fd

colo_mean <- mean.fd(colofd)
colo_sd <- sd.fd(colofd)

x1 = colo_mean+(1/2)*colo_sd
x2 = colo_mean-(1/2)*colo_sd

#With plot
dev.new(width=6, height=3)
plot(
  colo_mean,
  xlab = "Semana de gestação", 
  ylab = "Medida do colo", 
  lwd=2, 
  axes = FALSE,
  ylim = c(20,40)
)

axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 20:40, lty = 1, labels = 20:40)
box(col = "black")
lines(x1, col="red", lty = 2)
lines(x2, col="red", lty = 2)

#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(colo_mean, newdata = axis_labels),
  desvio_mais = predict(x1, newdata = axis_labels),
  desvio_menos = predict(x2, newdata = axis_labels)
)

dev.new(width = 6, height = 3)
ggplot(data = df) +
  geom_line(aes(x = x, y = mean, group = 1)) +
  geom_line(aes(x = x, y = mean.1, group = 1, colour = "red"), linetype = "dashed") +
  geom_line(aes(x = x, y = mean.2, group = 1, colour = "red"), linetype = "dashed") +
  labs(x = "Semana de gestação", y = "Medida do colo") +
  scale_y_continuous(breaks = 20:40, limits = c(20, 40)) +
  theme_bw() +
  theme(legend.position = "none")

#Mean of contraction measurements and 1/2 +- deviation: 
avaliacoes <- 1:5
rng <- c(1, 5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda = lambdac)
contra_transpose <- t(num_contra_wide[, 3:7])
contrafd <- smooth.basis(
  avaliacoes,
  contra_transpose,
  contrafdPar
)$fd

contra_mean <- mean.fd(contrafd)
contra_sd <- sd.fd(contrafd)

x1 = contra_mean+(1/2)*contra_sd
x2 = contra_mean-(1/2)*contra_sd

#With plot
dev.new(width = 6, height = 3)
plot(
  contra_mean,
  xlab = "Semana de gestação", 
  ylab = "Número de contrações", 
  lwd = 2, 
  axes = FALSE,
  ylim = c(0, 5)
)

axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = 0:5, lty = 1, labels = 0:5)
box(col = "black")
lines(x1, col = "red", lty = 2)
lines(x2, col = "red", lty = 2)

#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(contra_mean, newdata = axis_labels),
  desvio_mais = predict(x1, newdata = axis_labels),
  desvio_menos = predict(x2, newdata = axis_labels)
)

dev.new(width = 6, height = 3)
ggplot(data=df) +
  geom_line(aes(x = x, y = mean, group = 1)) +
  geom_line(aes(x = x, y = mean.1, group = 1, colour = "red"), linetype = "dashed") +
  geom_line(aes(x = x, y = mean.2, group = 1, colour = "red"), linetype = "dashed") +
  labs(x = "Semana de gestação", y = "Número de contrações") +
  scale_y_continuous(breaks = 0:5, limits = c(0, 5)) +
  theme_bw() +
  theme(legend.position = "none")

######## Profile plot #########

#Cervical measurements:
perfil_mcolo <- 
  ggplot(medida_colo_long,
         aes(x = ig_aval_sem, y = medida_colo_imp)) + 
  geom_line(aes(group = id),size = 0.8) + 
  labs(y="Medida do colo uterino",
       x="Semana de gestação") +
  # scale_x_continuous(breaks=seq(24,34,1)) +
  scale_y_continuous(breaks=seq(0, 54, 4)) +
  theme_bw()

dev.new(width = 6, height = 3)
perfil_mcolo

#Contraction measurements:
perfil_ncontra <- 
  ggplot(num_contra_long,
         aes(x = ig_aval_sem,y = num_contra_imp)) + 
  geom_line(aes(group=id),size = 0.8) + 
  labs(y = "Número de contrações",
       x = "Semana de gestação") +
  # scale_x_continuous(breaks=seq(24,34,1)) +
  scale_y_continuous(breaks=seq(0, 20, 2)) +
  theme_bw()

dev.new(width = 6, height = 3)
perfil_ncontra

######################## fda model fit ###########################

require(fda)

#Cervical measurements:
var_func <- t(medida_colo_wide[, 3:7])
igp_parto <- medida_colo_wide$igp_parto
smallbasis <- create.bspline.basis(c(0, 5), 5)
coloSmooth <- smooth.basis(
  seq(1, 5, 1),
  var_func,
  smallbasis
)
colofd <- coloSmooth$fd
cololist = vector("list", 2)
cololist[[1]] = rep(1, 263)
cololist[[2]] = colofd

conbasis = create.constant.basis(c(0, 5))
betabasis = create.bspline.basis(c(0, 5), 5)
betalist = vector("list", 2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(igp_parto, cololist, betalist)
betaestlist = fRegressList$betaestlist
colobetafd = betaestlist[[2]]$fd

dev.new(width = 6, height = 3)
plot(
  colobetafd, 
  xlab = "Semana de gestação",
  ylab = "Beta para a medida do colo", 
  xlim = c(1, 5), 
  axes = FALSE)

axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = seq(-0.02, 0.05, 0.01), lty = 1, labels = seq(-0.02, 0.05, 0.01))
box(col = "black")

#With ggplot2
# df <- data.frame(
#   x = axis_labels,
#   mean = predict(colobetafd, 
#                  newdata = axis_labels)
# )
# 
# dev.new(width=6,height=3)
# ggplot(data=df, aes(x=x, y=mean, group = 1)) +
#   geom_smooth() +
#   labs(x="t", y= expression(paste(beta(t)))) +
#   scale_y_continuous() +
#   theme_bw()

#Contraction measurements:
var_func <- t(num_contra_wide[, 3:7])
igp_parto <- num_contra_wide$igp_parto
smallbasis <- create.bspline.basis(c(0, 5), 5)
contra_novo2Smooth <- smooth.basis(
  seq(1, 5, 1),
  var_func,
  smallbasis
)
contra_novo2fd <- contra_novo2Smooth$fd
contra_novo2list = vector("list", 2)
contra_novo2list[[1]] = rep(1, 263)
contra_novo2list[[2]] = contra_novo2fd

conbasis = create.constant.basis(c(0, 5))
betabasis = create.bspline.basis(c(0, 5), 5)
betalist = vector("list", 2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(
  igp_parto,
  contra_novo2list,
  betalist
)

betaestlist = fRegressList$betaestlist
contra_novo2betafd = betaestlist[[2]]$fd

dev.new(width = 6, height = 3)
plot(
  contra_novo2betafd, 
  xlab = "Semana de gestação",
  ylab = "Beta para o número de contrações", 
  xlim = c(1, 5), 
  axes = FALSE)

axis(side = 1, at = 1:5, lty = 1, labels = axis_labels)
axis(side = 2, at = seq(-0.4, 0.2, 0.1), lty = 1, labels = seq(-0.4, 0.2, 0.1))
box(col = "black")

######################## refund model fit ########################
require(refund)

#My functions for the following single plots

ggplot_ajuste <- function(fitJJ){ 
  
  t <- axis_labels
  coefs = data.frame(
    grid = t,
    Spline = coef(fitJJ)$value
  )

  coefs.m = reshape2::melt(coefs, id = "grid")
  colnames(coefs.m) = c("Semana de gestação", "Spline", "Beta")

  dev.new(width = 6, height = 3)
  ggplot(coefs.m,
         aes(x = `Semana de gestação`, y = Beta, color = Spline, group = Spline),
         width = 12,
         height = 6) +
    geom_path() +
    theme_bw() +
    theme(legend.position = "none")
}

plot_ajuste <- function(fitJJ){ 
#dev.new(width = 6, height = 3)
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

###Fit with 1 functional predictor using pfr().

##Cervical measurements: 
Y <- medida_colo_wide$igp_parto
X <- medida_colo_wide[, 3:7]
X <- as.matrix(X)

#fpc() - Functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 4))

# Plot
plot_ajuste(fitJJ)

# ggplot2
ggplot_ajuste(fitJJ)

#lf() -  Regression splines:
Y <- medida_colo_wide$igp_parto
X <- medida_colo_wide[, 3:7]
X <- as.matrix(X)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 4, fx = T))

# Plot
plot_ajuste(fitJJ)

# ggplot2
ggplot_ajuste(fitJJ)


##Contraction measurements:
require(refund)

Y <- num_contra_wide$igp_parto
X <- num_contra_wide[, 3:7]
X <- as.matrix(X)

#fpc() - functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 5))

# Plot
plot_ajuste(fitJJ)

# ggplot2
ggplot_ajuste(fitJJ)

#lf() -  Regression splines:
Y <- num_contra_wide$igp_parto
X <- num_contra_wide[, 3:7]
X <- as.matrix(X)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 5, fx = T))

# Plot
plot_ajuste(fitJJ)

# ggplot2
ggplot_ajuste(fitJJ)

                      # DAQUI PRA BAIXO 
          #FIT RODA MAS O gráfico do FPC TA ESTRANHO 
            #Obs: rodar plot_ajuste na linha 570 

####### refund using 2 functional predictors with pfr()########

Y <- medida_colo_wide$igp_parto
X1 <- medida_colo_wide[, 3:7]
X1 <- as.matrix(X1)
X2 <- num_contra_wide[, 3:7]
X2 <- as.matrix(X2)


# Models applying lf() method in cervical measurements and 
#fpc() method in contractions measuments. Both are using
#the same spline basis with the same dimension in each model:

#Obs: fit_ps_4 for example, means fit for model with both 
#functional predictors where ps is bs = ps and 4 is k = 4.

# ps

# Both k = 4
fit_ps_4 = pfr(Y ~ lf(X1, bs = "ps", k = 4, fx = TRUE) +
                  fpc(X2, bs = "ps", k = 4)
               )

plot_ajuste(fit_ps_4)

summary(fit_ps_4)
AIC(fit_ps_4)
coef(fit_ps_4, select = 1)
coef(fit_ps_4, select = 2)

# Both k = 5
fit_ps_5 = pfr(Y ~ lf(X1, bs = "ps", k = 5, fx = TRUE) +
                  fpc(X2, bs = "ps", k = 5)  
               )

plot_ajuste(fit_ps_5)

summary(fit_ps_5)
AIC(fit_ps_5)
coef(fit_ps_5, select = 1)
coef(fit_ps_5, select = 2)

# tp

# Both k = 3
fit_tp_3 = pfr(Y ~ lf(X1, bs = "tp", k = 3, fx = TRUE) +
                  fpc(X2, bs = "tp", k = 3)  
               )

plot_ajuste(fit_tp_3)

summary(fit_tp_3)
AIC(fit_tp_3)
coef(fit_tp_3, select = 1)
coef(fit_tp_3, select = 2)

# Both k = 4
fit_tp_4 = pfr(Y ~ lf(X1, bs = "tp", k = 4, fx = TRUE) +
                  fpc(X2, bs = "tp", k = 4)  
               )

plot_ajuste(fit_tp_4)

summary(fit_tp_4)
AIC(fit_tp_4)
coef(fit_tp_4, select = 1)
coef(fit_tp_4, select = 2)

# Both k = 5
fit_tp_5 = pfr(Y ~ lf(X1, bs = "tp", k = 5, fx = TRUE) +
                  fpc(X2, bs = "tp", k = 5)  
               )

plot_ajuste(fit_tp_5)

summary(fit_tp_5)
AIC(fit_tp_5)
coef(fit_tp_5, select = 1)
coef(fit_tp_5, select = 2)


# cr

# Both k = 3
fit_cr_3 = pfr(Y ~ lf(X1, bs = "cr", k = 3, fx = TRUE) +
                  fpc(X2, bs = "cr", k = 3)  
               )

plot_ajuste(fit_cr_3)

summary(fit_cr_3)
AIC(fit_cr_3)
coef(fit_cr_3, select = 1)
coef(fit_cr_3, select = 2)

# Both k = 4
fit_cr_4 = pfr(Y ~ lf(X1, bs = "cr", k = 4, fx = TRUE) +
                  fpc(X2, bs = "cr", k = 4)  
               )

plot_ajuste(fit_cr_4)

summary(fit_cr_4)
AIC(fit_cr_4)
coef(fit_cr_4, select = 1)
coef(fit_cr_4, select = 2)

# Both k = 5
fit_cr_5 = pfr(Y ~ lf(X1, bs = "cr", k = 5, fx = TRUE) +
                  fpc(X2, bs = "cr", k = 5)  
               )

plot_ajuste(fit_cr_5)

summary(fit_cr_5)
AIC(fit_cr_5)
coef(fit_cr_5, select = 1)
coef(fit_cr_5, select = 2)

# Models applying fpc() method in cervical measurements and 
#lf() method in contractions mesuments. Both are using
#the same spline basis with the same dimension in each model:

# ps

# Both k = 4
fit_ps_4 = pfr(Y ~ fpc(X1, bs = "ps", k = 4) +
                    lf(X2, bs = "ps", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_ps_4)

summary(fit_ps_4)
AIC(fit_ps_4)
coef(fit_ps_4, select = 1)
coef(fit_ps_4, select = 2)

# Both k = 5
fit_ps_5 = pfr(Y ~ fpc(X1, bs = "ps", k = 5) +
                    lf(X2, bs = "ps", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_ps_5)

summary(fit_ps_5)
AIC(fit_ps_5)
coef(fit_ps_5, select = 1)
coef(fit_ps_5, select = 2)

# tp

# Both k = 3
fit_tp_3 = pfr(Y ~ fpc(X1, bs = "tp", k = 3) +
                    lf(X2, bs = "tp", k = 3, fx = TRUE) 
               )

plot_ajuste(fit_tp_3)

summary(fit_tp_3)
AIC(fit_tp_3)
coef(fit_tp_3, select = 1)
coef(fit_tp_3, select = 2)

# Both k = 4
fit_tp_4 = pfr(Y ~ fpc(X1, bs = "tp", k = 4) +
                    lf(X2, bs = "tp", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_tp_4)

summary(fit_tp_4)
AIC(fit_tp_4)
coef(fit_tp_4, select = 1)
coef(fit_tp_4, select = 2)

# Both k = 5
fit_tp_5 = pfr(Y ~ fpc(X1, bs = "tp", k = 5) +
                    lf(X2, bs = "tp", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_tp_5)

summary(fit_tp_5)
AIC(fit_tp_5)
coef(fit_tp_5, select = 1)
coef(fit_tp_5, select = 2)


# cr

# Both k = 3
fit_cr_3 = pfr(Y ~ fpc(X1, bs = "cr", k = 3) +
                    lf(X2, bs = "cr", k = 3, fx = TRUE) 
               )

plot_ajuste(fit_cr_3)

summary(fit_cr_3)
AIC(fit_cr_3)
coef(fit_cr_3, select = 1)
coef(fit_cr_3, select = 2)

# Both k = 4
fit_cr_4 = pfr(Y ~ fpc(X1, bs = "cr", k = 4) +
                    lf(X2, bs = "cr", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_cr_4)

summary(fit_cr_4)
AIC(fit_cr_4)
coef(fit_cr_4, select = 1)
coef(fit_cr_4, select = 2)

# Both k = 5
fit_cr_5 = pfr(Y ~ fpc(X1, bs = "cr", k = 5) +
                    lf(X2, bs = "cr", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_cr_5)

summary(fit_cr_5)
AIC(fit_cr_5)
coef(fit_cr_5, select = 1)
coef(fit_cr_5, select = 2)

# Models applying lf() method on both functional predictors . Both
#are using the same spline basis with the same dimension in each model:

# ps

# Both k = 4
fit_ps_4 = pfr(Y ~ lf(X1, bs = "ps", k = 4, fx = TRUE) +
                   lf(X2, bs = "ps", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_ps_4)

summary(fit_ps_4)
AIC(fit_ps_4)
coef(fit_ps_4, select = 1)
coef(fit_ps_4, select = 2)

# Both k = 5
fit_ps_5 = pfr(Y ~ lf(X1, bs = "ps", k = 5, fx = TRUE) +
                   lf(X2, bs = "ps", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_ps_5)

summary(fit_ps_5)
AIC(fit_ps_5)
coef(fit_ps_5, select = 1)
coef(fit_ps_5, select = 2)

# tp

# Both k = 3
fit_tp_3 = pfr(Y ~ lf(X1, bs = "tp", k = 3, fx = TRUE) +
                   lf(X2, bs = "tp", k = 3, fx = TRUE) 
               )

plot_ajuste(fit_tp_3)

summary(fit_tp_3)
AIC(fit_tp_3)
coef(fit_tp_3, select = 1)
coef(fit_tp_3, select = 2)

# Both k = 4
fit_tp_4 = pfr(Y ~ lf(X1, bs = "tp", k = 4, fx = TRUE) +
                   lf(X2, bs = "tp", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_tp_4)

summary(fit_tp_4)
AIC(fit_tp_4)
coef(fit_tp_4, select = 1)
coef(fit_tp_4, select = 2)

# Both k = 5
fit_tp_5 = pfr(Y ~ lf(X1, bs = "tp", k = 5, fx = TRUE) +
                   lf(X2, bs = "tp", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_tp_5)

summary(fit_tp_5)
AIC(fit_tp_5)
coef(fit_tp_5, select = 1)
coef(fit_tp_5, select = 2)


# cr

# Both k = 3
fit_cr_3 = pfr(Y ~ lf(X1, bs = "cr", k = 3, fx = TRUE) +
                   lf(X2, bs = "cr", k = 3, fx = TRUE) 
               )

plot_ajuste(fit_cr_3)

summary(fit_cr_3)
AIC(fit_cr_3)
coef(fit_cr_3, select = 1)
coef(fit_cr_3, select = 2)

# Both k = 4
fit_cr_4 = pfr(Y ~ lf(X1, bs = "cr", k = 4, fx = TRUE) +
                   lf(X2, bs = "cr", k = 4, fx = TRUE) 
               )

plot_ajuste(fit_cr_4)

summary(fit_cr_4)
AIC(fit_cr_4)
coef(fit_cr_4, select = 1)
coef(fit_cr_4, select = 2)

# Both k = 5
fit_cr_5 = pfr(Y ~ lf(X1, bs = "cr", k = 5, fx = TRUE) +
                   lf(X2, bs = "cr", k = 5, fx = TRUE) 
               )

plot_ajuste(fit_cr_5)

summary(fit_cr_5)
AIC(fit_cr_5)
coef(fit_cr_5, select = 1)
coef(fit_cr_5, select = 2)

