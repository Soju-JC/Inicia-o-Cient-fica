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
  "esquisse", 
  "viridis",
  "cowplot",
  "tidyr", 
  "reshape2", 
  "VIM", 
  "mice"
)

lapply(packages, loadlibrary)

########################## Treatment ############################
dados <- read_excel("dados_igs_completas_11avals.xlsx")
#View(dados)

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

#labels for some plots
axis_labels <- c(
  "sem24", 
  "sem25", 
  "sem26", 
  "sem27", 
  "sem28", 
  "sem29", 
  "sem30", 
  "sem31", 
  "sem32", 
  "sem33", 
  "sem34"
  )

axis_labels <- as.factor(axis_labels)

############# Exploratory analysis of functional predictors  ################

require(fda)

#Mean of cervical measurements: 
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[,3:13])
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
  lwd = 3,
  col="red"
)
axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
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
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda=lambdac)
contra_transpose <- t(num_contra_wide[,3:13])
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
axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
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
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[,3:13])
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
  ylim = c(0,10),
  axes = FALSE,
  xlab = as.expression(bquote(bold("Semana de gestação"))),
  ylab = as.expression(bquote(bold("Desvio padrão"))),
  cex.lab= 1.5,
  cex.axis= 1.5,
  type = "l",
  lwd = 3,
  col="red"
)
axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
axis(side = 2, at = 0:10, lty = 1, labels = 0:10)
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
  scale_y_continuous(breaks = 0:10, limits = c(0,10)) +
  theme_bw()

#Standard deviation of contraction measurements:
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda=lambdac)
contra_transpose <- t(num_contra_wide[,3:13])
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
axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
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
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, m_colo_basis)
colofdPar <- fdPar(m_colo_basis, lambda=lambdac)
colo_transpose <- t(medida_colo_wide[,3:13])
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

axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
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

dev.new(width=6, height=3)
ggplot(data=df) +
  geom_line(aes(x=x, y=mean, group = 1)) +
  geom_line(aes(x=x, y=mean.1, group = 1, colour = "red"), linetype = "dashed") +
  geom_line(aes(x=x, y=mean.2, group = 1, colour = "red"), linetype = "dashed") +
  labs(x="Semana de gestação", y="Medida do colo") +
  scale_y_continuous(breaks = 20:40, limits = c(20,40)) +
  theme_bw() +
  theme(legend.position="none")

#Mean of contraction measurements and 1/2 +- deviation: 
avaliacoes <- 1:11
rng <- c(1, 11)
knots <- avaliacoes
norder <- 2 #11 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(
  rng, 
  nbasis,
  norder, 
  knots
)
lambdac <- df2lambda(avaliacoes, n_contra_basis)
contrafdPar <- fdPar(n_contra_basis, lambda=lambdac)
contra_transpose <- t(num_contra_wide[,3:13])
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
dev.new(width=6, height=3)
plot(
  contra_mean,
  xlab = "Semana de gestação", 
  ylab = "Número de contrações", 
  lwd=2, 
  axes = FALSE,
  ylim = c(0,5)
)

axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
axis(side = 2, at = 0:5, lty = 1, labels = 0:5)
box(col = "black")
lines(x1, col="red", lty = 2)
lines(x2, col="red", lty = 2)

#With ggplot2
df <- data.frame(
  x = axis_labels,
  mean = predict(contra_mean, newdata = axis_labels),
  desvio_mais = predict(x1, newdata = axis_labels),
  desvio_menos = predict(x2, newdata = axis_labels)
)

dev.new(width=6, height=3)
ggplot(data=df) +
  geom_line(aes(x=x, y=mean, group = 1)) +
  geom_line(aes(x=x, y=mean.1, group = 1, colour = "red"), linetype = "dashed") +
  geom_line(aes(x=x, y=mean.2, group = 1, colour = "red"), linetype = "dashed") +
  labs(x="Semana de gestação", y="Número de contrações") +
  scale_y_continuous(breaks = 0:5, limits = c(0,5)) +
  theme_bw() +
  theme(legend.position="none")

######## Profile plot #########

#Cervical measurements:
perfil_mcolo <- 
  ggplot(medida_colo_long,
         aes(x=ig_aval_sem, y=medida_colo_imp)) + 
  geom_line(aes(group=id), size=0.8) + 
  labs(y="Medida do colo uterino",
       x="Semana de gestação") +
  scale_x_continuous(breaks=seq(24,34,1)) +
  scale_y_continuous(breaks=seq(0,54,4)) +
  theme_bw()

dev.new(width=6, height=3)
perfil_mcolo

#Contraction measurements:
perfil_ncontra <- 
  ggplot(num_contra_long,
         aes(x=ig_aval_sem, y=num_contra_imp)) + 
  geom_line(aes(group=id), size=0.8) + 
  labs(y="Número de contrações",
       x="Semana de gestação") +
  scale_x_continuous(breaks=seq(24,34,1)) +
  scale_y_continuous(breaks=seq(0,20,2)) +
  theme_bw()

dev.new(width=6, height=3)
perfil_ncontra

######################## fda model fit ###########################

require(fda)

#Cervical measurements:
var_func <- t(medida_colo_wide[, 3:13])
igp_parto <- medida_colo_wide$igp_parto
smallbasis <- create.bspline.basis(c(0, 11), 11)
coloSmooth <- smooth.basis(
  seq(1, 11, 1),
  var_func,
  smallbasis
)
colofd <- coloSmooth$fd
cololist = vector("list", 2)
cololist[[1]] = rep(1, 263)
cololist[[2]] = colofd

conbasis = create.constant.basis(c(0, 11))
betabasis = create.bspline.basis(c(0, 11), 10)
betalist = vector("list", 2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(igp_parto, cololist, betalist)
betaestlist = fRegressList$betaestlist
colobetafd = betaestlist[[2]]$fd

#With plot()
dev.new(width=6, height=3)
plot(
  colobetafd, 
  xlab= "t",
  ylab= expression(paste(beta(t))), 
  xlim = c(1, 11), 
  axes = FALSE
)

axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
axis(side = 2, at = seq(-0.05, 0.2, 0.01), lty = 1, labels = seq(-0.05, 0.2, 0.01))
box(col = "black")

# #With ggplot2
# df <- data.frame(
#   x = axis_labels,
#   mean = predict(colobetafd, 
#                  newdata = axis_labels)
# )
# 
# dev.new(width=6, height=3)
# ggplot(data=df, aes(x=x, y=mean, group = 1)) +
#   geom_smooth() +
#   labs(x="t", y= expression(paste(beta(t)))) +
#   scale_y_continuous() +
#   theme_bw()


#Contraction measurements:
var_func <- t(num_contra_wide[, 3:13])
igp_parto <- num_contra_wide$igp_parto
smallbasis <- create.bspline.basis(c(0, 11), 11)
contra_novo2Smooth <- smooth.basis(
  seq(1, 11, 1),
  var_func,
  smallbasis
)
contra_novo2fd <- contra_novo2Smooth$fd
contra_novo2list = vector("list", 2)
contra_novo2list[[1]] = rep(1, 263)
contra_novo2list[[2]] = contra_novo2fd

conbasis = create.constant.basis(c(0, 11))
betabasis = create.bspline.basis(c(0, 11), 10)
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

#With plot()
dev.new(width=6, height=3)
plot(
  contra_novo2betafd, 
  xlab= "t",
  ylab= expression(paste(beta(t))), 
  xlim = c(1, 11), 
  axes = FALSE
)

axis(side = 1, at = 1:11, lty = 1, labels = axis_labels)
axis(side = 2, at = seq(-0.2, 0.2, 0.01), lty = 1, labels = seq(-0.2, 0.2, 0.01))
box(col = "black")

######################## refund model fit ########################

require(refund)

#My function for the following single plots in ggplot2

ggplot_ajuste <- function(fitJJ){ 
  
  t <- axis_labels
  coefs = data.frame(
    grid = t,
    Spline = coef(fitJJ)$value
  )
  
  coefs.m = reshape2::melt(coefs, id = "grid")
  colnames(coefs.m) = c("Semana de gestação", "Spline", "Beta")
  
  dev.new(width=6, height=3)
  ggplot(coefs.m,
         aes(x = `Semana de gestação`, y = Beta, color = Spline, group = Spline),
         width=12,
         height=6) +
    geom_path() +
    theme_bw() +
    theme(legend.position = "none")
}

###Fit with 1 functional predictor using pfr().

##Cervical measurements: 
Y <- medida_colo_wide$igp_parto
X <- medida_colo_wide[, 3:13]
X <- as.matrix(X)
t <- seq(1, 11,length = 11)

#fpc() - Functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 6))

# Plot
dev.new(width=6, height=3)
plot(
  fitJJ,
  ylab= expression(paste(beta(t))), 
  xlab= "t", 
  xlim = c(0,1), 
  xaxt = "n",
  cex.lab= 1,
  cex.axis= 1
)

axis(side = 1, at = seq(0, 1, 0.25), lty = 1, labels = seq(0, 1, 0.25))

# ggplot2
ggplot_ajuste(fitJJ)

#lf() -  Regression splines:
Y <- medida_colo_wide$igp_parto
X <- medida_colo_wide[, 3:13]
X <- as.matrix(X)
t <- seq(1, 11,length = 11)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 6, fx = T))

# Plot
dev.new(width=6, height=3)
plot(
  fitJJ,
  ylab= expression(paste(beta(t))), 
  xlab= "t", 
  xlim = c(0,1), 
  xaxt = "n",
  cex.lab= 1,
  cex.axis= 1
)

axis(side = 1, at = seq(0, 1, 0.25), lty = 1, labels = seq(0, 1, 0.25))

# ggplot2
ggplot_ajuste(fitJJ)

# #fpc() And lf() plotted:
# Y <- medida_colo_wide$igp_parto
# X <- medida_colo_wide[, 3:13]
# X <- as.matrix(X)
# t <- seq(1, 11,length = 11)
# 
# fitJJ1 = pfr(Y ~ fpc(X, k = 6))
# fitJJ2 = pfr(Y ~ lf(X, bs = "ps", k = 6, fx = T))
# 
# coefs = data.frame(grid = t,
#                    FPC = coef(fitJJ1)$value,
#                    Spline = coef(fitJJ2)$value)
# 
# coefs.m = reshape2::melt(coefs, id = "grid")
# colnames(coefs.m) = c("grid", "Method", "Value")
# 
# dev.new(width=6,height=3)
# ggplot(coefs.m, 
#        aes(x = grid, y = Value, color = Method, group
#            = Method),width=12,height=6) +
#   geom_path() +
#   theme_bw()

##Contraction measurements:
require(refund)

Y <- num_contra_wide$igp_parto
X <- num_contra_wide[, 3:13]
X <- as.matrix(X)
t <- seq(1, 11,length = 11)

#fpc() - functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 7))

# Plot
dev.new(width=6, height=3)
plot(
  fitJJ,
  ylab= expression(paste(beta(t))), 
  xlab= "t", 
  xlim = c(0,1), 
  xaxt = "n",
  cex.lab= 1,
  cex.axis= 1
)

axis(side = 1, at = seq(0, 1, 0.25), lty = 1, labels = seq(0, 1, 0.25))

# ggplot2
ggplot_ajuste(fitJJ)

#lf() -  Regression splines:
Y <- num_contra_wide$igp_parto
X <- num_contra_wide[, 3:13]
X <- as.matrix(X)
t <- seq(1, 11,length = 11)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 8, fx = T))

# Plot
dev.new(width=6, height=3)
plot(
  fitJJ,
  ylab= expression(paste(beta(t))), 
  xlab= "t", 
  xlim = c(0,1), 
  xaxt = "n",
  cex.lab= 1,
  cex.axis= 1
)

axis(side = 1, at = seq(0, 1, 0.25), lty = 1, labels = seq(0, 1, 0.25))

# ggplot2
ggplot_ajuste(fitJJ)

# #fpc() And lf() plotted:
# Y <- num_contra_wide$igp_parto
# X <- num_contra_wide[, 3:13]
# X <- as.matrix(X)
# t <- seq(1, 11,length = 11)
# 
# fitJJ1 = pfr(Y ~ fpc(X, k = 7))
# fitJJ2 = pfr(Y ~ lf(X, bs = "ps", k = 8, fx = T))
# 
# coefs = data.frame(grid = t,
#                    FPC = coef(fitJJ1)$value,
#                    Spline = coef(fitJJ2)$value)
# 
# coefs.m = reshape2::melt(coefs, id = "grid")
# colnames(coefs.m) = c("grid", "Method", "Value")
# 
# dev.new(width=6,height=3)
# ggplot(coefs.m, 
#        aes(x = grid, y = Value, color = Method, group
#            = Method),width=12,height=6) +
#   geom_path() +
#   theme_bw()

####### Tests for refund using 2 functional predictors and pfr_old() ########

##EXAMPLE FROM pfr_old() DOCUMENTATION FOR DTI2 DATA:

#Load and reassign the data:
require(refund)
data(DTI2)
Y  <- DTI2$pasat ## PASAT outcome.
id <- DTI2$id    ## subject id.
W1 <- DTI2$cca   ## Corpus Callosum.
W2 <- DTI2$rcst  ## Right corticospinal.
V  <- DTI2$visit ## visit.

#Prep scalar covariate:
visit.1.rest <- matrix(as.numeric(V > 1), ncol=1)
covar.in <- visit.1.rest 


#Note there is missingness in the functional predictors:
apply(is.na(W1), 2, mean)
apply(is.na(W2), 2, mean)

#Fit two univariate models:
pfr.obj.t1 <- pfr_old(Y = Y, covariates=covar.in, funcs = list(W1),subj = id, kz = 10, kb = 50)
pfr.obj.t2 <- pfr(Y = Y, covariates=covar.in, funcs = list(W2),
                  subj = id, kz = 10, kb = 50)

#One model with two functional predictors using "smooth.face"
#for smoothing predictors:
pfr.obj.t3 <- pfr(Y = Y, covariates=covar.in, funcs = list(W1, W2), 
                  subj = id, kz = 10, kb = 50, nbasis=35,smooth.option="fpca.face")



####### MY TESTS... 

#Obs1: The 'system is computationally singular' error occurs when using MICE with 
#method = pmm as it generates a non-invertible matrix. The problem is solved
#by using a non-stochastic method like method = cart. 

#Obs2: I had some problems taking variables from differents objects, so i am going
#to create a dataframe with the variables that i will use and the functional
#predictors as a AsIs objects inside before the model fit.

#Wide-format:
#Happens -> error:(Model has more coefficients than data) with pfr_old().
Y <- medida_colo_wide$igp_parto
Y <- as.matrix(Y)
X1 <- medida_colo_wide[, 3:13]
X1 <- as.matrix(X1)
X1 <- I(X1)
X2 <- num_contra_wide[, 3:13]
X2 <- as.matrix(X2)
X2 <- I(X2)
id <- medida_colo_wide$id
id <- as.matrix(id)

data_asis1 <- data.frame(
  igp_parto=Y,
  id=id,
  colo_perfis=X1,
  contra_perfis=X2
)

rm("Y")
rm("X1")
rm("X2")
rm("id")
Y <- data_asis1$igp_parto
X1 <- data_asis1$colo_perfis
X2 <- data_asis1$contra_perfis
id <- data_asis1$id

#### Using pfr(). Is this working ? 

### 2 functional predictors:

## prf () is taking into account the order in which the functional 
## predictors are placed, giving priority to the behavior of the first.

## X1 -> X2

# 1°
fitJJ = pfr(Y ~ fpc(X1, k = 6) + lf(X2, k = 8))
ggplot_ajuste(fitJJ)

# 2°
fitJJ = pfr(Y ~ lf(X1, bs = "ps", k = 6, fx = T) + fpc(X2, k = 7))
ggplot_ajuste(fitJJ)

# 3°
fitJJ = pfr(Y ~ lf(X1, bs = "ps", k = 6, fx = T) + lf(X2, bs = "ps", k = 8, fx = T))
ggplot_ajuste(fitJJ)

## X2 -> X1

# 1°
fitJJ = pfr(Y ~ fpc(X2, k = 7) + lf(X1, k = 6))
ggplot_ajuste(fitJJ)

# 2°
fitJJ = pfr(Y ~ lf(X2, bs = "ps", k = 8, fx = T) + fpc(X1, k = 6))
ggplot_ajuste(fitJJ)

# 3°
fitJJ = pfr(Y ~ lf(X2, bs = "ps", k = 8, fx = T) + lf(X1, bs = "ps", k = 6, fx = T))
ggplot_ajuste(fitJJ)

#### Using pfr_old()

#1 functional predictor:
fitJJ = pfr_old(
  Y = Y, 
  funcs = X1, 
  subj = id, 
  kz = 9,
  kb = 30, 
  nbasis = 10
)

#2 functional predictors:
fitJJ = pfr_old(
  Y = Y, 
  funcs = list(X1,X2), 
  subj = id, 
  kz = 4,
  kb = 10, 
  nbasis = 10
)

#Long-format:
#Happens -> error:(A term has fewer unique covariate combinations than 
#specified maximum degrees of freedom).
Y <- medida_colo_long$igp_parto
Y <- as.matrix(Y)
X1 <- medida_colo_long$medida_colo_imp
X1 <- as.matrix(X1)
X1 <- I(X1)
X2 <- num_contra_long$num_contra_imp
X2 <- as.matrix(X2)
X2 <- I(X2)
id <- medida_colo_long$id
id <- as.matrix(id)

data_asis2 <- data.frame(
  igp_parto=Y,
  id=id,
  colo_perfis=X1,
  contra_perfis=X2
)

rm("Y")
rm("X1")
rm("X2")
rm("id")
Y <- data_asis2$igp_parto
X1 <- data_asis2$colo_perfis
X2 <- data_asis2$contra_perfis
id <- data_asis2$id

#1 functional predictor:
fitJJ = pfr_old(
  Y = Y, 
  funcs = X1, 
  subj = id, 
  kz = 5, 
  kb = 5, 
  nbasis = 9 
)

#2 functional predictors:
fitJJ = pfr_old(
  Y = Y, 
  funcs = list(X1,X2), 
  subj = id, 
  kz = 10,
  kb = 30, 
  nbasis = 11, 
  smooth.option="fpca.sc"
)
