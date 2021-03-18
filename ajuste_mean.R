#Study of regression models with scalar response variable and functional 
#covariables using the fda and refund packages. The data was collected
#from 336 preagnant woman during the prenatal evaluations for a study
#from the Hospital das Clínicas da Faculdade de Medicina da Universidade 
#de Sao Paulo (FMUSP).

#Script using data with the time grid of 5 evaluations, it takes from 
#week 24 to 34 every two weeks of gestation and was made imputation of 
#the missing data using the mean imputation.

########################### Packages ##############################
rm(list = ls())
loadlibrary <- function(x){
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies = T)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

packages <- c("tidyverse","readxl","janitor",
              "skimr","lubridate","summarytools",
              "magrittr","knitr","esquisse", 
              "viridis",
              "cowplot", "tidyr","reshape2")
lapply(packages, loadlibrary)

########################## Treatment ############################
dados <- read_excel("Base_TocoColoPerinatal.xls")
dados <- clean_names(dados)
names(dados)
View(dados)

#Checks the existence of values that should not exist in the
#variables adjusted to that function: 
verifica_erro <- function(v){
  
  if(v %in% "ovulos"){
    any(as.data.frame(dados)$v != 1 & as.data.frame(dados)$v != 2)
  } else if(v %in% "corion"){
    any(as.data.frame(dados)$v != 1 & as.data.frame(dados)$v != 2)
  } else if(v != "ovulos"& v != "corion") 
    any(as.data.frame(dados)$v != 0 & as.data.frame(dados)$v != 1)  
}

list_var_names <- c("ovulos", "corion", "cor_branco", "ind_ap",
                    "hv_tabagismo", "hv_alcool", "hv_drogas")

sapply(list_var_names, verifica_erro)

#Cervical length = 0 now receives NA
dados$medida_colo[dados$medida_colo == 0] <- NA

#Factor variables:
dados$ovulos <- fct_recode(as.factor(dados$ovulos),
                           placebo = "1", progesterona = "2")
dados$corion <- fct_recode(as.factor(dados$corion),
                           "monocoriônica" = "1",
                           "dicoriônica" = "2")
dados[c("cor_branco",
        "ind_ap",
        "hv_tabagismo",
        "hv_alcool",
        "hv_drogas")] <- 
  lapply(dados[c("cor_branco",
                 "ind_ap",
                 "hv_tabagismo",
                 "hv_alcool",
                 "hv_drogas")], 
         factor,
         levels = c(0,1),
         labels = c("não","sim"))

#Truncated gestational age of the evaluation 
dados$ig_aval_sem <- trunc(dados$ig_aval)

#Number of observations:
n_obs <- sum(dados$indic_aval==1)

#Grid of weeks:
n_time <- max(dados$ig_aval_sem) - min(dados$ig_aval_sem) + 1
  
#Concatenate measurements of cervical length:
dados1 <- spread(data=dados, key = ig_aval_sem,value = medida_colo)

dadosa <- data.frame(id=unique(dados$id))
dadosa$s24 <- aggregate(dados1$"24",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s25 <- aggregate(dados1$"25",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s26 <- aggregate(dados1$"26",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s27 <- aggregate(dados1$"27",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s28 <- aggregate(dados1$"28",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s29 <- aggregate(dados1$"29",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s30 <- aggregate(dados1$"30",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s31 <- aggregate(dados1$"31",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s32 <- aggregate(dados1$"32",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s33 <- aggregate(dados1$"33",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$s34 <- aggregate(dados1$"34",
                        by=list(Category=dados1$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa$ig_parto <- aggregate(dados1$igp_parto,
                             by=list(Category=dados1$id),
                             FUN=function(x) mean(x, na.rm = TRUE))$x
dadosa[is.na(dadosa)] <- NA

#Concatenate every two weeks(as was observed in practice):
colo_aux <- matrix(0, nrow = dim(dadosa)[1], ncol = 8)
colo_aux[,1] <- dadosa[,1]
colo_aux[,2] <- apply(dadosa[ ,2:4], 1,
                      function(x) mean(x, na.rm=TRUE))
colo_aux[,3] <- apply(dadosa[ ,5:6], 1,
                      function(x) mean(x, na.rm=TRUE))
colo_aux[,4] <- apply(dadosa[ ,7:8], 1,
                      function(x) mean(x, na.rm=TRUE))
colo_aux[,5] <- apply(dadosa[ ,9:10], 1,
                      function(x) mean(x, na.rm=TRUE))
colo_aux[,6] <- apply(dadosa[ ,11:12], 1,
                      function(x) mean(x, na.rm=TRUE))
colo_aux[,7] <- apply(colo_aux[,2:6], 1,
                      function(x) sum(is.na(x)))
colo_aux[,8] <- dadosa$ig_parto

#Count of NA's: 
table(colo_aux[,7])

#Concatenate number of contractions:
dados2 <- spread(data=dados, key = ig_aval_sem,value = num_contra)

dadosb <- data.frame(id=unique(dados$id))
dadosb$s24 <- aggregate(dados2$"24",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s25 <- aggregate(dados2$"25",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s26 <- aggregate(dados2$"26",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s27 <- aggregate(dados2$"27",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s28 <- aggregate(dados2$"28",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s29 <- aggregate(dados2$"29",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s30 <- aggregate(dados2$"30",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s31 <- aggregate(dados2$"31",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s32 <- aggregate(dados2$"32",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s33 <- aggregate(dados2$"33",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$s34 <- aggregate(dados2$"34",
                        by=list(Category=dados2$id),
                        FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb$ig_parto <- aggregate(dados2$igp_parto,
                             by=list(Category=dados2$id),
                             FUN=function(x) mean(x, na.rm = TRUE))$x
dadosb[is.na(dadosb)] <- NA

#Concatenate every two weeks: 
contra_aux <- matrix(0, nrow = dim(dadosb)[1], ncol = 8)
contra_aux[,1] <- dadosb[,1]
contra_aux[,2] <- apply(dadosb[ ,2:4], 1,
                        function(x) mean(x, na.rm=TRUE))
contra_aux[,3] <- apply(dadosb[ ,5:6], 1,
                        function(x) mean(x, na.rm=TRUE))
contra_aux[,4] <- apply(dadosb[ ,7:8], 1,
                        function(x) mean(x, na.rm=TRUE))
contra_aux[,5] <- apply(dadosb[ ,9:10], 1,
                        function(x) mean(x, na.rm=TRUE))
contra_aux[,6] <- apply(dadosb[ ,11:12], 1,
                        function(x) mean(x, na.rm=TRUE))
contra_aux[,7] <- apply(contra_aux[,2:6], 1,
                        function(x) sum(is.na(x)))
contra_aux[,8] <- dadosb$ig_parto

#Count of NA's: 
table(contra_aux[,7])

#Change the columns names of the concatenated data: 
coluna_names1 <- c("id","colo_sem24_26","colo_sem27_28",
                   "colo_sem29_30","colo_sem31_32",
                   "colo_sem33_34","NAs","igp_parto")
coluna_names2 <- c("id","num_sem24_26","num_sem27_28",
                   "num_sem29_30","num_sem31_32",
                   "num_sem33_34","NAs","igp_parto")

colnames(colo_aux) <- coluna_names1
colnames(contra_aux) <- coluna_names2

#Let's just consider those cases with at least 2 prenatal evaluations: 
colo_novo <- colo_aux[colo_aux[, "NAs"] < 4,]
contra_novo <- contra_aux[contra_aux[, "NAs"] < 4,]
colo_novo <- as_tibble(colo_novo)
contra_novo <- as_tibble(contra_novo)
casos_em_comum <- inner_join(colo_novo,contra_novo,by="id")

identical(colo_novo$id,casos_em_comum$id)
#We can see by the identical() above that all pregnant women with at 
#least 2 cervical measurements have at least 2 contraction measurements. 

identical(contra_novo$id,casos_em_comum$id)
#The same doesn't happen here, the identical() above shows that NOT all
#pregnant women with at least 2 contraction measurements have at least
#2 cervical measurements.

contra_novo2 <- semi_join(contra_novo,casos_em_comum,by="id")
#Now we have the data where all pregnant women with at least 2
#contraction measurements have at least 2 cervical measurements.

#Making sure all missing data are in NA form, and delete NAs column
#that we are not going to use from now on:
colo_novo$NAs <- NULL
dados_colo <- colo_novo
#dados_colo[is.na(dados_colo)] <- NA

contra_novo2$NAs <- NULL
dados_num_contra <- contra_novo2
#dados_num_contra[is.na(dados_num_contra)] <- NA

##Mean imputation of missing data: 
for (j in 2:6){
  media <- mean(dados_colo[[j]], na.rm=TRUE)
  for (i in 1:dim(dados_colo)[1]){
    if(is.na(dados_colo[i,j])){
      dados_colo[i,j] <- media
    }
  }
}

for (j in 2:6){
  media <- mean(dados_num_contra[[j]], na.rm=TRUE)
  for (i in 1:dim(dados_num_contra)[1]){
    if(is.na(dados_num_contra[i,j])){
      dados_num_contra[i,j] <- media
    }
  }
}

#Long-format of the cervical measurements data:
colo_longf <- pivot_longer(dados_colo[,1:7],-c(id,igp_parto),
                           values_to = "medida_colo",
                           names_to = "indic_aval")

colo_longf$indic_aval[colo_longf$indic_aval=="colo_sem24_26"] <- 1
colo_longf$indic_aval[colo_longf$indic_aval=="colo_sem27_28"] <- 2
colo_longf$indic_aval[colo_longf$indic_aval=="colo_sem29_30"] <- 3
colo_longf$indic_aval[colo_longf$indic_aval=="colo_sem31_32"] <- 4
colo_longf$indic_aval[colo_longf$indic_aval=="colo_sem33_34"] <- 5

colo_longf$indic_aval <- as.numeric(colo_longf$indic_aval)
colnames(colo_longf)[which(names(colo_longf)=="id")] <- "id"
colnames(colo_longf)[which(names(colo_longf)=="igp_parto")] <- "igp_parto"

#Long-format of the contraction measurements data:
contra_longf <- pivot_longer(dados_num_contra[,1:7],-c(id,igp_parto),
                             values_to = "num_contra",
                             names_to = "indic_aval")

contra_longf$indic_aval[contra_longf$indic_aval=="num_sem24_26"] <- 1
contra_longf$indic_aval[contra_longf$indic_aval=="num_sem27_28"] <- 2
contra_longf$indic_aval[contra_longf$indic_aval=="num_sem29_30"] <- 3
contra_longf$indic_aval[contra_longf$indic_aval=="num_sem31_32"] <- 4
contra_longf$indic_aval[contra_longf$indic_aval=="num_sem33_34"] <- 5

contra_longf$indic_aval <- as.numeric(contra_longf$indic_aval)
colnames(contra_longf)[which(names(contra_longf)=="id")] <- "id"
colnames(contra_longf)[which(names(contra_longf)=="igp_parto")] <- "igp_parto"



############# Exploratory analysis of functional covariates  ################

require(fda)

#Mean of cervical measurements: 
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(dados_colo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd
colo_mean <- mean.fd(colofd)

dev.new(width=6,height=3)
plot(colo_mean,col="green",lwd=2,ylim = c(24,40))

#Mean of contraction measurements: 
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(dados_num_contra[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd
contra_mean <- mean.fd(contrafd)

dev.new(width=6,height=3)
plot(contra_mean,col="green",lwd=2,ylim = c(0,5))

#Standard deviation of cervical measurements:
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(dados_colo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd
colo_sd <- sd.fd(colofd)

dev.new(width=6,height=3)
plot(colo_sd,col="green",lwd=2)

#Standard deviation of contraction measurements:
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(dados_num_contra[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd
contra_sd <- sd.fd(contrafd)

dev.new(width=6,height=3)
plot(contra_sd,col="green",lwd=2)

#Mean of cervical measurements and 1/2 +- deviation: 
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(dados_colo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd

colo_mean <- mean.fd(colofd)
colo_sd <- sd.fd(colofd)

x = colo_mean+(1/2)*colo_sd
y = colo_mean-(1/2)*colo_sd

dev.new(width=6,height=3)
plot(colo_mean,col="blue",lwd=2,ylim = c(20,40))
lines(x,col="red",lty = 2)
lines(y,col="red",lty = 2)

#Mean of contraction measurements and 1/2 +- deviation:
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases.
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(dados_num_contra[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd

contra_mean <- mean.fd(contrafd)
contra_sd <- sd.fd(contrafd)

x = contra_mean+(1/2)*contra_sd
y = contra_mean-(1/2)*contra_sd

dev.new(width=6,height=3)
plot(contra_mean,col="blue",lwd=2,ylim = c(0,5))
lines(x,col="red",lty = 2)
lines(y,col="red",lty = 2)

######## Profile plot #########

#Cervical measurements:
perfil_mcolo <- 
  ggplot(colo_longf,
         aes(x=indic_aval,y=medida_colo)) + 
  geom_line(aes(group=id),size=0.8) + 
  labs(y="Medida do colo uterino",
       x="Avaliação") +
  scale_x_continuous(breaks=seq(1,5,1)) +
  scale_y_continuous(breaks=seq(0,54,4)) +
  theme_bw()

dev.new(width=6,height=3)
perfil_mcolo

#Contraction measurements:
perfil_ncontra <- 
  ggplot(contra_longf,
         aes(x=indic_aval,y=num_contra)) + 
  geom_line(aes(group=id),size=0.8) + 
  labs(y="Número de contrações",
       x="Avaliação") +
  scale_x_continuous(breaks=seq(1,5,1)) +
  scale_y_continuous(breaks=seq(0,20,2)) +
  theme_bw()

dev.new(width=6,height=3)
perfil_ncontra

######################## fda model fit ###########################

require(fda)

#Cervical measurements:
var_func <- t(dados_colo[,2:6])
igp_parto <- dados_colo$igp_parto
smallbasis <- create.bspline.basis(c(0,5), 5)
coloSmooth <- smooth.basis(seq(1,5,1),
                           var_func, smallbasis)
colofd <- coloSmooth$fd
cololist = vector("list",2)
cololist[[1]] = rep(1,263)
cololist[[2]] = colofd

conbasis = create.constant.basis(c(0,5))
betabasis = create.bspline.basis(c(0,5),4)
betalist = vector("list",2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(igp_parto,cololist,betalist)
betaestlist = fRegressList$betaestlist
colobetafd = betaestlist[[2]]$fd

dev.new(width=6,height=3)
plot(colobetafd, xlab="Avaliação",
     ylab="Beta para a medida do colo")

#Contraction measurements:
var_func <- t(dados_num_contra[,2:6])
igp_parto <- dados_num_contra$igp_parto
smallbasis <- create.bspline.basis(c(0,5), 5)
contra_novo2Smooth <- smooth.basis(seq(1,5,1),
                           var_func, smallbasis)
contra_novo2fd <- contra_novo2Smooth$fd
contra_novo2list = vector("list",2)
contra_novo2list[[1]] = rep(1,263)
contra_novo2list[[2]] = contra_novo2fd

conbasis = create.constant.basis(c(0,5))
betabasis = create.bspline.basis(c(0,5),4)
betalist = vector("list",2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(igp_parto,
                        contra_novo2list,
                        betalist)
betaestlist = fRegressList$betaestlist
contra_novo2betafd = betaestlist[[2]]$fd

dev.new(width=6,height=3)
plot(contra_novo2betafd,
     xlab="Avaliação",
     ylab="Beta para o número de contrações")


######################## refund model fit ########################

require(refund)

###Fit with 1 functional covariate using pfr().

##Cervical measurements:
Y <- dados_colo$igp_parto 
X <- dados_colo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

#fpc() - Functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 5))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ)$value)

coefs.m = reshape2::melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "FPC", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = FPC, group
           = FPC),width=12,height=6) +
  geom_path() +
  theme_bw()

#lf() -  Regression splines:
Y <- dados_colo$igp_parto 
X <- dados_colo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   Spline = coef(fitJJ)$value)

coefs.m = reshape2::melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Spline", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Spline, group
           = Spline),width=12,height=6) +
  geom_path() +
  theme_bw()

#fpc() And lf() plotted:
Y <- dados_colo$igp_parto 
X <- dados_colo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ1 = pfr(Y ~ fpc(X, k = 5))
fitJJ2 = pfr(Y ~ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ1)$value,
                   Spline = coef(fitJJ2)$value)

coefs.m = reshape2::melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Method", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Method, group
                    = Method),width=12,height=6) +
  geom_path() +
  ylim(-0.3,0.3) +
  theme_bw()

##Contraction measurements:
require(refund)

Y <- dados_num_contra$igp_parto
X <- dados_num_contra[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

#fpc() - functional principal component regression:
fitJJ = pfr(Y ~ fpc(X, k = 5))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ)$value)

coefs.m = reshape2::melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "FPC", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = FPC, group
           = FPC),width=12,height=6) +
  geom_path() +
  theme_bw()

#lf() -  Regression splines:
Y <- dados_num_contra$igp_parto
X <- dados_num_contra[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ = pfr(Y ~ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   Spline = coef(fitJJ)$value)

coefs.m = reshape2::melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Spline", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Spline, group
           = Spline),width=12,height=6) +
  geom_path() +
  theme_bw()

#fpc() And lf() plotted:
Y <- dados_num_contra$igp_parto
X <- dados_num_contra[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ1 = pfr(Y ~ fpc(X, k = 5))
fitJJ2 = pfr(Y ~ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ1)$value,
                   Spline = coef(fitJJ2)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Method", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Method, group
           = Method),width=12,height=6) +
  geom_path() +
  # ylim(-0.3,0.3) +
  theme_bw()

####### Tests for refund using 2 functional covariates and pfr_old() ########

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
#Obs: I had some problems taking variables from differents objects, so i am going
#to create a dataframe with the variables that i will use and the functional
#covariate as a AsIs object inside before the model fit.


#Wide-format:
#Happens -> error:(Model has more coefficients than data).
Y <- dados_colo$igp_parto
Y <- as.matrix(Y)
X1 <- dados_colo[,2:6]
X1 <- as.matrix(X1)
X1 <- I(X1)
X2 <- dados_num_contra[,2:6]
X2 <- as.matrix(X2)
X2 <- I(X2)
id <- dados_num_contra$id
id <- as.matrix(id)

data_asis1 <- data.frame(igp_parto=Y,
                         id=id,
                         colo_perfis=X1,
                         contra_perfis=X2)

rm("Y")
rm("X1")
rm("X2")
rm("id")
Y <- data_asis1$igp_parto
X1 <- data_asis1$colo_perfis
X2 <- data_asis1$contra_perfis
id <- data_asis1$id

#1 covariate:
fitJJ = pfr_old(Y = Y,funcs = X1,subj = id,
                kz = 4,kb = 9,nbasis = 5)

#2 cavariates:
fitJJ = pfr_old(Y = Y,funcs = list(X1,X2),subj = id,
                kz = 4,kb = 9,nbasis = 5,smooth.option="fpca.sc")

#Long-format:
#Happens -> error:(A term has fewer unique covariate combinations than 
#specified maximum degrees of freedom).
Y <- colo_longf$igp_parto
Y <- as.matrix(Y)
X1 <- colo_longf$medida_colo
X1 <- as.matrix(X1)
X1 <- I(X1)
X2 <- contra_longf$num_contra
X2 <- as.matrix(X2)
X2 <- I(X2)
id <- colo_longf$id
id <- as.matrix(id)

data_asis2 <- data.frame(igp_parto=Y,
                         id=id,
                         colo_perfis=X1,
                         contra_perfis=X2)

rm("Y")
rm("X1")
rm("X2")
rm("id")
Y <- data_asis2$igp_parto
X1 <- data_asis2$colo_perfis
X2 <- data_asis2$contra_perfis
id <- data_asis2$id

#1 covariate:
fitJJ = pfr_old(Y = Y,funcs = X1,subj = id,
                kz = 4,kb = 9,nbasis = 5)

#2 covariates:
fitJJ = pfr_old(Y = Y,funcs = list(X1,X2),subj = id,
                kz = 4,kb = 9,nbasis = 5,smooth.option="fpca.sc")
