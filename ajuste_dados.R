########################### Pacotes ##############################

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

########################## Tratamento ############################

dados <- read_excel("Base_TocoColoPerinatal.xls")
View(dados)

dados <- clean_names(dados)
names(dados)

#Verifica a existência de valores que não deveriam existir nas
#variáveis ajustadas à essa função.

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

dados$medida_colo[dados$medida_colo == 0] <- NA


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

dados$ig_aval_sem <- trunc(dados$ig_aval)

n_obs <- sum(dados$indic_aval==1)
n_time <- max(dados$ig_aval_sem) - min(dados$ig_aval_sem) + 1
  
## concatenar medida do colo
dados1 <- 
  spread(data=dados, key = ig_aval_sem,value = medida_colo)

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

## Concatenar de 2 em 2 semanas (como foi observado na pratica)
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

# contagem de NA's
table(colo_aux[,7])

## Concatenar num_contra
dados2 <- 
  spread(data=dados, key = ig_aval_sem,value = num_contra)

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

## Concatenar de 2 em 2 semanas 
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

# Contagem de NA's
table(contra_aux[,7])

## vamos só considerar aqueles casos com pelo menos 2 avaliações no
#pré-natal

colo_novo <- colo_aux[colo_aux[, 7] < 4, ]
contra_novo <- contra_aux[contra_aux[, 7] < 4, ]
colo_novo <- as_tibble(colo_novo)
contra_novo <- as_tibble(contra_novo)

casos_em_comum <- inner_join(colo_novo,contra_novo,by="col1")
identical(colo_novo$col1,casos_em_comum$col1)
# todas gestantes com pelomenos 2 avaliações de medida
# do colo possuem pelomenos 2 avaliações de contrações.

contra_novo2 <- semi_join(contra_novo,casos_em_comum,by="col1")
#Temos então 'colo_novo' com as medidas do colo das gestantes
#que possuem pelomenos 2 avaliações da medida do colo e pelomenos 2 
#avaliações do número de contrações, e 'contra_novo2' com as
#medidas do número de contrações para estas mesmas gestantes.

View(colo_novo)
View(contra_novo2)

#atribuir missing value aqui
for (j in 2:6){
  media <- mean(colo_novo[,j], na.rm=TRUE)
  for (i in 1:dim(colo_novo)[1]){
    if(is.na(colo_novo[i,j])){
      colo_novo[i,j] <- media
    }
  }
}

for (j in 2:6){
  media <- mean(contra_novo2[,j], na.rm=TRUE)
  for (i in 1:dim(contra_novo2)[1]){
    if(is.na(contra_novo2[i,j])){
      contra_novo2[i,j] <- media
    }
  }
}



######################## Descritiva ###########################

#funçao media da medida do colo com 4 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 1 #4 bases
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(colo_novo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd
colo_mean <- mean.fd(colofd)
plot(colo_mean,col="green",lwd=2,ylim = c(24,40))

# com ggplot2
# 
# df <- data.frame(x = avaliacoes,
#                  mean = predict(colo_mean,
#                                 newdata = avaliacoes))
# ggplot(data=df, aes(x=x, y=mean)) +
#   geom_line() +
#   labs(x="time", y="mean value") +
#   ylim(26,35) +
#   theme_bw()

#funçao media da medida do colo com 5 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(colo_novo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd
colo_mean <- mean.fd(colofd)
plot(colo_mean,col="green",lwd=2,ylim = c(24,40))

#com ggplot2

df <- data.frame(x = avaliacoes,
                 mean = predict(colo_mean,
                                newdata = avaliacoes))
ggplot(data=df, aes(x=x, y=mean)) +
  geom_line() +
  labs(x="time", y="mean value") +
  ylim(24,40) +
  theme_bw()

#funçao media do num de contrações com 4 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 1 #4 bases
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(contra_novo2[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd
contra_mean <- mean.fd(contrafd)
plot(contra_mean,col="green",lwd=2,ylim = c(0,5))

# com ggplot2
# 
# df <- data.frame(x = avaliacoes,
#                  mean = predict(contra_mean,
#                                 newdata = avaliacoes))
# ggplot(data=df, aes(x=x, y=mean)) +
#   geom_line() +
#   labs(x="time", y="num_contra value") +
#   ylim(0,5) +
#   theme_bw()

#funçao media do num de contrações com 5 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(contra_novo2[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd
contra_mean <- mean.fd(contrafd)
plot(contra_mean,col="green",lwd=2,ylim = c(0,5))

#com ggplot2

df <- data.frame(x = avaliacoes,
                 mean = predict(contra_mean,
                                newdata = avaliacoes))
ggplot(data=df, aes(x=x, y=mean)) +
  geom_line() +
  labs(x="time", y="num_contra value") +
  ylim(0,5) +
  theme_bw()

#funçao desvio padrão da medida do colo com 5 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(colo_novo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd
colo_sd <- sd.fd(colofd)
plot(colo_sd,col="green",lwd=2)

#funçao desvio padrão da num_contrações com 5 bases(teste)
avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(contra_novo2[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd
contra_sd <- sd.fd(contrafd)
plot(contra_sd,col="green",lwd=2)

#funçao desvio padrão e media juntos (medida_colo) com 5 bases(teste)

avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
m_colo_basis <- create.bspline.basis(rng,nbasis,
                                     norder,knots)
lambdac <- df2lambda(avaliacoes,m_colo_basis)
colofdPar <- fdPar(m_colo_basis,lambda=lambdac)
colo_transpose <- t(colo_novo[,2:6])
colofd <- smooth.basis(avaliacoes,
                       colo_transpose,colofdPar)$fd

colo_mean <- mean.fd(colofd)
colo_sd <- sd.fd(colofd)

x = colo_mean+(1/2)*colo_sd
y = colo_mean-(1/2)*colo_sd

plot(colo_mean,col="blue",lwd=2,ylim = c(20,40))
lines(x,col="red",lty = 2)
lines(y,col="red",lty = 2)

#funçao desvio padrão e media juntos (num_contra) com 5 bases(teste)

avaliacoes <- 1:5
rng <- c(1,5)
knots <- avaliacoes
norder <- 2 #5 bases
nbasis <- length(knots)+norder-2 
n_contra_basis <- create.bspline.basis(rng,nbasis,
                                       norder,knots)
lambdac <- df2lambda(avaliacoes,n_contra_basis)
contrafdPar <- fdPar(n_contra_basis,lambda=lambdac)
contra_transpose <- t(contra_novo2[,2:6])
contrafd <- smooth.basis(avaliacoes,
                         contra_transpose,
                         contrafdPar)$fd

contra_mean <- mean.fd(contrafd)
contra_sd <- sd.fd(contrafd)

x = contra_mean+(1/2)*contra_sd
y = contra_mean-(1/2)*contra_sd

plot(contra_mean,col="blue",lwd=2,ylim = c(0,5))
lines(x,col="red",lty = 2)
lines(y,col="red",lty = 2)

           ######## Gráfico de perfis #########

## Medida do colo


#Deixando os dados em long-format
colo_longf <- pivot_longer(colo_novo[,1:6],-col1,
                               values_to = "medida_colo",
                               names_to = "indic_aval")

colo_longf$indic_aval[colo_longf$indic_aval=="col2"] <- 1
colo_longf$indic_aval[colo_longf$indic_aval=="col3"] <- 2
colo_longf$indic_aval[colo_longf$indic_aval=="col4"] <- 3
colo_longf$indic_aval[colo_longf$indic_aval=="col5"] <- 4
colo_longf$indic_aval[colo_longf$indic_aval=="col6"] <- 5

colo_longf$indic_aval <- as.numeric(colo_longf$indic_aval)
colnames(colo_longf)[which(names(colo_longf)=="col1")] <- "id"

perfil_mcolo <- 
  ggplot(colo_longf,
         aes(x=indic_aval,y=medida_colo)) + 
  geom_line(aes(group=id),size=0.8) + 
  labs(y="Medida do colo uterino",
       x="Avaliação") +
  scale_x_continuous(breaks=seq(1,5,1)) +
  scale_y_continuous(breaks=seq(0,54,4)) +
  theme_bw()


## Número de contrações


#Deixando os dados em long-format
contra_longf <- pivot_longer(contra_novo2[,1:6],-col1,
                           values_to = "num_contra",
                           names_to = "indic_aval")

contra_longf$indic_aval[contra_longf$indic_aval=="col2"] <- 1
contra_longf$indic_aval[contra_longf$indic_aval=="col3"] <- 2
contra_longf$indic_aval[contra_longf$indic_aval=="col4"] <- 3
contra_longf$indic_aval[contra_longf$indic_aval=="col5"] <- 4
contra_longf$indic_aval[contra_longf$indic_aval=="col6"] <- 5

contra_longf$indic_aval <- as.numeric(contra_longf$indic_aval)
colnames(contra_longf)[which(names(contra_longf)=="col1")] <- "id"

perfil_ncontra <- 
  ggplot(contra_longf,
         aes(x=indic_aval,y=num_contra)) + 
  geom_line(aes(group=id),size=0.8) + 
  labs(y="Número de contrações",
       x="Avaliação") +
  scale_x_continuous(breaks=seq(1,5,1)) +
  scale_y_continuous(breaks=seq(0,20,2)) +
  theme_bw()

######################## Ajuste FDA ###########################
require(fda)
#teste ajuste fda da medida do colo
var_func <- t(colo_novo[,2:6])
igp_parto <- as.numeric(colo_novo[,8])
smallbasis <- create.bspline.basis(c(0,5), 5)
coloSmooth <- smooth.basis(seq(1,5,1),
                           var_func, smallbasis)
colofd <- coloSmooth$fd
plot(colofd)
cololist = vector("list",2)
cololist[[1]] = rep(1,263)
cololist[[2]] = colofd

conbasis = create.constant.basis(c(0,5))
betabasis = create.bspline.basis(c(0,5),4)
betalist = vector("list",2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(colo_novo$col8,cololist,betalist)
betaestlist = fRegressList$betaestlist
colobetafd = betaestlist[[2]]$fd
dev.new(width=6,height=3)
plot(colobetafd, xlab="Avaliação",
     ylab="Beta para a medida do colo")

#teste ajuste fda do numero de contrações
var_func <- t(contra_novo2[,2:6])
igp_parto <- as.numeric(contra_novo2[,8])
smallbasis <- create.bspline.basis(c(0,5), 5)
contra_novo2Smooth <- smooth.basis(seq(1,5,1),
                           var_func, smallbasis)
contra_novo2fd <- contra_novo2Smooth$fd
plot(contra_novo2fd)
contra_novo2list = vector("list",2)
contra_novo2list[[1]] = rep(1,263)
contra_novo2list[[2]] = contra_novo2fd

conbasis = create.constant.basis(c(0,5))
betabasis = create.bspline.basis(c(0,5),4)
betalist = vector("list",2)
betalist[[1]] = conbasis
betalist[[2]] = betabasis

fRegressList = fRegress(contra_novo2$col8,
                        contra_novo2list,
                        betalist)
betaestlist = fRegressList$betaestlist
contra_novo2betafd = betaestlist[[2]]$fd
dev.new(width=6,height=3)
plot(contra_novo2betafd,
     xlab="Avaliação",
     ylab="Beta para o número de contrações")

#teste ajuste fda da medida do colo e num_contrações (NÃO FUNCIONA)
# var_func1 <- t(colo_novo[,2:6])
# igp_parto <- as.numeric(colo_novo[,8])
# smallbasis1 <- create.bspline.basis(c(0,5), 5)
# coloSmooth <- smooth.basis(seq(1,5,1),
#                            var_func1, smallbasis1)
# 
# var_func2 <- t(contra_novo2[,2:6])
# smallbasis2 <- create.bspline.basis(c(0,5), 5)
# contra_novo2Smooth <- smooth.basis(seq(1,5,1),
#                                    var_func2, smallbasis2)
# colofd <- coloSmooth$fd
# contra_novo2fd <- contra_novo2Smooth$fd
# colo_contra_list = vector("list",3)
#  colo_contra_list[[1]] = rep(1,263)
# colo_contra_list[[2]] = colofd
# colo_contra_list[[3]] = contra_novo2fd
# 
#  conbasis = create.constant.basis(c(0,5))
# betabasis1 = create.bspline.basis(c(0,5),5)
# betabasis2 = create.bspline.basis(c(0,5),5)
# betalist = vector("list",3)
#  betalist[[1]] = conbasis
# betalist[[2]] = betabasis1
# betalist[[3]] = betabasis2
# 
# fRegressList = fRegress(igp_parto,colo_contra_list,betalist)
# names(fRegressList)
# betaestlist = fRegressList$betaestlist
# colo_contra_betafd = betaestlist$fd
# plot(colo_contra_betafd, xlab="Avaliação",
#      ylab="Beta")

######################## Ajuste refund ########################
require(refund)
#teste ajuste refund da medida do colo
igp_parto <- as.numeric(colo_novo[,8])
Y <- igp_parto
X <- colo_novo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

#FPC - functional principal component regression
fitJJ = pfr(Y ∼ fpc(X, k = 5))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "FPC", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = FPC, group
           = FPC),width=12,height=6) +
  geom_path() +
  theme_bw()

#Splines
igp_parto <- as.numeric(colo_novo[,8])
Y <- igp_parto
X <- colo_novo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ = pfr(Y ∼ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   Spline = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Spline", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Spline, group
           = Spline),width=12,height=6) +
  geom_path() +
  theme_bw()

#FPC And Splines plotted
fitJJ1 = pfr(Y ∼ fpc(X, k = 5))
fitJJ2 = pfr(Y ∼ lf(X, bs = "ps", k = 5, fx = T))

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
  ylim(-0.3,0.3) +
  theme_bw()

#teste ajuste refund do número de contrações
require(refund)
igp_parto <- as.numeric(contra_novo2[,8])
Y <- igp_parto
X <- contra_novo2[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

#FPC - functional principal component regression
fitJJ = pfr(Y ∼ fpc(X, k = 5))

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "FPC", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = FPC, group
           = FPC),width=12,height=6) +
  geom_path() +
  theme_bw()

#Splines
igp_parto <- as.numeric(contra_novo2[,8])
Y <- igp_parto
X <- contra_novo2[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)
fitJJ = pfr(Y ∼ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   Spline = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Spline", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Spline, group
           = Spline),width=12,height=6) +
  geom_path() +
  theme_bw()

#FPC And Splines plotted
igp_parto <- as.numeric(contra_novo2[,8])
Y <- igp_parto
X <- contra_novo2[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ1 = pfr(Y ∼ fpc(X, k = 5))
fitJJ2 = pfr(Y ∼ lf(X, bs = "ps", k = 5, fx = T))

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

require(refund)
####### teste ajuste refund ambos preditores funcionais ########

### FPC - functional principal component regression

## EXEMPLO

# load and reassign the data;
data(DTI2)
Y  <- DTI2$pasat ## PASAT outcome
id <- DTI2$id    ## subject id
W1 <- DTI2$cca   ## Corpus Callosum
W2 <- DTI2$rcst  ## Right corticospinal
V  <- DTI2$visit ## visit

# prep scalar covariate
visit.1.rest <- matrix(as.numeric(V > 1), ncol=1)
covar.in <- visit.1.rest 


# note there is missingness in the functional predictors
apply(is.na(W1), 2, mean)
apply(is.na(W2), 2, mean)

# fit two univariate models
pfr.obj.t1 <- pfr_old(Y = Y, covariates=covar.in, funcs = list(W1),subj = id, kz = 10, kb = 50)
pfr.obj.t2 <- pfr(Y = Y, covariates=covar.in, funcs = list(W2),
                  subj = id, kz = 10, kb = 50)

# one model with two functional predictors using "smooth.face"
#  for smoothing predictors
pfr.obj.t3 <- pfr(Y = Y, covariates=covar.in, funcs = list(W1, W2), 
                  subj = id, kz = 10, kb = 50, nbasis=35,smooth.option="fpca.face")


####### MEUS TESTES (ainda não funciona) 
igp_parto <- as.numeric(colo_novo[,8])
Y <- igp_parto
X1 <- colo_novo[,2:6]
X2 <- contra_novo2[,2:6]
X1 <- as.matrix(X1)
X2 <- as.matrix(X2)
t <- seq(1,5,length = 5)

#1 (formato novo)
fitJJ = pfr(Y ∼ lf(list(X1,X2), k = 5)) 

#2 (formato antigo)
fitJJ = pfr_old(Y=Y,funcs=list(X1,X2),kz=10,kb=10,nbasis=5)

coefs = data.frame(grid = t,
                   FPC = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "FPC", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = FPC, group
           = FPC),width=12,height=6) +
  geom_path() +
  theme_bw()

#Splines
igp_parto <- as.numeric(colo_novo[,8])
Y <- igp_parto
X <- colo_novo[,2:6]
X <- as.matrix(X)
t <- seq(1,5,length = 5)

fitJJ = pfr(Y ∼ lf(X, bs = "ps", k = 5, fx = T))

coefs = data.frame(grid = t,
                   Spline = coef(fitJJ)$value)

coefs.m = melt(coefs, id = "grid")
colnames(coefs.m) = c("grid", "Spline", "Value")

dev.new(width=6,height=3)
ggplot(coefs.m, 
       aes(x = grid, y = Value, color = Spline, group
           = Spline),width=12,height=6) +
  geom_path() +
  theme_bw()


################### Codigo da professora ######################

#Aplicando no FDA (covariavel funcional = colo_novo,
#resposta = igp_parto)
var_func <- t(colo_novo[,2:6])
igp_parto <- as.numeric(colo_novo[,8])

smallbasis <- create.bspline.basis(c(0,5), 5)
# The covariate is the temperature curve for each station.
tempfd <- smooth.basis(seq(1,5,1),
                       var_func, smallbasis)$fd
aj <- fRegress(igp_parto ~ tempfd)

pred <- aj$yhatfdobj

# plot the data and the fit
plot(pred, igp_parto, type="p", pch="o")
lines(pred, igp_parto, lty=2)
# print root mean squared error
RMSE <- sqrt(mean((igp_parto-pred)^2))
print(paste("RMSE =",RMSE))
# plot the estimated regression function
plot(aj$betaestlist[[2]])
# This isn't helpful either, the coefficient function is too
# complicated to interpret.
# display the number of basis functions used:
print(aj$betaestlist[[2]]$fd$basis$nbasis)
# 4 basis functions to fit 263 values 

#Aplicando no FDA (covariavel funcional = contra_novo2,
#resposta = igp_parto)
var_func2 <- t(contra_novo2[,2:6])
igp_parto <- as.numeric(contra_novo2[,8])

smallbasis <- create.bspline.basis(c(0,1), 4)
# The covariate is the temperature curve for each station.
tempfd <- smooth.basis(seq(0,1,0.25),
                       var_func, smallbasis)$fd
aj <- fRegress(igp_parto ~ tempfd)

pred <- aj$yhatfdobj

# plot the data and the fit
plot(pred, igp_parto, type="p", pch="o")
lines(pred, igp_parto, lty=2)
# print root mean squared error
RMSE <- sqrt(mean((igp_parto-pred)^2))
print(paste("RMSE =",RMSE))
# plot the estimated regression function
plot(aj$betaestlist[[2]])
# This isn't helpful either, the coefficient function is too
# complicated to interpret.
# display the number of basis functions used:
print(aj$betaestlist[[2]]$fd$basis$nbasis)
# 4 basis functions to fit 263 values 


## Ajustando fRegress.CV
# Esse só com  xfdlist and betalist
##
xfdlist <- list(const=rep(1, 267), tempfd=tempfd)
# The intercept must be constant for a scalar response
betabasis1 <- create.constant.basis(c(0, 1))
betafd1 <- fd(0, betabasis1)
betafdPar1 <- fdPar(betafd1)
betafd2 <- create.fourier.basis(c(0,1), 2)
# convert to an fdPar object
betafdPar2 <- fdPar(betafd2)
betalist <- list(const=betafdPar1, tempfd=betafdPar2)
aj2 <- fRegress.CV(igp_parto, xfdlist, betalist)

## Do livro:
tempbasis <- create.fourier.basis(c(0,1), 3)
tempSmooth <- smooth.basis(seq(0,1,0.25), var_func, smallbasis)
tempfd <- tempSmooth$fd

templist <-  vector("list",2)
templist[[1]] <- rep(1, 267)
templist[[2]] <- tempfd

# Three Estimates of the Regression Coefficient Predicting
#Annual Precipitation

##1) Low-Dimensional Regression Coefficient Function β
conbasis <- create.constant.basis(c(0,1))
betabasis <- create.fourier.basis(c(0,1),3)
betalist <- vector("list",2)
betalist[[1]] <- conbasis
betalist[[2]] <- betabasis

fRegressList <- fRegress(igp_parto,templist,betalist)

betaestlist <- fRegressList$betaestlist
tempbetafd <- betaestlist[[2]]$fd
plot(tempbetafd, xlab="Pre natal",
     ylab="Beta para IGP")

pred1 <- fRegressList$yhatfdobj
cres1 <- igp_parto - pred1
SSE1.1 <- sum(cres1^2)
SSE0 <- sum((igp_parto- mean(igp_parto))^2)
RSQ1 <-  (SSE0-SSE1.1)/SSE0
Fratio1 <- ((SSE0-SSE1.1)/3)/(SSE1.1/266)

#Coefficient β Estimate Using a Roughness Penalty
#First, we set up a harmonic acceleration operator,
#as we did already in Chapter 5.
Lcoef <- c(0,(2*pi)^2,0)
harmaccelLfd <- vec2Lfd(Lcoef, c(0,1))

#β estimate by a functional parameter object that 
# incorporates both this roughness penalty and a level
# of smoothing:

betabasis <- create.fourier.basis(c(0, 1), 3)
lambda <- 10^12.5
betafdPar <- fdPar(betabasis, harmaccelLfd, lambda)
betalist[[2]] <- betafdPar

fRegressList2 <- fRegress(igp_parto, templist, betalist)
betaestlist2 <- fRegressList2$betaestlist
pred2 <- fRegressList2$yhatfdobj

SSE1.2 <- sum((igp_parto-pred2)^2)
RSQ2 = (SSE0 - SSE1.2)/SSE0
Fratio2 = ((SSE0-SSE1.2)/3)/(SSE1.2/266)

##2) Choosing Smoothing Parameters
loglam <- seq(5,15,0.5)
nlam <- length(loglam)
SSE.CV <- matrix(0,nlam,1)
for (ilam in 1:nlam) {
  lambda = 10^loglam[ilam]
  betalisti = betalist
  betafdPar2 = betalisti[[2]]
  betafdPar2$lambda = lambda
  betalisti[[2]] = betafdPar2
  fRegi = fRegress.CV(igp_parto, templist,
                      betalisti)
  SSE.CV[ilam] = fRegi$SSE.CV
}

##3) Scalar Response Models by Functional Principal Components



## Teste

F.res <-  Fperm.fd(igp_parto, templist, betalist)

F.res$qval
F.res$Fobs

