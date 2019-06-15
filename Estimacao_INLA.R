# CÃ³digo da Tese do Ensaio 2 do I de Moran #
# O objetivo do cÃ³digo Ã© calcular o Ã de Moran Ã¡ra diferentres especificaÃ§Ãµes da matriz de vizinhanÃ§as #
# (ou adjacÃªncias) e verificar quais que se adequam melhor aos dados com os testes disponÃ?veis #
# O I de Moran Ã© feito com bases nas taxas por 100.000 habitantes #
# Lendo o shape 43mu2500gsd, os dois lagos entram como "municÃ?pio"
require(rgdal)
require(spdep)
require(INLA)

#setwd("C:/Users/renan/Desktop/Projeto Tese")
setwd("C:/Users/Windows 8.1/Desktop/Projeto Tese")

df_mun_pre <- read.csv("DadosMunBrutoEnsaio2.csv", header=T, sep=";", dec=",")
df_mun <- df_mun_pre[df_mun_pre$Mun != "Pinto Bandeira",] # Retira Pinto Bandeira

crimes <- c("Roub", "RoubVei", "Latro", "Furt", "HomDol", "FurtVei", "ExtoMS") # Crimes a ser usado no INLA
# Roub, RoubVei, Latro, Furt, HomDol, FurtVei, ExtoMS, Exto, Este, EntTra, EntPosse, DelCorrup, DelArmMun

banco_mun <- 
  cbind(df_mun[,1:2],
        df_mun[,substring(names(df_mun), first=1, last=nchar(names(df_mun))-4) %in% crimes],
        df_mun[,(dim(df_mun)[2]-13):dim(df_mun)[2]])

#mapa_rs_pre <- readOGR("C:/Users/renan/Desktop/Projeto Tese/Shapes", "43mu2500gsd", encoding='UTF-8', verbose=FALSE)
mapa_rs_pre <- readOGR("C:/Users/Windows 8.1/Desktop/Projeto Tese/Shapes", "43mu2500gsd", encoding='UTF-8', verbose=FALSE)
# all.x = FALSE é para retirar os dois lagos que não são municípios
mapa_rs <- merge(mapa_rs_pre, banco_mun, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)


# Construindo os grafos

# Contiguidade de 1ª ordem
nbrsm_c1 <- poly2nb(mapa_rs, queen = TRUE) # Um ponto já é suficiente para ser ser vizinho
nbrsw_c1 <- nb2listw(nbrsm_c1) # Lista de Vizinhança padronizada

# Vizinho de 1ª ordem (o número de vizinhos é o número de ordens)
nbrsm_pre <- knearneigh(coordinates(mapa_rs), k=1) # Um ponto já é suficiente para ser ser vizinho
nbrsm_k1 <- knn2nb(nbrsm_pre)
nbrsw_k1 <- nb2listw(nbrsm_k1) # Lista de Vizinhança padronizada

# Vizinho de 2ª ordem (o número de vizinhos é o número de ordens)
nbrsm_pre <- knearneigh(coordinates(mapa_rs), k=2) # Um ponto já é suficiente para ser ser vizinho
nbrsm_k2 <- knn2nb(nbrsm_pre)
nbrsw_k2 <- nb2listw(nbrsm_k2) # Lista de Vizinhança padronizada

# Vizinho de 3ª ordem (o número de vizinhos é o número de ordens)
nbrsm_pre <- knearneigh(coordinates(mapa_rs), k=3) # Um ponto já é suficiente para ser ser vizinho
nbrsm_k3 <- knn2nb(nbrsm_pre)
nbrsw_k3 <- nb2listw(nbrsm_k3) # Lista de Vizinhança padronizada


# Para o INLA
#nb2INLA(file="graphs/c1.g", nbrsm_c1)
#nb2INLA(file="graphs/k1.g", nbrsm_k1)
#nb2INLA(file="graphs/k2.g", nbrsm_k2)
#nb2INLA(file="graphs/k3.g", nbrsm_k3)










# Modelos antigos, caso seja de interesse mudar as prioris
#forms_elias <- list(m0=y ~ 1,
#              m1=y ~ 1 + f(i, model="ar1"),
#              m2=y ~ 1 + f(i, model="besag", graph="graphs/k1.g"),
#              m3=y ~ 1 + f(Ano, model="ar1") + f(i, model="besag", graph="graphs/k1.g"),
#              m4=y ~ 1 + f(i, model="besag", graph="graphs/k1.g",group=Ano, control.group=list(model="ar1", hyper=list(theta=list(param=c(0,1))))))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
#forms_ant <- list(m2 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE,
#                             hyper=list(prec=list(prior="loggamma",param=c(1,0.0005)))),
#                  m5 = y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE,
#                             hyper=list(prec.unstruct=list(prior="loggamma",param=c(1,0.0005)),
#                                        prec.spatial =list(prior="loggamma",param=c(1,0.0005)))))










# Modelos a serem criados:

# Roub, RoubVei, Latro, Furt, HomDol, FurtVei, ExtoMS, Exto, Este, EntTra, EntPosse, DelCorrup, DelArmMun

######################################
########### ROUBOS ###################
######################################
crime <- "Roub"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.roub <- lapply(forms, inla, family="Poisson",
                    data=base, E=base$esperado,
                    control.predictor=list(compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE))



##################################################
########### ROUBOS DE VE?CULOS ###################
##################################################
crime <- "RoubVei"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.roubvei <- lapply(forms, inla, family="Poisson",
                       data=base, E=base$esperado,
                       control.predictor=list(compute=TRUE),
                       control.compute=list(dic=TRUE, cpo=TRUE))



##################################################
########### LATROC?NIOS ##########################
##################################################
crime <- "Latro"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.latro <- lapply(forms, inla, family="Poisson",
                     data=base, E=base$esperado,
                     control.predictor=list(compute=TRUE),
                     control.compute=list(dic=TRUE, cpo=TRUE))


##################################################
########### FURTOS ###############################
##################################################
crime <- "Furt"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.furt <- lapply(forms, inla, family="Poisson",
                    data=base, E=base$esperado,
                    control.predictor=list(compute=TRUE),
                    control.compute=list(dic=TRUE, cpo=TRUE))


##################################################
########### HOMIC?DIOS DOLOSOS ###################
##################################################
crime <- "HomDol"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.homdol <- lapply(forms, inla, family="Poisson",
                      data=base, E=base$esperado,
                      control.predictor=list(compute=TRUE),
                      control.compute=list(dic=TRUE, cpo=TRUE))


##################################################
########### FURTO DE VE?CULOS ####################
##################################################
crime <- "FurtVei"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.furtvei <- lapply(forms, inla, family="Poisson",
                       data=base, E=base$esperado,
                       control.predictor=list(compute=TRUE),
                       control.compute=list(dic=TRUE, cpo=TRUE))



############################################################
########### Extors?o Mediante Sequestro ####################
############################################################
crime <- "ExtoMS"

data <- cbind(mapa_rs@data[,1],
              mapa_rs@data[,substring(names(mapa_rs@data), first=1, last=nchar(names(mapa_rs@data))-4) %in% c(crime, "Pop")])
names(data)[1] <- "GEOCODIG_M"

teste1 <- reshape(data, v.names=crime, idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(2:15))
teste2 <- reshape(data, v.names=c("Pop"), idvar="GEOCODIG_M", timevar = "Ano",
                  direction = "long", varying = list(16:29))
base <- cbind(teste1[,c("GEOCODIG_M","Ano", crime)], teste2[,"Pop"])
names(base)[3:4] <- c("Variavel", "Pop")


var.a <- tapply(base$Variavel, base$Ano, sum, na.rm=TRUE)
pop.a <- tapply(base$Pop, base$Ano, sum, na.rm=TRUE)
tx.a <- var.a/pop.a
base$esperado <- base$Pop * rep(tx.a, each=length(nbrsm_c1))
smro <- base$Variavel/base$esperado
base$y <- base$Variavel
base$i <- rep(1:496, times = length(unique(base$Ano)))

# O ajuste tem que ser feito para vizinhos de 1 a 3 ordem no knn
forms <- list(m.bes.c1 =    y ~ 1 + f(i, model="besag", graph="graphs/c1.g"),
              m.bym.c1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/c1.g"),
              m.bes.rw.c1 = y ~ 1 + f(i, model="besag", graph="graphs/c1.g") + f(Ano, model="rw1"),
              m.bym.rw.c1 = y ~ 1 + f(i, model="bym",   graph="graphs/c1.g") + f(Ano, model="rw1"),
              
              m.bes.k1 =    y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bym.k1 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k1.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k1 = y ~ 1 + f(i, model="besag", graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k1 = y ~ 1 + f(i, model="bym",   graph="graphs/k1.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k2 =    y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bym.k2 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k2.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k2 = y ~ 1 + f(i, model="besag", graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k2 = y ~ 1 + f(i, model="bym",   graph="graphs/k2.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              
              m.bes.k3 =    y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bym.k3 =    y ~ 1 + f(i, model="bym"  , graph="graphs/k3.g", adjust.for.con.comp = FALSE),
              m.bes.rw.k3 = y ~ 1 + f(i, model="besag", graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"),
              m.bym.rw.k3 = y ~ 1 + f(i, model="bym",   graph="graphs/k3.g", adjust.for.con.comp = FALSE) + f(Ano, model="rw1"))


mods.extoms <- lapply(forms, inla, family="Poisson",
                      data=base, E=base$esperado,
                      control.predictor=list(compute=TRUE),
                      control.compute=list(dic=TRUE, cpo=TRUE))





#save.image("Resultados_mods_INLA.Rdata")
