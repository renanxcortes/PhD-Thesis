#setwd("C:/Users/renan/Desktop/Projeto Tese")
setwd("C:/Users/Windows 8.1/Desktop/Projeto Tese")

load("Resultados_mods_INLA.Rdata")

# Roub, RoubVei, Latro, Furt, HomDol, FurtVei, ExtoMS, Exto, Este, EntTra, EntPosse, DelCorrup, DelArmMun
tab_dic <- data.frame(Matriz = rep(c("Cont. (1ª)", "Knn (1ª)", "Knn (2ª)", "Knn (3ª)"), each=4),
                      Especificação = rep(c("Besag", "BYM", "Besag + RW1", "BYM + RW1"), times=4),
                      Roubo =    sapply(mods.roub, function(x) x$dic$dic),
                      RouboVei = sapply(mods.roubvei, function(x) x$dic$dic),
                      Latro =    sapply(mods.latro, function(x) x$dic$dic),
                      Furto =    sapply(mods.furt, function(x) x$dic$dic),
                      HomDol =   sapply(mods.homdol, function(x) x$dic$dic),
                      FurtVei =  sapply(mods.furtvei, function(x) x$dic$dic),
                      ExtoMS =   sapply(mods.extoms, function(x) x$dic$dic), row.names=NULL)

tab_cpo <- data.frame(Matriz = rep(c("Cont. (1ª)", "Knn (1ª)", "Knn (2ª)", "Knn (3ª)"), each=4),
                      Especificação = rep(c("Besag", "BYM", "Besag + RW1", "BYM + RW1"), times=4),
                      Roubo =    sapply(mods.roub, function(x) -sum(log(x$cpo$cpo))),
                      RouboVei = sapply(mods.roubvei, function(x) -sum(log(x$cpo$cpo))),
                      Latro =    sapply(mods.latro, function(x) -sum(log(x$cpo$cpo))),
                      Furto =    sapply(mods.furt, function(x) -sum(log(x$cpo$cpo))),
                      HomDol =   sapply(mods.homdol, function(x) -sum(log(x$cpo$cpo))),
                      FurtVei =  sapply(mods.furtvei, function(x) -sum(log(x$cpo$cpo))),
                      ExtoMS =   sapply(mods.extoms, function(x) -sum(log(x$cpo$cpo))), row.names=NULL)

write.table(tab_dic, header=T, sep=";")
#estimado_besag <- base$esperado*exp(mods$m2$summary.linear.pred$mean)
#estimado_bym <- base$esperado*exp(mods$m5$summary.linear.pred$mean)
#aux <- data.frame(Y=base$y, Est_bes=estimado_besag, Est_bym=estimado_bym,Pop=base$Pop)
# DIC e CPOmods.roub
#sapply(mods, function(x) x$dic[[1]])
#-sum(log(mods$m2$cpo$cpo))




# Estimativas (o objeto 'base' tem que ser atualizado, pois ele mudava a cada estimativa)

load("bases_para_estimar_y.Rdata")

# ROUBO #
yhat_roubo_m.bes.c1 <- base_roubo$esperado * exp(mods.roub$m.bes.c1$summary.linear.pred$mean)
yhat_roubo_m.bym.c1 <- base_roubo$esperado * exp(mods.roub$m.bym.c1$summary.linear.pred$mean)
yhat_roubo_m.bes.rw.c1 <- base_roubo$esperado * exp(mods.roub$m.bes.rw.c1$summary.linear.pred$mean)
yhat_roubo_m.bym.rw.c1 <- base_roubo$esperado * exp(mods.roub$m.bym.rw.c1$summary.linear.pred$mean)

yhat_roubo_m.bes.k1 <- base_roubo$esperado * exp(mods.roub$m.bes.k1$summary.linear.pred$mean)
yhat_roubo_m.bym.k1 <- base_roubo$esperado * exp(mods.roub$m.bym.k1$summary.linear.pred$mean)
yhat_roubo_m.bes.rw.k1 <- base_roubo$esperado * exp(mods.roub$m.bes.rw.k1$summary.linear.pred$mean)
yhat_roubo_m.bym.rw.k1 <- base_roubo$esperado * exp(mods.roub$m.bym.rw.k1$summary.linear.pred$mean)

yhat_roubo_m.bes.k2 <- base_roubo$esperado * exp(mods.roub$m.bes.k2$summary.linear.pred$mean)
yhat_roubo_m.bym.k2 <- base_roubo$esperado * exp(mods.roub$m.bym.k2$summary.linear.pred$mean)
yhat_roubo_m.bes.rw.k2 <- base_roubo$esperado * exp(mods.roub$m.bes.rw.k2$summary.linear.pred$mean)
yhat_roubo_m.bym.rw.k2 <- base_roubo$esperado * exp(mods.roub$m.bym.rw.k2$summary.linear.pred$mean)

yhat_roubo_m.bes.k3 <- base_roubo$esperado * exp(mods.roub$m.bes.k3$summary.linear.pred$mean)
yhat_roubo_m.bym.k3 <- base_roubo$esperado * exp(mods.roub$m.bym.k3$summary.linear.pred$mean)
yhat_roubo_m.bes.rw.k3 <- base_roubo$esperado * exp(mods.roub$m.bes.rw.k3$summary.linear.pred$mean)
yhat_roubo_m.bym.rw.k3 <- base_roubo$esperado * exp(mods.roub$m.bym.rw.k3$summary.linear.pred$mean)


# ROUBO VEICULOS #
yhat_roubovei_m.bes.c1 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.c1$summary.linear.pred$mean)
yhat_roubovei_m.bym.c1 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.c1$summary.linear.pred$mean)
yhat_roubovei_m.bes.rw.c1 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.rw.c1$summary.linear.pred$mean)
yhat_roubovei_m.bym.rw.c1 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.rw.c1$summary.linear.pred$mean)

yhat_roubovei_m.bes.k1 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.k1$summary.linear.pred$mean)
yhat_roubovei_m.bym.k1 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.k1$summary.linear.pred$mean)
yhat_roubovei_m.bes.rw.k1 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.rw.k1$summary.linear.pred$mean)
yhat_roubovei_m.bym.rw.k1 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.rw.k1$summary.linear.pred$mean)

yhat_roubovei_m.bes.k2 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.k2$summary.linear.pred$mean)
yhat_roubovei_m.bym.k2 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.k2$summary.linear.pred$mean)
yhat_roubovei_m.bes.rw.k2 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.rw.k2$summary.linear.pred$mean)
yhat_roubovei_m.bym.rw.k2 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.rw.k2$summary.linear.pred$mean)

yhat_roubovei_m.bes.k3 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.k3$summary.linear.pred$mean)
yhat_roubovei_m.bym.k3 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.k3$summary.linear.pred$mean)
yhat_roubovei_m.bes.rw.k3 <- base_roubovei$esperado * exp(mods.roubvei$m.bes.rw.k3$summary.linear.pred$mean)
yhat_roubovei_m.bym.rw.k3 <- base_roubovei$esperado * exp(mods.roubvei$m.bym.rw.k3$summary.linear.pred$mean)


# LATROCINIO #
yhat_latro_m.bes.c1 <- base_latro$esperado * exp(mods.latro$m.bes.c1$summary.linear.pred$mean)
yhat_latro_m.bym.c1 <- base_latro$esperado * exp(mods.latro$m.bym.c1$summary.linear.pred$mean)
yhat_latro_m.bes.rw.c1 <- base_latro$esperado * exp(mods.latro$m.bes.rw.c1$summary.linear.pred$mean)
yhat_latro_m.bym.rw.c1 <- base_latro$esperado * exp(mods.latro$m.bym.rw.c1$summary.linear.pred$mean)

yhat_latro_m.bes.k1 <- base_latro$esperado * exp(mods.latro$m.bes.k1$summary.linear.pred$mean)
yhat_latro_m.bym.k1 <- base_latro$esperado * exp(mods.latro$m.bym.k1$summary.linear.pred$mean)
yhat_latro_m.bes.rw.k1 <- base_latro$esperado * exp(mods.latro$m.bes.rw.k1$summary.linear.pred$mean)
yhat_latro_m.bym.rw.k1 <- base_latro$esperado * exp(mods.latro$m.bym.rw.k1$summary.linear.pred$mean)

yhat_latro_m.bes.k2 <- base_latro$esperado * exp(mods.latro$m.bes.k2$summary.linear.pred$mean)
yhat_latro_m.bym.k2 <- base_latro$esperado * exp(mods.latro$m.bym.k2$summary.linear.pred$mean)
yhat_latro_m.bes.rw.k2 <- base_latro$esperado * exp(mods.latro$m.bes.rw.k2$summary.linear.pred$mean)
yhat_latro_m.bym.rw.k2 <- base_latro$esperado * exp(mods.latro$m.bym.rw.k2$summary.linear.pred$mean)

yhat_latro_m.bes.k3 <- base_latro$esperado * exp(mods.latro$m.bes.k3$summary.linear.pred$mean)
yhat_latro_m.bym.k3 <- base_latro$esperado * exp(mods.latro$m.bym.k3$summary.linear.pred$mean)
yhat_latro_m.bes.rw.k3 <- base_latro$esperado * exp(mods.latro$m.bes.rw.k3$summary.linear.pred$mean)
yhat_latro_m.bym.rw.k3 <- base_latro$esperado * exp(mods.latro$m.bym.rw.k3$summary.linear.pred$mean)



# FURTO #
yhat_furt_m.bes.c1 <- base_furt$esperado * exp(mods.furt$m.bes.c1$summary.linear.pred$mean)
yhat_furt_m.bym.c1 <- base_furt$esperado * exp(mods.furt$m.bym.c1$summary.linear.pred$mean)
yhat_furt_m.bes.rw.c1 <- base_furt$esperado * exp(mods.furt$m.bes.rw.c1$summary.linear.pred$mean)
yhat_furt_m.bym.rw.c1 <- base_furt$esperado * exp(mods.furt$m.bym.rw.c1$summary.linear.pred$mean)

yhat_furt_m.bes.k1 <- base_furt$esperado * exp(mods.furt$m.bes.k1$summary.linear.pred$mean)
yhat_furt_m.bym.k1 <- base_furt$esperado * exp(mods.furt$m.bym.k1$summary.linear.pred$mean)
yhat_furt_m.bes.rw.k1 <- base_furt$esperado * exp(mods.furt$m.bes.rw.k1$summary.linear.pred$mean)
yhat_furt_m.bym.rw.k1 <- base_furt$esperado * exp(mods.furt$m.bym.rw.k1$summary.linear.pred$mean)

yhat_furt_m.bes.k2 <- base_furt$esperado * exp(mods.furt$m.bes.k2$summary.linear.pred$mean)
yhat_furt_m.bym.k2 <- base_furt$esperado * exp(mods.furt$m.bym.k2$summary.linear.pred$mean)
yhat_furt_m.bes.rw.k2 <- base_furt$esperado * exp(mods.furt$m.bes.rw.k2$summary.linear.pred$mean)
yhat_furt_m.bym.rw.k2 <- base_furt$esperado * exp(mods.furt$m.bym.rw.k2$summary.linear.pred$mean)

yhat_furt_m.bes.k3 <- base_furt$esperado * exp(mods.furt$m.bes.k3$summary.linear.pred$mean)
yhat_furt_m.bym.k3 <- base_furt$esperado * exp(mods.furt$m.bym.k3$summary.linear.pred$mean)
yhat_furt_m.bes.rw.k3 <- base_furt$esperado * exp(mods.furt$m.bes.rw.k3$summary.linear.pred$mean)
yhat_furt_m.bym.rw.k3 <- base_furt$esperado * exp(mods.furt$m.bym.rw.k3$summary.linear.pred$mean)

# HOMICIDIO DOLOSO #
yhat_homdol_m.bes.c1 <- base_homdol$esperado * exp(mods.homdol$m.bes.c1$summary.linear.pred$mean)
yhat_homdol_m.bym.c1 <- base_homdol$esperado * exp(mods.homdol$m.bym.c1$summary.linear.pred$mean)
yhat_homdol_m.bes.rw.c1 <- base_homdol$esperado * exp(mods.homdol$m.bes.rw.c1$summary.linear.pred$mean)
yhat_homdol_m.bym.rw.c1 <- base_homdol$esperado * exp(mods.homdol$m.bym.rw.c1$summary.linear.pred$mean)

yhat_homdol_m.bes.k1 <- base_homdol$esperado * exp(mods.homdol$m.bes.k1$summary.linear.pred$mean)
yhat_homdol_m.bym.k1 <- base_homdol$esperado * exp(mods.homdol$m.bym.k1$summary.linear.pred$mean)
yhat_homdol_m.bes.rw.k1 <- base_homdol$esperado * exp(mods.homdol$m.bes.rw.k1$summary.linear.pred$mean)
yhat_homdol_m.bym.rw.k1 <- base_homdol$esperado * exp(mods.homdol$m.bym.rw.k1$summary.linear.pred$mean)

yhat_homdol_m.bes.k2 <- base_homdol$esperado * exp(mods.homdol$m.bes.k2$summary.linear.pred$mean)
yhat_homdol_m.bym.k2 <- base_homdol$esperado * exp(mods.homdol$m.bym.k2$summary.linear.pred$mean)
yhat_homdol_m.bes.rw.k2 <- base_homdol$esperado * exp(mods.homdol$m.bes.rw.k2$summary.linear.pred$mean)
yhat_homdol_m.bym.rw.k2 <- base_homdol$esperado * exp(mods.homdol$m.bym.rw.k2$summary.linear.pred$mean)

yhat_homdol_m.bes.k3 <- base_homdol$esperado * exp(mods.homdol$m.bes.k3$summary.linear.pred$mean)
yhat_homdol_m.bym.k3 <- base_homdol$esperado * exp(mods.homdol$m.bym.k3$summary.linear.pred$mean)
yhat_homdol_m.bes.rw.k3 <- base_homdol$esperado * exp(mods.homdol$m.bes.rw.k3$summary.linear.pred$mean)
yhat_homdol_m.bym.rw.k3 <- base_homdol$esperado * exp(mods.homdol$m.bym.rw.k3$summary.linear.pred$mean)

# FURTO DE VEICULOS #
yhat_furtvei_m.bes.c1 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.c1$summary.linear.pred$mean)
yhat_furtvei_m.bym.c1 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.c1$summary.linear.pred$mean)
yhat_furtvei_m.bes.rw.c1 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.rw.c1$summary.linear.pred$mean)
yhat_furtvei_m.bym.rw.c1 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.rw.c1$summary.linear.pred$mean)

yhat_furtvei_m.bes.k1 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.k1$summary.linear.pred$mean)
yhat_furtvei_m.bym.k1 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.k1$summary.linear.pred$mean)
yhat_furtvei_m.bes.rw.k1 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.rw.k1$summary.linear.pred$mean)
yhat_furtvei_m.bym.rw.k1 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.rw.k1$summary.linear.pred$mean)

yhat_furtvei_m.bes.k2 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.k2$summary.linear.pred$mean)
yhat_furtvei_m.bym.k2 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.k2$summary.linear.pred$mean)
yhat_furtvei_m.bes.rw.k2 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.rw.k2$summary.linear.pred$mean)
yhat_furtvei_m.bym.rw.k2 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.rw.k2$summary.linear.pred$mean)

yhat_furtvei_m.bes.k3 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.k3$summary.linear.pred$mean)
yhat_furtvei_m.bym.k3 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.k3$summary.linear.pred$mean)
yhat_furtvei_m.bes.rw.k3 <- base_furtvei$esperado * exp(mods.furtvei$m.bes.rw.k3$summary.linear.pred$mean)
yhat_furtvei_m.bym.rw.k3 <- base_furtvei$esperado * exp(mods.furtvei$m.bym.rw.k3$summary.linear.pred$mean)

# EXTORSAO MEDIANTE SEQUESTRO #
yhat_extoms_m.bes.c1 <- base_extoms$esperado * exp(mods.extoms$m.bes.c1$summary.linear.pred$mean)
yhat_extoms_m.bym.c1 <- base_extoms$esperado * exp(mods.extoms$m.bym.c1$summary.linear.pred$mean)
yhat_extoms_m.bes.rw.c1 <- base_extoms$esperado * exp(mods.extoms$m.bes.rw.c1$summary.linear.pred$mean)
yhat_extoms_m.bym.rw.c1 <- base_extoms$esperado * exp(mods.extoms$m.bym.rw.c1$summary.linear.pred$mean)

yhat_extoms_m.bes.k1 <- base_extoms$esperado * exp(mods.extoms$m.bes.k1$summary.linear.pred$mean)
yhat_extoms_m.bym.k1 <- base_extoms$esperado * exp(mods.extoms$m.bym.k1$summary.linear.pred$mean)
yhat_extoms_m.bes.rw.k1 <- base_extoms$esperado * exp(mods.extoms$m.bes.rw.k1$summary.linear.pred$mean)
yhat_extoms_m.bym.rw.k1 <- base_extoms$esperado * exp(mods.extoms$m.bym.rw.k1$summary.linear.pred$mean)

yhat_extoms_m.bes.k2 <- base_extoms$esperado * exp(mods.extoms$m.bes.k2$summary.linear.pred$mean)
yhat_extoms_m.bym.k2 <- base_extoms$esperado * exp(mods.extoms$m.bym.k2$summary.linear.pred$mean)
yhat_extoms_m.bes.rw.k2 <- base_extoms$esperado * exp(mods.extoms$m.bes.rw.k2$summary.linear.pred$mean)
yhat_extoms_m.bym.rw.k2 <- base_extoms$esperado * exp(mods.extoms$m.bym.rw.k2$summary.linear.pred$mean)

yhat_extoms_m.bes.k3 <- base_extoms$esperado * exp(mods.extoms$m.bes.k3$summary.linear.pred$mean)
yhat_extoms_m.bym.k3 <- base_extoms$esperado * exp(mods.extoms$m.bym.k3$summary.linear.pred$mean)
yhat_extoms_m.bes.rw.k3 <- base_extoms$esperado * exp(mods.extoms$m.bes.rw.k3$summary.linear.pred$mean)
yhat_extoms_m.bym.rw.k3 <- base_extoms$esperado * exp(mods.extoms$m.bym.rw.k3$summary.linear.pred$mean)