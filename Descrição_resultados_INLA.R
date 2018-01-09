#setwd("C:/Users/renan/Desktop/Projeto Tese")
setwd("C:/Users/Windows 8.1/Desktop/Projeto Tese")

require(tidyverse)
require(ggplot2)
require(xtable)
require(stringr) # Função que deixa a primeira maiuscula str_to_title

load("Estimativas_de_Ocorrencia_modelos.Rdata")
load("bases_para_estimar_y.Rdata")

corresp <- readRDS("Corresp_Mun_PopRS.rds")
corresp$CodIBGE <- as.factor(corresp$CodIBGE) # Para pode 'joinear' depois
corresp_aux <- corresp %>% select(CodIBGE, Cidade)



base_aux <- tbl_df(base_roubo)
base_aux2 <- base_aux %>%
  select(GEOCODIG_M, Ano, Pop, i) %>%
  mutate(Roubo_INLA = yhat_roubo_m.bym.c1,     Roubo_Bruto = base_roubo$y,
         RouboVei_INLA = yhat_roubovei_m.bym.c1,   RouboVei_Bruto = base_roubovei$y,
         Latro_INLA = yhat_latro_m.bym.c1,          Latro_Bruto = base_latro$y,
         Furto_INLA = yhat_furt_m.bes.rw.k3,        Furto_Bruto = base_furt$y,
         HomDol_INLA = yhat_homdol_m.bym.c1,       HomDol_Bruto = base_homdol$y,
         FurtoVei_INLA = yhat_furtvei_m.bym.rw.c1, FurtoVei_Bruto = base_furtvei$y,
         Exto_INLA = yhat_extoms_m.bym.c1     , Exto_Bruto = base_extoms$y) %>%
  left_join(corresp_aux, by = c("GEOCODIG_M" = "CodIBGE"))

# Gráfico comparativo Bruto vs. INLA
aux_1 <- base_aux2 %>%
  gather(Tipo, Valor_INLA, Roubo_INLA, RouboVei_INLA, Latro_INLA, Furto_INLA, HomDol_INLA, FurtoVei_INLA, Exto_INLA) %>%
  select(Tipo, Valor_INLA)

aux_2 <- base_aux2 %>% # Direita
  gather(Tipo2, Valor_Bruto, Roubo_Bruto, RouboVei_Bruto, Latro_Bruto, Furto_Bruto, HomDol_Bruto, FurtoVei_Bruto, Exto_Bruto) %>%
  select(Tipo2, Valor_Bruto)

aux3 <- bind_cols(aux_1, aux_2) %>% separate(col = Tipo, into = c("Crime", "Método"), sep="_") %>%
        select(Crime, Valor_INLA, Valor_Bruto)

ggplot(aux3, aes(x = Valor_INLA, y = Valor_Bruto)) + 
  geom_point() +
  facet_wrap(~ Crime, scales="free") # Cada escala se ajusta




# plot(2002:2015,filter(base_aux2, Cidade == "SAO LEOPOLDO")$HomDol_INLA)

#hd <- tbl_df(cbind(base_homdol, yhat_homdol_m.bym.c1))
#hd %>% filter(GEOCODIG_M == 4307401) # Esmeralda (deu uma bela suavizada em 2015!)
#hd %>% filter(GEOCODIG_M == 4318465) # Sao Jose do Herval em 2013
#hd %>% filter(GEOCODIG_M == 4313334) # Nova Ramada em 2012, pela vizinhança e comparando com 2015, ficou fantástico

# VIDENTE DUTRA É UM CASO INTERESSANTE DE SE COMPARAR 2003 E 2014
# Compar a também cidades da RMPA, se der, pra não ficar de fora...

# Problema
#plot(2002:2015,filter(base_aux2, Cidade == "PORTO ALEGRE")$Hom_Dol)

w_INLA <- 2
w_Bruto <- 1
ww <- w_INLA + w_Bruto

# Calculando o número de ocorrências combinando INLA e o dado bruto
base_aux3 <- base_aux2 %>%
             mutate(Roubo_O = (Roubo_INLA * w_INLA + Roubo_Bruto * w_Bruto) / ww,
                    RouboVei_O = (RouboVei_INLA * w_INLA + RouboVei_Bruto * w_Bruto) / ww,
                    Latro_O = (Latro_INLA * w_INLA + Latro_Bruto * w_Bruto) / ww,
                    Furto_O = (Furto_INLA * w_INLA + Furto_Bruto * w_Bruto) / ww,
                    HomDol_O = (HomDol_INLA * w_INLA + HomDol_Bruto * w_Bruto) / ww,
                    FurtoVei_O = (FurtoVei_INLA * w_INLA + FurtoVei_Bruto * w_Bruto) / ww,
                    Exto_O = (Exto_INLA * w_INLA + Exto_Bruto * w_Bruto) / ww) %>%
             select(GEOCODIG_M, Ano, Pop, i, Cidade, Roubo_O, RouboVei_O, Latro_O, Furto_O, HomDol_O, FurtoVei_O, Exto_O,
                    HomDol_Bruto, Latro_Bruto)


# Homicídio: Esmeralda, São José do Herval,São José do Inhacorá

cid_hom <- c("ESMERALDA", "SAO JOSE DO HERVAL", "SAO JOSE DO INHACORA")
aux <- base_aux3 %>% filter(Cidade %in% cid_hom) %>% gather(Tipo, Quantidade, HomDol_O, HomDol_Bruto)
ggplot(aux, aes(x = 2001+Ano, y = Quantidade, col = Tipo)) + geom_line(size= 1.1) +
  facet_wrap(~ Cidade) +
  ggtitle("Homicídios Comparativos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Teste")


# Latrocínio: Riozinho e Muliterno

cid_latro <- c("RIOZINHO", "MULITERNO")
aux <- base_aux3 %>% filter(Cidade %in% cid_latro) %>% gather(Tipo, Quantidade, Latro_O, Latro_Bruto)
ggplot(aux, aes(x=Ano, y=Quantidade, col=Tipo)) + geom_line(size= 1.1) +
  facet_wrap(~Cidade) +
  ggtitle("Latrocínios Comparativos") +
  theme(plot.title = element_text(hjust = 0.5))

#plot(2002:2015,filter(base_aux3, Cidade == "PORTO ALEGRE")$HomDol_O)
#plot(2002:2015,filter(base_aux3, Cidade == "ALVORADA")$Latro_O)

# Multiplicando pelas penas e somando (Tabela do artigo): 
# 


base_aux4 <- base_aux3 %>%
             mutate(Numerador = Roubo_O * 4 +
                                RouboVei_O * 4 +
                                Latro_O * 20 +
                                Furto_O * 1 +
                                HomDol_O  * 6 +
                                FurtoVei_O * 1 +
                                Exto_O * 8,
                    Numerador_Vida = Latro_O * 20 +
                                     HomDol_O  * 6,
                    Numerador_Patri = Roubo_O * 4 +
                                      RouboVei_O * 4 +
                                      Furto_O * 1 +
                                      FurtoVei_O * 1 +
                                      Exto_O * 8,
                    ICrime = Numerador/Pop * 365,
                    Indice_Vida = Numerador_Vida/Pop * 365,
                    Indice_Patri = Numerador_Patri/Pop * 365) %>% # Interpretação do indicador: Número de Dias esperado que cada habitante teria que pagar por ano caso todos fossem condenados a pena mínima
              select(GEOCODIG_M, Ano, Pop, i, Cidade, ICrime, Indice_Vida, Indice_Patri)


#plot(2002:2015,filter(base_aux4, Cidade == "PORTO ALEGRE")$Indice)
#plot(2002:2015,filter(base_aux4, Cidade == "CAXIAS DO SUL")$Indice) # Caxias não tá tão parecido com Roubo ou Furto
#plot(2002:2015,filter(base_aux4, Cidade == "BAGE")$Indice) # Um bom exemplo também
#plot(2002:2015,filter(base_aux4, Cidade == "MONTENEGRO")$Indice)

# TOP 10 CIDADES #
cidades <- c("PORTO ALEGRE", "CAXIAS DO SUL", "CANOAS", "PELOTAS", "SANTA MARIA", "GRAVATAI", "VIAMAO", "NOVO HAMBURGO", "SAO LEOPOLDO", "ALVORADA")
tb <- base_aux4 %>% filter(Cidade %in% cidades)

tb_top_10 <- gather(tb, Tipo_Indice, Valor, ICrime, Indice_Vida, Indice_Patri)
ggplot(tb_top_10, aes(x=Ano+2001, y = Valor, col=Cidade)) + geom_line(size=1.1) +
  facet_wrap(~Tipo_Indice, scales="free") +
  xlab("Anos") +
  theme(legend.position="bottom",
        legend.text=element_text(size=6),
        legend.title=element_blank())


# TOP 10 CIDADES ESTRANHAS #
cidades <- c("ESMERALDA", "ENTRE RIOS DO SUL", "PONTE PRETA", "SAO JOSE DO HERVAL", "SAO JOSE DO INHACORA", "RIOZINHO", "MULITERNO", "GRAMADO DOS LOUREIROS", "NICOLAU VERGUEIRO", "NOVA RAMADA") # GRAMADO DOS LOUREIROS FOI LATRO EM 2010 E NICOLAU FOI LATRO EM 2009
tb2 <- base_aux4 %>% filter(Cidade %in% cidades)

tb_low_10 <- gather(tb2, Tipo_Indice, Valor, ICrime, Indice_Vida, Indice_Patri)
ggplot(tb_low_10, aes(x=Ano+2001, y = Valor, col=Cidade)) + geom_line(size=1.1) +
  facet_wrap(~Tipo_Indice, scales="free") +
  xlab("Anos") +
  theme(legend.position="bottom",
        legend.text=element_text(size=6),
        legend.title=element_blank())


# Rankings
arrange(base_aux4, desc(ICrime))
arrange(base_aux4, desc(Indice_Vida))
arrange(base_aux4, desc(Indice_Patri))

# Para anos específicos
arrange(filter(base_aux4,Ano==14), desc(Indice_Vida))
arrange(filter(base_aux4,Ano==10), desc(Indice_Vida))


# Índios e Agricultores em Vicente Dutra

# PAra poder estabilizar o índice_Vida tem que ser radicale dar peso 1 pro INLA.. tava difícil de estabilizar muitos municípios....

pos <- c(1:10, 486:496) # Top 10 e Low 10

rank_ICrime <- data.frame(
  
  Posicao = paste0(c(1:10, 486:496),"º"),
  
  g_2006 = str_to_title(arrange(filter(base_aux4,Ano==5), desc(ICrime))$Cidade[pos]),
  g_2009 = str_to_title(arrange(filter(base_aux4,Ano==8), desc(ICrime))$Cidade[pos]),
  g_2012 = str_to_title(arrange(filter(base_aux4,Ano==11), desc(ICrime))$Cidade[pos]),
  g_2015 = str_to_title(arrange(filter(base_aux4,Ano==14), desc(ICrime))$Cidade[pos])
  
)

xtable(rank_ICrime)




rank_vida <- data.frame(
  
  Posicao = paste0(c(1:10, 486:496),"º"),
  
  v_2006 = str_to_title(arrange(filter(base_aux4,Ano==5), desc(Indice_Vida))$Cidade[pos]),
  v_2009 = str_to_title(arrange(filter(base_aux4,Ano==8), desc(Indice_Vida))$Cidade[pos]),
  v_2012 = str_to_title(arrange(filter(base_aux4,Ano==11), desc(Indice_Vida))$Cidade[pos]),
  v_2015 = str_to_title(arrange(filter(base_aux4,Ano==14), desc(Indice_Vida))$Cidade[pos])
  
)

xtable(rank_vida)





