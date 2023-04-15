library(tidyverse)
library(haven)
library(assertthat)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)
library(MASS)
if(!require("AER")) install.packages("AER")
if(!require("ivreg")) install.packages("ivreg")
library("data.table")
library("car")
library("AER")
library("ivreg")
library(psych)
SIP<-SIP_2017_2020_EVA %>%
  filter(ano<"2020" )
parto_ant<- ifelse(SIP$gprev_partos>0 , 1,0)
cesarea_ant<- ifelse(SIP$gprev_part_ces, 1,0)
fumo<- ifelse( c(SIP$fuma_t1==1|SIP$fuma_t2==1|SIP$fuma_t3==1|SIP$fumap_t3==1
                         |SIP$fumap_t2==1|SIP$fumap_t1==1),1,0)
alcohol<- ifelse( c(SIP$alcohol_t1==1|SIP$alcohol_t2==1|SIP$alcohol_t3==1),1,0)
drogas<- ifelse( c(SIP$drogas_t1==1|SIP$drogas_t2==1|SIP$drogas_t3==1),1,0)

SIP2<-cbind.data.frame(SIP, parto_ant, cesarea_ant, fumo, alcohol, drogas)

modelo<-rlm(cesarea ~ enf_hipert +imc + edadm
           +parto_ant +cesarea_ant + abortos +publica +multiple + cp_adec_msp + fumo
           + alcohol + drogas + enf_una_o_mas  + ant_p_otro + ant_p_otro+ ant_p_ecl+ ant_p_precl
           + ant_p_hta + ant_p_dbtG + ant_p_dbtII + ant_p_dbtI + ant_f_otro
           +ant_f_dbt + ant_f_hta + ant_f_ecl +ant_f_precl , SIP2)
summary(modelo)

#Se plantea el instrumento anti_no_uso
forma_r<-lm(enf_hipert~imc + edadm + planeado + anti_no_uso
            +parto_ant +cesarea_ant + abortos +publica +multiple + cp_adec_msp + fumo + alcohol + drogas
              + enf_una_o_mas  + ant_p_otro + ant_p_otro+ ant_p_ecl+ ant_p_precl
            + ant_p_hta  + ant_p_dbtG + ant_p_dbtII + ant_p_dbtI + ant_f_otro
            +ant_f_dbt + ant_f_hta + ant_f_ecl +ant_f_precl , SIP2)
summary(forma_r)

f.conjunta<- linearHypothesis(forma_r,"anti_no_uso=0")$F[2]
v.crit.conj <- qf(0.05, 4, forma_r$df.residual, lower.tail = FALSE)
#hay evidencía estadística para rechazar H0 y aceptar relevnacia de instrumento.

#Variables instrumentales, paquete ivreg
Modelo1Inst_1 <- ivreg(cesarea ~ enf_hipert +imc + edadm + planeado 
                       +parto_ant +cesarea_ant + abortos +publica +multiple + cp_adec_msp
                       + fumo + alcohol + drogas
                       + enf_una_o_mas  + ant_p_otro + ant_p_otro+ ant_p_ecl+ ant_p_precl
                       + ant_p_hta  + ant_p_dbtG + ant_p_dbtII + ant_p_dbtI + ant_f_otro
                       +ant_f_dbt + ant_f_hta + ant_f_ecl +ant_f_precl +anti_no_uso , data = SIP2)
summary(Modelo1Inst_1)
#Estimación robusta de errores estándar
coeftest(Modelo1Inst_1, vcov. = vcovHC)


## Histograma de residuos
hist(Modelo1Inst_1$residuals, main = "Histograma de residuos del Modelo", xlab = "Residuos del Modelo")

SIP10<- read_dta("base SIP econometria_et.dta")
obesidad<-ifelse(SIP10$imc>=30, 1,0)
sip<- cbind.data.frame(SIP10,obesidad)
Nac2010<- sip %>%
  filter(ano=="2010")
Nac2011<- sip %>%
  filter(ano=="2011")
Nac2012<- sip %>%
  filter(ano=="2012")
Nac2013<- sip %>%
  filter(ano=="2013")
Nac2014<- sip %>%
  filter(ano=="2014")
Nac2015<- sip %>%
  filter(ano=="2015")
Nac2016<- sip %>%
  filter(ano=="2016")
Nac2017<- sip %>%
  filter(ano=="2017")
Nac2018<- sip %>%
  filter(ano=="2018")
Nac2019<- sip %>%
  filter(ano=="2019")
Nac2020<- sip %>%
  filter(ano=="2020")
#voy cambiando los años manualmente y recolectando la media muestral
summary(Nac2020$cesarea)
summary(Nac2020$hipert)
summary(Nac2020$obesidad)

## Bondad de ajuste: Tabla de predicciones correctas e incorrectas
prediccion <- predict.lm(Modelo1Inst_1)
cesarea_hat <- ifelse(prediccion >=0.5, 1, 0)
cesarea<-na.omit(SIP2$cesarea)
cesaream<-sample(cesarea,91008 , replace=FALSE)
addmargins(table(cesaream,cesarea_hat))
prop.table(table(cesaream,cesarea_hat),1)

#Hay un efecto diferencial en la influencia de la hipertensión sobre las cesáreas
#cuando la madre tiene sobre peso respecto a cuando no?
obs<- ifelse(SIP2$imc >=30 , 1,0) #obesidad
SIP3<-cbind.data.frame(SIP, afro, parto_ant, cesarea_ant,obs)
oxh<-obs*SIP3$enf_hipert
SIP4<-cbind.data.frame(SIP, afro, parto_ant, cesarea_ant,obs,fumo, alcohol, drogas ,oxh)
Modelo1Inst_2 <- ivreg(cesarea ~ enf_hipert +obs +oxh + edadm + planeado 
                       +parto_ant +cesarea_ant + abortos +publica +multiple + cp_adec_msp
                       + fuma_t1 + fuma_t2 +fuma_t3 + fumap_t1 + fumap_t2 + fumap_t3 + alcohol_t1
                       +alcohol_t2 +alcohol_t3 + drogas_t1 + drogas_t2 + drogas_t3 + 
                         + enf_una_o_mas  + ant_p_otro + ant_p_otro+ ant_p_ecl+ ant_p_precl
                       + ant_p_hta + afro + ant_p_dbtG + ant_p_dbtII + ant_p_dbtI + ant_f_otro
                       +ant_f_dbt + ant_f_hta + ant_f_ecl +ant_f_precl +anti_no_uso , data = SIP4)
summary(Modelo1Inst_2)
  
#Estimación robusta de errores estándar
coeftest(Modelo1Inst_2, vcov. = vcovHC)

#test de chow
ModeloR = ivreg(cesarea ~ enf_hipert +obs + edadm + planeado 
                +parto_ant +cesarea_ant + abortos +publica +multiple + cp_adec_msp
                + fuma_t1 + fuma_t2 +fuma_t3 + fumap_t1 + fumap_t2 + fumap_t3 + alcohol_t1
                +alcohol_t2 +alcohol_t3 + drogas_t1 + drogas_t2 + drogas_t3 + 
                  + enf_una_o_mas  + ant_p_otro + ant_p_otro+ ant_p_ecl+ ant_p_precl
                + ant_p_hta + afro + ant_p_dbtG + ant_p_dbtII + ant_p_dbtI + ant_f_otro
                +ant_f_dbt + ant_f_hta + ant_f_ecl +ant_f_precl +anti_no_uso , data = SIP4)

q<- Modelo1Inst_2$rank-ModeloR$rank
realizacion.f<- ((deviance(ModeloR)-deviance(Modelo1Inst_2))/q)/(deviance(Modelo1Inst_2)/Modelo1Inst_2$df.residual)
v.critic.F<- qf(0.90,q,Modelo1Inst_2$df.residual)
#Se rechaza Ho y se tiene evidencia estadística a un 90% de confianza que hay una diferecia.

SIP10<- read_dta("base SIP econometria_et.dta")
obesidad<-ifelse(SIP10$imc>=30, 1,0)
sip<- cbind.data.frame(SIP10,obesidad)
Nac2010<- sip %>%
  filter(ano=="2010")
Nac2011<- sip %>%
  filter(ano=="2011")
Nac2012<- sip %>%
  filter(ano=="2012")
Nac2013<- sip %>%
  filter(ano=="2013")
Nac2014<- sip %>%
  filter(ano=="2014")
Nac2015<- sip %>%
  filter(ano=="2015")
Nac2016<- sip %>%
  filter(ano=="2016")
Nac2017<- sip %>%
  filter(ano=="2017")
Nac2018<- sip %>%
  filter(ano=="2018")
Nac2019<- sip %>%
  filter(ano=="2019")
Nac2020<- sip %>%
  filter(ano=="2020")
#voy cambiando los años manualmente y recolectando la media muestral
summary(Nac2020$cesarea)
summary(Nac2020$hipert)
summary(Nac2020$obesidad)

fumo<- ifelse(fumo==1, c(SIP==1|fumoseg==1|fumoter==1),0)
SIP2$imc

