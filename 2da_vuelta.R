library(tidyverse)
library(forecast)
library(zoo)
library(tseries)


############################# DIAS #############################################

# 31 DE MAYO = 1
# 1 DE JUNIO = 2
# 2 DE JUNIO = 3
# 3 DE JUNIO = 4
# 4 DE JUNIO = 5
# 5 DE JUNIO = 6
# 6 DE JUNIO = 7
# 7 DE JUNIO = 8
# 8 DE JUNIO = 9
# 9 DE JUNIO = 10
# 10 DE JUNIO = 11
# 11 DE JUNIO = 12

########################## TRACKING PRESIDENCIAL #################################

# Ingreso diario de datos del tracking presidencial
TP = data.frame(dia =  c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11),
                RH = c(52.4, 52.3, 50.4, 47.9, 48.1, 47.8, 47.1, 46.7, 46.8, 47.9), #media movil
                GP = c(44.8, 45.1, 45.6, 46.3, 46.8, 46.8, 47.8, 48.5, 48.1, 47.1),
                VB = c(2.7, 2.6, 4, 5.8, 5.1, 5.4, 5.1, 4.9, 5.1, 5),
                ERR = c(2.9, 2.4, 2.1, 1.9, 1.7, 1.7, 1.6, 1.4, 1.4, 1.4),
                muestra = c(1200, 1755, 2308, 2840, 3240, 3641, 4041, 4438, 4836, 5236)) #muestra acumulada 




# Mutate intervalo basado en el margen de error
TP = TP %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)
  
# df con medias generales
traM = data.frame(RH = round(mean(TP$RH), 2), 
                  GP = round(mean(TP$GP), 2), 
                  VB = round(mean(TP$VB), 2), 
                  II_RH = round(mean(TP$II_RH), 2), 
                  IS_RH = round(mean(TP$IS_RH), 2), 
                  II_GP = round(mean(TP$II_GP), 2), 
                  IS_GP = round(mean(TP$IS_GP), 2), 
                  II_VB = round(mean(TP$II_VB), 2), 
                  IS_VB = round(mean(TP$IS_VB), 2))

##################### MODELO ARIMA CON TRACKING POLLS GAD3 #########################

## RODOLFO HERNANDEZ
TS_RH = as.ts(TP$RH)
RH_TS = ts(TS_RH,
             start = 1,
           frequency = 1)

ajuste_RH = auto.arima(y = RH_TS)

#summary(ajuste_RH)

pred_RH = forecast(ajuste_RH)
RH_FCS = data.frame(intencion = 'RH',
                    minimo = round(min(pred_RH[['lower']]), 2), 
                    maximo = round(max(pred_RH[['upper']]), 2))

## GUSTAVO PETRO
TS_GP = as.ts(TP$GP)

GP_TS = ts(TS_GP,
           start = 1,
           frequency =1)

ajuste_GP = auto.arima(y = GP_TS)
#summary(ajuste_GP)

pred_GP = forecast(ajuste_GP)
GP_FCS = data.frame(intencion = 'GP',
                    minimo = round(min(pred_GP[['lower']]), 2), 
                    maximo = round(max(pred_GP[['upper']]), 2))

## VOTO EN BLANCO

TS_VB = as.ts(TP$VB)

VB_TS = ts(TS_VB,
           start = 1,
           frequency = 1)

ajuste_VB = auto.arima(y = VB_TS)
#summary(ajuste_VB)

pred_VB = forecast(ajuste_VB)
VB_FCS = data.frame(intencion = 'VB',
                    minimo = round(min(pred_VB[['lower']]), 2), 
                    maximo = round(max(pred_VB[['upper']]), 2))

## RESUMEN FORECAST TODAS LAS SERIES 
ST2DA = rbind(RH_FCS, GP_FCS , VB_FCS)


#################### ENCUESTAS CENTRO NACIONAL DE CONSULTORIA #####################

# Ingreso de datos encuesta CNC
CNC = data.frame(dia = c(1, 3),
                 RH = c(41, 41),
                 GP = c(39, 44.9),
                 VB = c(5, 3),
                 IDS = c(14, 9.4),
                 NIN = c(1, 1.7),
                 ERR = c(2.8, 2.1),
                 muestra = c(1200, 2172))

# Mutate intervalos basados en el margen de error
CNC = CNC %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR,
         II_IDS = IDS - ERR,
         IS_IDS = IDS + ERR,
         II_NIN = NIN - ERR,
         IS_NIN = NIN + ERR)

# df de medias generales
CNCM = data.frame(RH = round(mean(CNC$RH), 2), 
                  GP = round(mean(CNC$GP), 2), 
                  VB = round(mean(CNC$VB), 2), 
                  IDS = round(mean(CNC$IDS), 2), 
                  NIN = round(mean(CNC$NIN), 2), 
                  II_RH = round(mean(CNC$II_RH), 2), 
                  IS_RH = round(mean(CNC$IS_RH), 2), 
                  II_GP = round(mean(CNC$II_GP), 2), 
                  IS_GP = round(mean(CNC$IS_GP), 2), 
                  II_VB = round(mean(CNC$II_VB), 2), 
                  IS_VB = round(mean(CNC$IS_VB), 2), 
                  II_IDS = round(mean(CNC$II_IDS), 2), 
                  IS_IDS = round(mean(CNC$IS_IDS), 2), 
                  II_NIN = round(mean(CNC$II_NIN), 2), 
                  IS_NIN = round(mean(CNC$IS_NIN), 2))

############################## GUARUMO ################################


# Ingreso de datos encuesta CNC
GUA = data.frame(dia = c(5, 10),
                 RH = c(46.4, 48.2),
                 GP = c(43.3, 46.5),
                 VB = c(8.4, 5.3),
                 ERR = c(2.5, 2.5),
                 IDS = c(1.9, 0),
                 muestra = c(1958, 2029))

# Mutate intervalos basados en el margen de error
GUA = GUA %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
GUAM = data.frame(RH = round(mean(GUA$RH), 2), 
                  GP = round(mean(GUA$GP), 2), 
                  VB = round(mean(GUA$VB), 2), 
                  II_RH = round(mean(GUA$II_RH), 2), 
                  IS_RH = round(mean(GUA$IS_RH), 2), 
                  II_GP = round(mean(GUA$II_GP), 2), 
                  IS_GP = round(mean(GUA$IS_GP), 2), 
                  II_VB = round(mean(GUA$II_VB), 2), 
                  IS_VB = round(mean(GUA$IS_VB), 2))

############################## YANHAAS ################################


# Ingreso de datos encuesta CNC
YAN = data.frame(dia = c(4, 11),
                 RH = c(41, 35),
                 GP = c(42, 45),
                 VB = c(13, 13),
                 ERR = c(3.2, 3.2),
                 IDS = c(5, 7),
                 muestra = c(1234, 1234))

# Mutate intervalos basados en el margen de error
YAN = YAN %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
YANM = data.frame(RH = round(mean(YAN$RH), 2), 
                  GP = round(mean(YAN$GP), 2), 
                  VB = round(mean(YAN$VB), 2), 
                  II_RH = round(mean(YAN$II_RH), 2), 
                  IS_RH = round(mean(YAN$IS_RH), 2), 
                  II_GP = round(mean(YAN$II_GP), 2), 
                  IS_GP = round(mean(YAN$IS_GP), 2), 
                  II_VB = round(mean(YAN$II_VB), 2), 
                  IS_VB = round(mean(YAN$IS_VB), 2))

############################## DATEXCO ################################


# Ingreso de datos encuesta CNC
DAT = data.frame(dia = c(4),
                 RH = c(46),
                 GP = c(42),
                 VB = c(0),
                 ERR = c(3.7),
                 IDS = c(12),
                 muestra = c(700))

# Mutate intervalos basados en el margen de error
DAT = DAT %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
DATM = data.frame(RH = round(mean(DAT$RH), 2), 
                  GP = round(mean(DAT$GP), 2), 
                  VB = round(mean(DAT$VB), 2), 
                  II_RH = round(mean(DAT$II_RH), 2), 
                  IS_RH = round(mean(DAT$IS_RH), 2), 
                  II_GP = round(mean(DAT$II_GP), 2), 
                  IS_GP = round(mean(DAT$IS_GP), 2), 
                  II_VB = round(mean(DAT$II_VB), 2), 
                  IS_VB = round(mean(DAT$IS_VB), 2))


############################## MASSIVECALLER ################################


# Ingreso de datos encuesta CNC
MC = data.frame(dia = c(2, 10),
                 RH = c(55.4, 53.8),
                 GP = c(44.6, 46.2),
                 VB = c(0, 0),
                 ERR = c(3.4, 3.4),
                 IDS = c(0, 0),
                 muestra = c(1000, 1000))

# Mutate intervalos basados en el margen de error
MC = MC %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
MCM = data.frame(RH = round(mean(MC$RH), 2), 
                  GP = round(mean(MC$GP), 2), 
                  VB = round(mean(MC$VB), 2), 
                  II_RH = round(mean(MC$II_RH), 2), 
                  IS_RH = round(mean(MC$IS_RH), 2), 
                  II_GP = round(mean(MC$II_GP), 2), 
                  IS_GP = round(mean(MC$IS_GP), 2), 
                  II_VB = round(mean(MC$II_VB), 2), 
                  IS_VB = round(mean(MC$IS_VB), 2))

############################## INVAMER ################################


# Ingreso de datos encuesta CNC
IV = data.frame(dia = c(8),
                RH = c(48.2),
                GP = c(47.2),
                VB = c(4.7),
                ERR = c(2.69),
                IDS = c(0),
                muestra = c(2000))

# Mutate intervalos basados en el margen de error
IV = IV %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
IVM = data.frame(RH = round(mean(IV$RH), 2), 
                 GP = round(mean(IV$GP), 2), 
                 VB = round(mean(IV$VB), 2), 
                 II_RH = round(mean(IV$II_RH), 2), 
                 IS_RH = round(mean(IV$IS_RH), 2), 
                 II_GP = round(mean(IV$II_GP), 2), 
                 IS_GP = round(mean(IV$IS_GP), 2), 
                 II_VB = round(mean(IV$II_VB), 2), 
                 IS_VB = round(mean(IV$IS_VB), 2))

############################## MOSQUETEROS ################################


# Ingreso de datos encuesta CNC
MOS = data.frame(dia = c(9),
                RH = c(44.68),
                GP = c(43.83),
                VB = c(4.54),
                ERR = c(7.95),
                IDS = c(0),
                muestra = c(6000))

# Mutate intervalos basados en el margen de error
MOS = MOS %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
MOSM = data.frame(RH = round(mean(MOS$RH), 2), 
                 GP = round(mean(MOS$GP), 2), 
                 VB = round(mean(MOS$VB), 2), 
                 II_RH = round(mean(MOS$II_RH), 2), 
                 IS_RH = round(mean(MOS$IS_RH), 2), 
                 II_GP = round(mean(MOS$II_GP), 2), 
                 IS_GP = round(mean(MOS$IS_GP), 2), 
                 II_VB = round(mean(MOS$II_VB), 2), 
                 IS_VB = round(mean(MOS$IS_VB), 2))

############################## ATLAS INTEL ################################


# Ingreso de datos encuesta CNC
AIN = data.frame(dia = c(12),
                 RH = c(50.2),
                 GP = c(47.5),
                 VB = c(2.4),
                 ERR = c(1),
                 IDS = c(0),
                 muestra = c(4467))

# Mutate intervalos basados en el margen de error
AIN = AIN %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
AINM = data.frame(RH = round(mean(MOS$RH), 2), 
                  GP = round(mean(MOS$GP), 2), 
                  VB = round(mean(MOS$VB), 2), 
                  II_RH = round(mean(MOS$II_RH), 2), 
                  IS_RH = round(mean(MOS$IS_RH), 2), 
                  II_GP = round(mean(MOS$II_GP), 2), 
                  IS_GP = round(mean(MOS$IS_GP), 2), 
                  II_VB = round(mean(MOS$II_VB), 2), 
                  IS_VB = round(mean(MOS$IS_VB), 2))


############################## DATA ANALISIS ################################


# Ingreso de datos encuesta CNC
DANA = data.frame(dia = c(10),
                 RH = c(45.9),
                 GP = c(43.6),
                 VB = c(5.6),
                 ERR = c(2.18),
                 IDS = c(4.9),
                 muestra = c(2024))

# Mutate intervalos basados en el margen de error
DANA = DANA %>%
  mutate(II_RH = RH - ERR,
         IS_RH = RH + ERR,
         II_GP = GP - ERR,
         IS_GP = GP + ERR,
         II_VB = VB - ERR,
         IS_VB = VB + ERR)

# df de medias generales
DANAM = data.frame(RH = round(mean(DANA$RH), 2), 
                  GP = round(mean(DANA$GP), 2), 
                  VB = round(mean(DANA$VB), 2), 
                  II_RH = round(mean(DANA$II_RH), 2), 
                  IS_RH = round(mean(DANA$IS_RH), 2), 
                  II_GP = round(mean(DANA$II_GP), 2), 
                  IS_GP = round(mean(DANA$IS_GP), 2), 
                  II_VB = round(mean(DANA$II_VB), 2), 
                  IS_VB = round(mean(DANA$IS_VB), 2))

########################### GENERAL TODOS ####################################


todo = rbind(TP %>% mutate(FIRMA = 'TRACKING'), 
             cbind(CNC[1:4], CNC[7:14]) %>% mutate(FIRMA = 'CNC'), 
             cbind(GUA[1:5], GUA[7:13]) %>% mutate(FIRMA = 'GUARUMO'),
             cbind(YAN[1:5], YAN[7:13]) %>% mutate(FIRMA = 'YANHAAS'),
             cbind(DAT[1:5], DAT[7:13]) %>% mutate(FIRMA = 'DATEXCO'),
             cbind(MC[1:5], MC[7:13]) %>% mutate(FIRMA = 'MASSIVECALLER'),
             cbind(IV[1:5], IV[7:13]) %>% mutate(FIRMA = 'INVAMER'),
             cbind(MOS[1:5], MOS[7:13]) %>% mutate(FIRMA = 'MOSQUETEROS'),
             cbind(AIN[1:5], AIN[7:13]) %>% mutate(FIRMA = 'ATLAS INTEL'),
             cbind(DANA[1:5], DANA[7:13]) %>% mutate(FIRMA = 'DATA ANALISIS'))



todo = todo %>%
  arrange(dia)


TODOM = data.frame(RH = round(mean(todo$RH), 2), 
                  GP = round(mean(todo$GP), 2), 
                  VB = round(mean(todo$VB), 2))

IC_RH = round(1.96*(sd(todo$RH)/sqrt(length(todo$RH))), 3)
IC_GP = round(1.96*(sd(todo$GP)/sqrt(length(todo$GP))), 3)
IC_VB = round(1.96*(sd(todo$VB)/sqrt(length(todo$VB))), 3)

medias = data.frame(intencion = c('RH', 'GP', 'VB'),
                       Media = c(TODOM$RH, TODOM$GP, TODOM$VB),
                        SD = c(round(sd(todo$RH), 3), round(sd(todo$GP), 3), round(sd(todo$VB), 3)),
                       IC = c(IC_RH, IC_GP, IC_VB))

######################## PROMEDIOS DE CADA DIA #############################################
promedio = data.frame()

a = unique(todo$dia)

for (i in a){
  print(i)
  promedios[i,] = todo %>%
    filter(dia == i) %>%
    summarise(RH = mean(RH),
              GP = mean(GP),
              VB = mean(VB))
}

### Eliminando dia 6 = 5 de junio
promedios = promedios %>%
  drop_na()

promedios = promedios %>%
  mutate(dia = unique(todo$dia))

####################### SERIE TIEMPO CON PROMEDIOS DE TODAS LAS ENCUESTAS ##############


## RODOLFO HERNANDEZ
TS_PRH = as.ts(promedios$RH)
PRH_TS = ts(TS_PRH,
           start = 1,
           frequency = 1)

ajuste_PRH = auto.arima(PRH_TS)
pred_PRH = forecast(ajuste_PRH)

PRH_FCS = data.frame(intencion = 'RH',
                    minimo = round(min(pred_PRH[['lower']]), 2), 
                    maximo = round(max(pred_PRH[['upper']]), 2))

## GUSTAVO PETRO
TS_PGP = as.ts(promedios$GP)
PGP_TS = ts(TS_PGP,
            start = 1,
            frequency = 1)

ajuste_PGP = auto.arima(y = PGP_TS)
pred_PGP = forecast(ajuste_PGP)
PGP_FCS = data.frame(intencion = 'GP',
                     minimo = round(min(pred_PGP[['lower']]), 2), 
                     maximo = round(max(pred_PGP[['upper']]), 2))


## VOTO EN BLANCO
TS_PVB = as.ts(promedios$VB)
PVB_TS = ts(TS_PVB,
            start = 1,
            frequency = 1)

ajuste_PVB = auto.arima(y = PVB_TS)
pred_PVB = forecast(ajuste_PVB)
PVB_FCS = data.frame(intencion = 'VB',
                     minimo = round(min(pred_PVB[['lower']]), 2), 
                     maximo = round(max(pred_PVB[['upper']]), 2))


STP = rbind(PRH_FCS, PGP_FCS , PVB_FCS)


## grafico serie de resultados promedio diario de encuestas
ggplot(data = promedios, aes(x = dia))+
  geom_line(aes(y = RH, color="RH"), size=1,  linetype=1,  group = 1) +
  geom_line(aes(y = GP, color="GP"), size=1, linetype=1, group = 2) +
  geom_line(aes(y = VB, color="VB"), size=1, linetype=1, group = 3) +
  
  geom_point(aes(y = RH), color="#FF0400", size=2, group = 1) +
  geom_point(aes(y = GP), color="#008000", size=2, group = 2) +
  geom_point(aes(y = VB), color="#0300FF", size=2, group = 3) +
  
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(size=12, face='bold', hjust = 0.5)) +
  labs(y = "INTENCION DE VOTO", x = "", color = "", title = 'INTENCION DE VOTO 2DA VUELTA') 
  

############################ MODELOS PROBABILISTICOS ########################################
library(car)
library(rriskDistributions)
fit.cont(todo$VB)

## PRUEBA DE NORMALIDAD
shapiro.test(todo$GP)
shapiro.test(todo$RH)

## CALCULO PROBABILIDAD DE VOTOS DE ACUERDO A DISTRIBUCION NORMAL
(pnorm(48,mean = mean(todo$RH), sd = sd(todo$RH), lower.tail = F))*100
(pnorm(47,mean = mean(todo$GP), sd = sd(todo$GP), lower.tail = F))*100

## CALCULO PROBABILIDAD DE VOTOS EN BLANCO DISTRIBUCION CAUCHY
(pcauchy(5, 4.923271, 0.859814, lower.tail = F)*100)


################### SIMULACIONES ##########

n=8000           # tamaño muestral
M=10000         # cantidad de muestras

## Simulacion intencion de voto Rodolfo Hernandez


Xbar_RH=rep(NA,M)                   # iniciar vector de medias muestrales
for(i in 1:M){
  X=rnorm(n,mean = mean(todo$RH),sd = sd(todo$RH)) # muestra aleatoria de tama?o n de una 
  # distribucion normal con media mu, 
  # desviacion estandar sigma 
  Xbar_RH[i]=mean(X)                 # media de la muestra
}

E_RH = mean(Xbar_RH)

hist(Xbar_RH, freq = F)

pXbar_RH <- dnorm(Xbar_RH,mean(todo$RH),sd(todo$RH)/sqrt(n))
points(Xbar_RH,pXbar_RH,pch='.',col=2)


## Simulacion intencion de voto Gustavo Petro

Xbar_GP=rep(NA,M)                   # iniciar vector de medias muestrales
for(i in 1:M){
  X=rnorm(n,mean = mean(todo$GP),sd = sd(todo$GP)) # muestra aleatoria de tama?o n de una 
  # distribucion normal con media mu, 
  # desviacion estandar sigma 
  Xbar_GP[i]=mean(X)                 # media de la muestra
}

E_GP = mean(Xbar_GP)

hist(Xbar_GP, freq = F)

pXbar_GP <- dnorm(Xbar_GP,mean(todo$GP),sd(todo$GP)/sqrt(n))
points(Xbar_GP,pXbar_GP,pch='.',col=2)


## Simulacion voto en blanco

Xbar_VB=rep(NA,M)                   # iniciar vector de medias muestrales
for(i in 1:M){
  X=rnorm(n,mean = mean(todo$VB),sd = sd(todo$VB)) # muestra aleatoria de tama?o n de una 
  # distribucion normal con media mu, 
  # desviacion estandar sigma 
  Xbar_VB[i]=mean(X)                 # media de la muestra
}

E_VB = mean(Xbar_VB)

hist(Xbar_VB, freq = F)

pXbar_VB <- dnorm(Xbar_VB,mean(todo$VB),sd(todo$VB)/sqrt(n))
points(Xbar_VB,pXbar_VB,pch='.',col=2)


E_RH + E_GP + E_VB


Xbar_RH=rep(NA,M)                   # iniciar vector de medias muestrales
for(i in 1:M){
  X=rbinom(n, 1, prob = mean(todo$RH)/100) # muestra aleatoria de tama?o n de una 
  # distribucion normal con media mu, 
  # desviacion estandar sigma 
  Xbar_RH[i]=mean(X)                 # media de la muestra
}

E_RH = mean(Xbar_RH)

hist(Xbar_RH, freq = F)


#Intervalos de confianza simulaciones
ICS_RH = round(1.96*(sd(Xbar_RH)/sqrt(M)), 3)
ICS_GP = round(1.96*(sd(Xbar_GP)/sqrt(M)), 3)
ICS_VB = round(1.96*(sd(Xbar_VB)/sqrt(M)), 3)

simulaciones = data.frame(intencion = c('RH', 'GP', 'VB'),
                          media = c(round(E_RH, 3), round(E_GP, 3), round(E_VB, 3)),
                          SD = c(round(sd(Xbar_RH), 3), round(sd(Xbar_GP), 3), round(sd(Xbar_VB), 3)),
                          IC = c(ICS_RH, ICS_GP, ICS_VB))


############################3 FINALES ##################

# medias y simulaciones
simulaciones
sum(simulaciones$media)
medias
sum(medias$Media)

# series de tiempo
ST2DA # tracking
STP # promedio diario todas las encuestas

# predicciones serie de tiempo tracking GAD3
pred_RH
pred_GP
pred_GP

mean(pred_RH$fitted) + mean(pred_GP$fitted) + mean(pred_VB$fitted)

# predicciones serie de tiempo promedio diario encuestas
pred_PRH
pred_PGP
pred_PVB

mean(pred_PRH$fitted) + mean(pred_PGP$fitted) + mean(pred_PVB$fitted)





