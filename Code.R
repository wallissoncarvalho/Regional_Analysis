library(readxl)
library(lmom)
library(lmomRFA)
library(writexl)


# ABRE PLANILHA COM AS MÁXIMAS ANUAIS, ONDE CADA COLUNA REPRESENTA UMA ESTAÇÃO
Maximas_Anuais <- read_excel("C:/Users/User/Maximas_Anuais.xlsx", sheet = "24h",
                             col_types = c("skip", "numeric", "numeric", "numeric", "numeric",
                                           "numeric","numeric", "numeric", "numeric", "numeric",
                                           "numeric", "numeric","numeric", "numeric", "numeric"))


# CÁLCULO DOS MOMENTOS-L E APLICAçÃO DA ANÁLISE DE FREQUÊNCIA REGIONAL

## Computes the sample L-moments of multiple data sets
lmo <- regsamlmu(Maximas_Anuais) 

## Computes discordancy, heterogeneity and goodness-of-fit measures for regional frequency analysis.
## These statistics are as described in Hosking and Wallis (1997, chaps. 3-5).
## nsim is the Number of simulations
sta <- regtst(lmo, nsim=10000)

## summary statistics of regional analysis of the data for the sites in a region
summary(sta)


# CÁLCULO DOS QUANTIS PARA CADA TEMPO DE RETORNO

## Return Periods Definition
tr <- c(2, 5, 10, 25,50, 100, 200, 500, 1000, 10000)
qunts <- 1-1/tr

## REGIONAL QUANTILES CALCULATION
regional_quantiles = regquant(qunts, regfit(lmo,"gev"))

## STATIONS QUANTILES CALCULATION
station_quantiles = sitequant(qunts,regfit(lmo,"gev"))

## DISPLAYING THE RESULTS
regional_quantiles
station_quantiles


# CALCULATE BOUNDS - CONFIDENCE INTERVAL
rmom <- regavlmom(lmo)
rfit <- regfit(lmo, "gev")
nsites <- nrow(lmo)
means <- rep(1,nsites)
LCVrange <- 0.025
LCVs <- seq(rmom[2]-LCVrange/2, rmom[2]+LCVrange/2, len=nsites)
Lskews<-rep(rmom[3], nsites)
pp <- t(apply(cbind(means, means*LCVs ,Lskews), 1, pelgev)) #Atenção: O último argumento deve ser em referência à distribuição escolhida, se gev: pelgev.
simq <- regsimq(qfunc=quagev, para=pp,nrec=lmo$n, nrep=1000, fit="gev",  f =qunts,
                boundprob = c(0.05, 0.95)) #Atenção: O último primeiro argumento deve ser em referência à distribuição escolhida, se gev: quagev.
regquantbounds(simq, regfit(lmo,"gev"))
sitequantbounds(simq, regfit(lmo,"gev"), 'Station Name') #Station Name refere-se a coluna de interesse