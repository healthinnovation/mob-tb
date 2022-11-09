################## 0.- LIBRARIES     #####################
library(tidyverse)
library(stats)
library(MASS)
library(jtools)
library(Matrix)
library(lme4)
library(reshape2)
library("scales")
library(ggplot2)
library(reshape2)
library(scales)
################## 1.- LOADING DATA   #####################
#C:\Users\DIEGO\Downloads\mob_tb_modelo\mob-tb\analysis\data\processed
mob_lima_exploratorio <- read.csv(file='mob-tb.csv')
mob_lima_exploratorio = filter(mob_lima_exploratorio, district != 'MAGDALENA VIEJA')
mob_lima_exploratorio$tmmean = (mob_lima_exploratorio$tmmn + mob_lima_exploratorio$tmmx) / 2
bd_bruta = mob_lima_exploratorio
################## 2.- RENAMING COLUMN NAMES ############
names(bd_bruta)[names(bd_bruta)=="alpha"]<-"ce_alpha"
names(bd_bruta)[names(bd_bruta)=="authority"]<-"ce_authority"
names(bd_bruta)[names(bd_bruta)=="betweenness"]<-"ce_betweenness"
names(bd_bruta)[names(bd_bruta)=="closeness"]<-"ce_closeness"
names(bd_bruta)[names(bd_bruta)=="hub"]<-"ce_hub"
names(bd_bruta)[names(bd_bruta)=="in_closeness"]<-"ce_in_closeness"
names(bd_bruta)[names(bd_bruta)=="degree"]<-"ce_degree"
names(bd_bruta)[names(bd_bruta)=="in_degree"]<-"ce_in_degree"
names(bd_bruta)[names(bd_bruta)=="in_strength"]<-"ce_in_strength"
names(bd_bruta)[names(bd_bruta)=="out_closeness"]<-"ce_out_closeness"
names(bd_bruta)[names(bd_bruta)=="out_degree"]<-"ce_out_degree"
names(bd_bruta)[names(bd_bruta)=="strength"]<-"ce_strength"
names(bd_bruta)[names(bd_bruta)=="out_strength"]<-"ce_out_strength"
names(bd_bruta)[names(bd_bruta)=="page_rank"]<-"ce_page_rank"
names(bd_bruta)[names(bd_bruta)=="tmmn"]<-"cl_tmmn"
names(bd_bruta)[names(bd_bruta)=="tmmx"]<-"cl_tmmx"
names(bd_bruta)[names(bd_bruta)=="tmmean"]<-"cl_tmmean"
names(bd_bruta)[names(bd_bruta)=="complete_secondary_edu"]<-"de_complete_secondary_edu"
names(bd_bruta)[names(bd_bruta)=="department"]<-"de_department"
names(bd_bruta)[names(bd_bruta)=="district"]<-"de_district"
names(bd_bruta)[names(bd_bruta)=="edu_achievement"]<-"de_edu_achievement"
names(bd_bruta)[names(bd_bruta)=="family_income"]<-"de_family_income"
names(bd_bruta)[names(bd_bruta)=="hh_1_nbi_or_more"]<-"de_hh_1_nbi_or_more"
names(bd_bruta)[names(bd_bruta)=="hh_high_economic_dependence"]<-"de_hh_high_economic_dependence"
names(bd_bruta)[names(bd_bruta)=="hh_inadequate_char"]<-"de_hh_inadequate_char"
names(bd_bruta)[names(bd_bruta)=="hh_school_absence"]<-"de_hh_school_absence"
names(bd_bruta)[names(bd_bruta)=="hh_wo_sanitation"]<-"de_hh_wo_sanitation"
names(bd_bruta)[names(bd_bruta)=="idh"]<-"de_idh"
names(bd_bruta)[names(bd_bruta)=="life_expectancy"]<-"de_life_expectancy"
names(bd_bruta)[names(bd_bruta)=="monetary_poverty"]<-"de_monetary_poverty"
names(bd_bruta)[names(bd_bruta)=="overcrowded_hh"]<-"de_overcrowded_hh"
names(bd_bruta)[names(bd_bruta)=="population_permanent"]<-"de_population_permanent"
names(bd_bruta)[names(bd_bruta)=="province"]<-"de_province"
names(bd_bruta)[names(bd_bruta)=="ubigeo"]<-"de_ubigeo"
names(bd_bruta)[names(bd_bruta)=="intra_mob"]<-"de_intra_mob"
names(bd_bruta)[names(bd_bruta)=="years_edu"]<-"de_years_edu"
names(bd_bruta)[names(bd_bruta)=="aai"]<-"po_aai"
names(bd_bruta)[names(bd_bruta)=="co"]<-"po_co"
names(bd_bruta)[names(bd_bruta)=="no2"]<-"po_no2"
names(bd_bruta)[names(bd_bruta)=="ntl"]<-"po_ntl"
names(bd_bruta)[names(bd_bruta)=="o3"]<-"po_o3"
names(bd_bruta)[names(bd_bruta)=="pm25"]<-"po_pm25"
names(bd_bruta)[names(bd_bruta)=="so2"]<-"po_so2"
names(bd_bruta)[names(bd_bruta)=="new_cases"]<-"ne_new_cases"

################## 3.- SETTING "DIRIS" VARIABLE #####################
bd_bruta$DIRIS = ifelse(bd_bruta$de_district %in% c(    "BARRANCO","CHORRILLOS",
                                                       "LURIN","PACHACAMAC",
                                                       "PUCUSANA","PUNTA HERMOSA",
                                                       "PUNTA NEGRA","SAN BARTOLO",
                                                       "SAN JUAN DE MIRAFLORES","SANTA MARIA DEL MAR",
                                                       "SANTIAGO DE SURCO","VILLA EL SALVADOR",
                                                       "VILLA MARIA DEL TRIUNFO") ,"LIMA_SUR" ,
                         ifelse(bd_bruta$de_district %in% c(   "ANCON","CARABAYLLO",
                                                             "COMAS","INDEPENDENCIA",
                                                             "LOS OLIVOS","PUENTE PIEDRA",
                                                             "RIMAC","SAN MARTIN DE PORRES",
                                                             "SANTA ROSA") ,"LIMA_NORTE",
                                ifelse(bd_bruta$de_district %in% c(  "ATE","CHACLACAYO",
                                                                   "CIENEGUILLA","EL AGUSTINO",
                                                                   "HUAYCAN","LA MOLINA",
                                                                   "LURIGANCHO","SANTA ANITA") ,"LIMA_ESTE",
                                       ifelse(bd_bruta$de_district %in% c( "BREÑA","JESUS MARIA","LA VICTORIA",
                                                                         "LIMA","LINCE","MAGDALENA DEL MAR","MAGDALENA VIEJA",
                                                                         "MIRAFLORES","PUEBLO LIBRE",
                                                                         "SAN BORJA","SAN ISIDRO",
                                                                         "SAN JUAN DE LURIGANCHO",
                                                                         "SAN LUIS","SAN MIGUEL","SURQUILLO") ,"LIMA_CENTRO", 0))))

# REMOVING IDENTIFY VARIABLES (QUALITATIVE NOMINAL)
bd = subset(bd_bruta, select = -c(de_ubigeo,de_department,de_province,de_district) )
################## 4.- PCA ###################################
'%!in%' <- function(x,y)!('%in%'(x,y))
bd$Flag_Obj = bd$ne_new_cases
varTest = names(bd[,names(bd) %!in% c("DIRIS","ne_new_cases","Flag_Obj")])

#Quitando las variables de movilización
bd_sin_ce <- subset(bd, select = -c(de_population_permanent,ce_alpha   ,ce_authority   ,ce_betweenness   ,ce_closeness   ,ce_degree   ,ce_hub   ,ce_in_closeness   ,ce_in_degree   ,ce_in_strength   ,ce_out_closeness   ,ce_out_degree   ,ce_out_strength   ,ce_page_rank   ,ce_strength,  DIRIS,ne_new_cases,Flag_Obj) )
#Escalando la variable Population para evitar que se lleve mayor peso 
#esas componentes y tener sesgo (Analisis previo al PCA)
bd_sin_ce$de_hh_wo_sanitation <- rescale(bd_sin_ce$de_hh_wo_sanitation)
bd_sin_ce$de_hh_high_economic_dependence <- rescale(bd_sin_ce$de_hh_high_economic_dependence)


respca = prcomp(bd_sin_ce,scale = TRUE)
names(respca)
respca$rotation
head(respca$rotation)[,1:10]

dim(respca$rotation) #Number of distinct components
summary(respca)
xx = respca$x
xx = as.data.frame(xx)
bd_sin_ce$PC1 = xx$PC1
bd_sin_ce$PC2 = xx$PC2
bd_sin_ce$PC3 = xx$PC3
bd_sin_ce$PC4 = xx$PC4
bd_sin_ce$PC5 = xx$PC5
bd_sin_ce$PC6 = xx$PC6
bd_sin_ce$PC7 = xx$PC7
bd_sin_ce$PC8 = xx$PC8
bd_sin_ce$PC9 = xx$PC9
bd_sin_ce$PC10 = xx$PC10
cor(bd_sin_ce)