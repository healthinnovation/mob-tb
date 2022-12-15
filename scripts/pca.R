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

install.packages("factoextra")
library('factoextra')
library("FactoMineR")
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
bd_solo_ce_population = subset(bd, select = c(de_population_permanent,ce_alpha   ,ce_authority   ,ce_betweenness   ,ce_closeness   ,ce_degree   ,ce_hub   ,ce_in_closeness   ,ce_in_degree   ,ce_in_strength   ,ce_out_closeness   ,ce_out_degree   ,ce_out_strength   ,ce_page_rank   ,ce_strength,  DIRIS,ne_new_cases,Flag_Obj) )
#Escalando la variable Population para evitar que se lleve mayor peso 
#esas componentes y tener sesgo (Analisis previo al PCA)
bd_sin_ce$de_hh_wo_sanitation <- rescale(bd_sin_ce$de_hh_wo_sanitation)
bd_sin_ce$de_hh_high_economic_dependence <- rescale(bd_sin_ce$de_hh_high_economic_dependence)

res.pca <- PCA(bd_sin_ce,  graph = FALSE)
# Extract eigenvalues/variances
get_eig(res.pca)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Extract the results for variables
var <- get_pca_var(res.pca)
var
# Coordinates of variables
head(var$coord)
# Contribution of variables
head(var$contrib)
# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)






respca = prcomp(bd_sin_ce,scale = TRUE)
respca$rotation
xx = respca$x
xx = as.data.frame(xx)
bd_sin_ce$PC1 = xx$PC1
bd_sin_ce$PC2 = xx$PC2
bd_sin_ce$PC3 = xx$PC3
bd_sin_ce$PC4 = xx$PC4
cor(bd_sin_ce)

bd_pca = cbind(bd_sin_ce$PC1,bd_sin_ce$PC2,bd_sin_ce$PC3,bd_sin_ce$PC4,bd_solo_ce_population$ne_new_cases,bd_solo_ce_population$de_population_permanent)
bd_pca = cbind(bd_sin_ce$PC1,bd_sin_ce$PC2,bd_sin_ce$PC3,bd_sin_ce$PC4,bd_solo_ce_population$ne_new_cases,bd_solo_ce_population$de_population_permanent)
df = as.data.frame(bd_pca)
df_pca <- df %>% 
  rename("PC1" = "V1",
         "PC2" = "V2",
         "PC3" = "V3",
         "PC4" = "V4",
         "ne_new_cases" = "V5",
         "de_population_permanent" = "V6",)

bd_solo_ce = subset(bd, select = c(ce_alpha   ,ce_authority   ,ce_betweenness   ,ce_closeness   ,ce_degree   ,ce_hub   ,ce_in_closeness   ,ce_in_degree   ,ce_in_strength   ,ce_out_closeness   ,ce_out_degree   ,ce_out_strength   ,ce_page_rank   ,ce_strength) )
bd_pca = cbind(df_pca,bd_solo_ce)



######################### GAM  #######################

fit_0 <- gam(ne_new_cases ~ offset(log(de_population_permanent)) + s(PC1) + s(PC2) + s(PC3) + s(PC4),
  family = nb(), data = df_pca)
summary(fit_0)


fit_1 <- gam(ne_new_cases ~ offset(log(de_population_permanent)) + s(PC1) 
            + s(PC2)
             #+ s(PC3)
             ,
             #+ s(PC4)
             family = nb(), data = bd_pca)
summary(fit_1)
AIC(fit_1)

fit_central_0 <- update(
  fit_1, . ~ . + ce_in_degree
)

fit_central_1 <- update(
  fit_1, . ~ . + ce_out_degree
)
fit_central_2 <- update(
  fit_1, . ~ . + ce_degree
)
fit_central_3 <- update(
  fit_1, . ~ . + ce_in_strength
)
fit_central_4 <- update(
  fit_1, . ~ . + ce_out_strength
)
fit_central_5 <- update(
  fit_1, . ~ . + ce_strength
)
fit_central_6 <- update(
  fit_1, . ~ . + ce_in_closeness
)
fit_central_7 <- update(
  fit_1, . ~ . + ce_out_closeness
)
fit_central_8 <- update(
  fit_1, . ~ . + ce_closeness
)
fit_central_9 <- update(
  fit_1, . ~ . + ce_betweenness
)
fit_central_10 <- update(
  fit_1, . ~ . + ce_page_rank
)
fit_central_11 <- update(
  fit_1, . ~ . + ce_alpha
)
fit_central_12 <- update(
  fit_1, . ~ . + ce_authority
)
fit_central_13 <- update(
  fit_1, . ~ . + ce_hub
)

get_aic <- function(...) {
  aic_df <- AIC(...)
  call <- match.call()
  models <- as.character(call)[-1]
  aic_tibble <- tibble(model = models, aic_df) %>% 
    arrange(AIC)
  aic_tibble
}





get_aic(
  fit_1, fit_central_0, fit_central_1, fit_central_2, fit_central_3, fit_central_4, 
  fit_central_5, fit_central_6, fit_central_7, fit_central_8, fit_central_9,
  fit_central_10, fit_central_11, fit_central_12, fit_central_13
)




####################### NO COPIAR A PARTIR DE AQUÍ #################

df_pca_1 = df_pca[1:1,]
glm_0 <- glm(ne_new_cases ~ PC1 + PC2 + PC3 + PC4,
             #offset(log(de_population_permanent)), 
             #+ PC1 
             ##+ PC2 + PC3 + PC4,
             ,family =  , data = df_pca_1)
install.packages('MASS')
summary(m1 <- glm.nb(ne_new_cases ~ PC1 + PC2 + PC3 + PC4, data = df_pca_1))
summary(glm_0)
glm(., family = negative.binomial(theta), data = df_pca_1)
str(df_pca)











