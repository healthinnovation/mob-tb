################## 1.- CARGA DE DATA                   #####################
mob_lima_exploratorio <- read.csv(file='mob-tb-lima-modelo.csv')
mob_lima_exploratorio <- subset( mob_lima_exploratorio, select = -X )
mob_lima_exploratorio
bd=mob_lima_exploratorio

bd_modelo= subset(bd, select = -c(total_casos,morbilidad,incidencia,incidencia_tbpfp,
                                  poblacion,
                                  nuevos_tbpfp,ubigeo_eess,department,province) )

bd_modelo$DIRIS = ifelse(bd_modelo$district %in% c(    "BARRANCO","CHORRILLOS",
                                                       "LURIN","PACHACAMAC",
                                                       "PUCUSANA","PUNTA HERMOSA",
                                                       "PUNTA NEGRA","SAN BARTOLO",
                                                       "SAN JUAN DE MIRAFLORES","SANTA MARIA DEL MAR",
                                                       "SANTIAGO DE SURCO","VILLA EL SALVADOR",
                                                       "VILLA MARIA DEL TRIUNFO") ,"LIMA_SUR" ,
                         ifelse(bd_modelo$district %in% c(   "ANCON","CARABAYLLO",
                                                             "COMAS","INDEPENDENCIA",
                                                             "LOS OLIVOS","PUENTE PIEDRA",
                                                             "RIMAC","SAN MARTIN DE PORRES",
                                                             "SANTA ROSA") ,"LIMA_NORTE",
                                ifelse(bd_modelo$district %in% c(  "ATE","CHACLACAYO",
                                                                   "CIENEGUILLA","EL AGUSTINO",
                                                                   "HUAYCAN","LA MOLINA",
                                                                   "LURIGANCHO","SANTA ANITA") ,"LIMA_ESTE",
                                       ifelse(bd_modelo$district %in% c( "BRENA","JESUS MARIA","LA VICTORIA",
                                                                         "LIMA","LINCE","MAGDALENA DEL MAR",
                                                                         "MIRAFLORES","PUEBLO LIBRE",
                                                                         "SAN BORJA","SAN ISIDRO",
                                                                         "SAN JUAN DE LURIGANCHO",
                                                                         "SAN LUIS","SAN MIGUEL","SURQUILLO") ,"LIMA_CENTRO", 0))))


bd_modelo= subset(bd_modelo, select = -c(district) )
bd_modelo
####Correlacion máxima de 0.30
####Sin morbilidad ni incidencia
##Correlacion sobre variables seleccionadas por  Correlacion de Pearson
bd_modelo_correlacion = subset(bd_modelo, select = c(population,out_strength,out_degree,hub,monetary_poverty,closeness,in_degree,alpha,idh,in_closeness,out_closeness,betweenness,nib_hogar,page_rank,authority,in_strength,max_temperature,min_temperature,mean_temperature,precipitation,evp,delta_temperature))
cor(bd_modelo_correlacion)
#CorrelacionadasOrdenadas
#install.packages("corrplot")
library('corrplot')
M<-cor(bd_modelo_correlacion)
head(round(M,2))
corrplot(M, method="circle")

########################## BORRADOR ################################
# population
# out_strength  
# out_degree 
# hub   , 0.85 con out_strength
# monetary_poverty , -0.73 con out_strength -0.84 con hub
# closeness
# in_degree
# alpha 0.99 con in_degree
# idh   0.69 con out_strength,   0.84 con hub,  -0.96 con monetary_poverty
# in_closeness
# out_closeness
# betweeness  0.76 con in_closeness
# nib_hogar -0.7 con hub, 0.88 con monetary_poverty, -0.88 con idh. 
# page rank



#bd_modelo_correlacion2 = subset(bd_modelo_correlacion, select = -c(out_strength,authority,in_strength))
cor(bd_modelo_correlacion2)
#se va alpha porque entre in_degree y alpha, alpha tiene más compleja su interpretatibidad
#y ambos tienen alta correlacion entre ellas vs las demas
#se va IDH, conversacion con Diego V.
#bd_modelo_correlacion3 = subset(bd_modelo_correlacion2, select = -c(hub,alpha,idh))
#cor(bd_modelo_correlacion3)
#últimos en irse: out_closeness, betweenness
#bd_modelo_correlacion3 = subset(bd_modelo_correlacion2, select = -c(hub,alpha,idh,out_closeness,
                                                                    betweenness,nib_hogar))
#cor(bd_modelo_correlacion3)

summary(py1 <- glm(formula = casos_nuevos ~  offset(log(population)) + out_degree  +
                     monetary_poverty    + closeness   + in_degree  + in_closeness +
                     page_rank , family="poisson", data=bd_modelo_1))
confint.default(py1); py1$aic/(py1$df.null+1)
pr <- resid(py1, type = "pearson")
pr

pchi2 <- sum(residuals(py1, type="pearson")^2)
pchi2
disp <- pchi2/py1$df.residual; pchi2; disp
## Como la dispersión está alejada de 1.0000 entonces probaremos con la Regresión Binomial Negativa
 
###################### MODELOS DRIVE ##########################
summary(py1 <- glm(formula = casos_nuevos ~  offset(log(population)) + out_strength  +
                     in_strength +     monetary_poverty  , family="poisson", data=bd_modelo_1))
confint.default(py1); py1$aic/(py1$df.null+1)
pr <- resid(py1, type = "pearson")
pr

pchi2 <- sum(residuals(py1, type="pearson")^2)
pchi2
disp <- pchi2/py1$df.residual; pchi2; disp
## Como la dispersión está alejada de 1.0000 entonces probaremos con la Regresión Binomial Negativa

################################# library fixed ####################
install.packages('fixed')
library('fixed')
m1 <- fenegbin( casos_nuevos ~  offset(log(population)) + out_strength  +
                  in_strength +     monetary_poverty| DIRIS,
               data = bd_modelo_1)
m1

m2 <- fenegbin( casos_nuevos ~  offset(log(population)) + out_strength  +
                  in_strength +     monetary_poverty,
                data = bd_modelo_1)
m2

m3 <- fenegbin( casos_nuevos ~  offset(log(population)) + in_closeness  +
                  out_closeness + monetary_poverty| DIRIS,
                data = bd_modelo_1)
m3


m4 <- fenegbin( casos_nuevos ~  offset(log(population)) + in_closeness  +
                  out_closeness +     monetary_poverty,
                data = bd_modelo_1)
m4



##########################################################


m2 <- fenegbin(casos_nuevos ~ 
                 offset(log(population)) + out_degree  +
                 monetary_poverty    + closeness   + in_degree  + in_closeness +
                 page_rank | DIRIS,
               data = bd_modelo_1)

m2


####################################  glm nb ##################################
library("MASS")
install.packages("lme4")
install.packages("Rcpp")
library(Rcpp)
library(lme4)
library(lme4)
## and use its glm.nb() - as indeed we have zero random effect:
## Not run: 
m.glm <- glm.nb(casos_nuevos ~ f1*f2, data=dd, trace=TRUE)
summary(m.glm)
m.nb <- glmer.nb(casos_nuevos ~ log(population)*out_degree + (1|DIRIS), data=bd_modelo_1, verbose=TRUE)
m.nb
## The neg.binomial theta parameter:
getME(m.nb, "glmer.nb.theta")
LL <- logLik(m.nb)
## mixed model has 1 additional parameter (RE variance)
stopifnot(attr(LL,"df")==attr(logLik(m.glm),"df")+1)
plot(m.nb, resid(.) ~ g)# works, as long as data 'dd' is found

## End(Not run)



################################## Apuntes  #######################
#Incluir variables de Educacion, edad promedio,  (DEPENDENCIA - R Miners)
#% de personas mayores con educacion secundaria  (DEPENDENCIA - R Miners)
#contaminación de aire  (LISTO)
#PCA de las 3 que tenemos, probar inflación de varianza
#intervalos de con, modelos efectos fijos diris, var socioambientales,
#graficos estimacion puntual y metricas

install.packages("plm")
library('plm')
colnames(bd_modelo)
##MODELO 1: distritos + demográficas + in_degree+ out_degree
fixed1 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + in_degree + out_degree , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed1
summary(fixed1)
#p-value<0.05  -> ok

##MODELO 2: distritos + demográficas + in_strength + out_strength
fixed2 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + in_strength + out_strength , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed2
summary(fixed2)
#p-value<0.05  -> ok

##MODELO 3: distritos + demográficas + in-closeness + out-closeness
fixed3 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + in_closeness + out_closeness , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed3
summary(fixed3)
#p-value<0.05  -> ok

##MODELO 4: distritos + demográficas + closeness
fixed4 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + closeness , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed4
summary(fixed4)
#p-value<0.05  -> ok

##MODELO 5: distritos + demográficas + page_rank
fixed5 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + page_rank , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed5
summary(fixed5)
#p-value<0.05  -> ok

##MODELO 6: distritos + demográficas + alpha
fixed6 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + alpha , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed6
summary(fixed6)
#p-value<0.05  -> ok

##MODELO 7: distritos + demográficas + authority
fixed7 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + authority , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed7
summary(fixed7)
#p-value<0.05  -> ok

##MODELO 8: distritos + demográficas + hub
fixed8 <- plm(casos_nuevos ~ monetary_poverty + idh + nib_hogar + 
                delta_temperature + hub , data=bd_modelo,
              index=c("DIRIS"), model="within",family=poisson)
fixed8
summary(fixed8)
#p-value<0.05  -> ok
install.packages('VIF')
library('VIF')
vif(fixed8)

