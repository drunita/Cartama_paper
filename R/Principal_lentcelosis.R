#plot(residuals(modelo))
#qqnorm(residuals(modelo))
#plot(modelo)



#############################################################################################
###GENERACION Y LIMPIEZA DEL DATA SET #######################################################
#############################################################################################


Datos_mat<-resumencartamaseveridad
Datos_mat$...13<-NULL

Datos_mat$Incidencia_1<-NULL
Datos_mat$Severidad_1<-NULL
Datos_mat$Severidad_pedunculo<-NULL


Datos_mat<-na.omit(Datos_mat)

#Datos_mat<-subset(Datos_mat,Datos_mat$Cosecha!="2020_t")


s_ped<-factor(Datos_mat$Severidad_pedunculo)
lote<-factor(Datos_mat$Lote)
arbol<-factor(Datos_mat$Arbol)
up<-factor(Datos_mat$UP)
ula<-up:lote:arbol
ul<-up:lote
la<- lote:arbol
year<-factor(Datos_mat$Cosecha)
fruta<-factor(Datos_mat$Fruta)


################################################################################################
############ MODELOS SEV E INC VS UP#########################################################
################################################################################################


y<-log(Datos_mat$Severidad_0+1)

modelo<-lmer(y~up*year + (1|lote) + (1|la),REML = FALSE)
modelo1<-lmer(y~up+year + (1|lote) + (1|la),REML = FALSE)
modelo2<-lmer(y~1 +  + (1|lote) + (1|la),REML = FALSE)


p_val<-model_comparision(modelo,modelo1,"Sev0_up.txt")

y<-round(Datos_mat$Incidencia_0)

modelo<-glmer(y~up*year + (1|lote) + (1|la),family = poisson)
modelo1<-glmer(y~up+year + (1|lote) + (1|la),family = poisson)
modelo2<-glmer(y~lote + (1|la),family = poisson)

p_val<-model_comparision(modelo,modelo1,"Inc0_up.txt")

y<-Datos_mat$Severidad_pedunculo

modelo<-glm(y~up*year,family = quasipoisson)
modelo1<-glm(y~up+year,family = quasipoisson)
modelo2<-glm(y~up,family = quasipoisson)
modelo3<-glm(y~year,family = quasipoisson)
modelo4<-glm(y~1,family = quasipoisson)

anova(modelo, modelo1, modelo2,modelo4, test = "Chisq")

sink("sevp_up.txt")
print(summary(modelo2))
sink()

###########################################################
#####Analisis de varianza###############################
#########################################################

modelo2<-lmer(y~year + (1|up)+(1|ul)+(1|ula),REML = FALSE)
sink("analisis_variansa.txt")

print(summary(modelo2))
sink()

# ##############################################################################################
# ############ MODELOS SEV E INC VS SEV PEDU##################################################
# ###############################################################################################
#
#
y<-log(Datos_mat$Severidad_0+1)
#
modelo<-lmer(y~s_ped*up + (1|lote) + (1|la),REML = FALSE)
modelo1<-lmer(y~s_ped+up + (1|lote) + (1|la),REML = FALSE)
modelo2<-lmer(y~s_ped + (1|lote) + (1|la),REML = FALSE)
modelo2<-lmer(y~up + (1|lote) + (1|la),REML = FALSE)


modelo2<-lmer(y~s_ped + (1|lote) + (1|la),REML = FALSE)
modelo3<-lmer(y~1 + (1|lote) + (1|la),REML = FALSE)
# modelo2<-lmer(y~s_ped + (1|lote) + (1|la),REML = FALSE)
#
p_val<-model_comparision(modelo,modelo1,"Sev0_Sevp.txt")
#
#
#
y<-round(Datos_mat$Incidencia_0)
#
modelo<-glmer(y~s_ped*up + (1|lote) + (1|la),family = poisson)
modelo1<-glmer(y~s_ped+up+ (1|lote) + (1|la),family = poisson)
modelo2<-glmer(y~s_ped + (1|lote) + (1|la),family = poisson)
#
p_val<-model_comparision(modelo,modelo1,"Inc0_sevp.txt")
#
#
#













################################################################################################
############ MODELOS SEV E INC VS Lote#########################################################
################################################################################################
####este analisis de variaza es para ambas unidades productivas...los analisis que involucran lote
###se den hacer para cada up por separado


Datos_mat<-subset(Datos_mat, Datos_mat$UP=="Escondida")
Datos_mat<-subset(Datos_mat, Datos_mat$UP=="Sinai")

s_ped<-factor(Datos_mat$Severidad_pedunculo)
lote<-factor(Datos_mat$Lote)
arbol<-factor(Datos_mat$Arbol)
up<-factor(Datos_mat$UP)
ula<-up:lote:arbol
ul<-up:lote
la<- lote:arbol
year<-factor(Datos_mat$Cosecha)
fruta<-factor(Datos_mat$Fruta)


 y<-log(Datos_mat$Severidad_0+1)

 modelo<-lmer(y~lote*year + (1|la),REML = FALSE)
 modelo1<-lmer(y~lote +year + (1|la),REML = FALSE)
 modelo2<-lmer(y~lote + (1|la),REML = FALSE)

 anova(modelo,modelo1)
 summary(modelo)

 p_val<-model_comparision(modelo,modelo1,"Sev0_lote_es.txt")

 y<-round(Datos_mat$Incidencia_0)

 modelo<-glmer(y~lote*year + (1|la),family = poisson)
 modelo1<-glmer(y~lote+year + (1|la),family = poisson)
 modelo2<-glmer(y~year + (1|up)+(1|ul)+(1|ula),family = poisson)

 p_val<-model_comparision(modelo,modelo1,"Inc0_lote_es.txt")




# ##############################################################################################
# ##### MODELOS SEV E INC VS TIEMPO DE MEDICION ################################################
# ###############################################################################################
#
y<-c(round(Datos_mat$Incidencia_0),round(Datos_mat$Incidencia_1))
up_t<-c(Datos_mat$UP,Datos_mat$UP)
medida<-factor(c(rep("0dpc",length(y)/2),rep("21dpc",length(y)/2)))
f<-c(fruta,fruta)
ano<-c(year,year)






modelo<-glmer(y~medida  + (1|up_t)+(1|f)+(1|ano),family = poisson)
modelo1<-glmer(y~1  + (1|up_t)+(1|f)+(1|ano),family = poisson)


p_val<-model_comparision(modelo,modelo1,"In_time.txt")

modelo<-glmer(y~medida*up_t + (1|f)+(1|ano),family = poisson)
modelo1<-glmer(y~medida+up_t + (1|f)+(1|ano),family = poisson)
p_val<-model_comparision(modelo,modelo1,"In_time_efectoup.txt")

#

 y<-c(log(Datos_mat$Severidad_0+1),log(Datos_mat$Severidad_1+1))
#
 modelo<-lmer(y~medida + (1|up_t)+(1|f)+(1|ano),REML = FALSE)
 modelo1<-lmer(y~1 + (1|up_t)+(1|f)+(1|ano),REML = FALSE)
# modelo2<-lmer(y~medida +(1|f),REML = FALSE)
#
#
 p_val<-model_comparision(modelo,modelo1,"Sev_time.txt")

 modelo<-lmer(y~medida*up_t + (1|f)+(1|ano),REML = FALSE)
 modelo1<-lmer(y~medida+up_t + (1|f)+(1|ano),REML = FALSE)
 p_val<-model_comparision(modelo,modelo1,"Se_time_efectoup.txt")
