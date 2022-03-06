

# ###################### Plot sev e ind 0dp por lote o up#######################################

  plot1 <- boxplot_treatments(Datos_mat,Datos_mat$UP,Datos_mat$Severidad_0,Datos_mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),c("Escondida", "Sinai"),"","Severity  (%)","",0,5,"Harvest")
  plot2 <- boxplot_treatments(Datos_mat,Datos_mat$UP,Datos_mat$Incidencia_0,Datos_mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),c("Escondida", "Sinai"),"","Incidence","",0,350,"Harvest")
#
#
 require(gridExtra)
  plot<-grid.arrange(plot1,plot2, ncol=2,nrow=1)
#
  ggsave(plot,filename="Tiempo0_up.png",device="png", dpi=500, width = 28, height = 16, units = "cm")
#
#

  plot1 <- boxplot_treatments(Datos_mat,Datos_mat$Lote,Datos_mat$Severidad_0,Datos_mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),names(table(Datos_mat$Lote)),"","Severity  (%)","",0,15,"Harvest")
  plot2 <- boxplot_treatments(Datos_mat,Datos_mat$Lote,Datos_mat$Incidencia_0,Datos_mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),names(table(Datos_mat$Lote)),"","Incidence","",0,350,"Harvest")

#
 require(gridExtra)
  plot<-grid.arrange(plot1,plot2, ncol=2,nrow=1)
#
 ggsave(plot,filename="Tiempo0_es.png",device="png", dpi=500, width = 28, height = 14, units = "cm")
#
#
 plot<- stack_barplot(Datos_mat,Datos_mat$Severidad_pedunculo,Datos_mat$UP,c("sev","up","y"),c("3","2","1"),c("Escondida", "Sinai"),"","Proporci?n","Necrosis ped?nculo")
#
#  ggsave(plot,filename="Peduncle_up.png",device="png", dpi=500, width = 15, height = 15, units = "cm")


 ###############
 ######## Sevped total
 #################

 mat_mean<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo),mean)
 mat_se<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo),sd)
 n<-c(table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Escondida"]),
      table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Sinai"]))
 mat<-data.frame(rep(c("Low","Medium","High"),2), c(rep("Escondida",3),rep("Sinai",3)),
                 c(mat_mean[1,1],mat_mean[1,2],mat_mean[1,3],mat_mean[2,1],mat_mean[2,2],mat_mean[2,3]),
                 c(mat_se[1,1],mat_se[1,2],mat_se[1,3],mat_se[2,1],mat_se[2,2],mat_se[2,3])/sqrt(n))

 colnames(mat)<-c("Sevp","Up","mean","Sd")
 plot1 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,
                             mat$Sevp,c("Low","Medium","High"),c("Escondida", "Sinai"),"","Severity (%)",0,1,"Peduncle necrosis","B")

 mat_mean<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo),mean)
 mat_se<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo),sd)
 mat<-data.frame(rep(c("Low","Medium","High"),2), c(rep("Escondida",3),rep("Sinai",3)),
                 c(mat_mean[1,1],mat_mean[1,2],mat_mean[1,3],mat_mean[2,1],mat_mean[2,2],mat_mean[2,3]),
                 c(mat_se[1,1],mat_se[1,2],mat_se[1,3],mat_se[2,1],mat_se[2,2],mat_se[2,3])/sqrt(n))
 colnames(mat)<-c("Sevp","Up","mean","Sd")

 plot2 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,
                             mat$Sevp,c("Low","Medium","High"),c("Escondida", "Sinai"),"","Incidence",0,110,"Peduncle necrosis","C")

 
 plot3<- stack_barplot(Datos_mat,Datos_mat$Severidad_pedunculo,Datos_mat$UP,c("sev","up","y"),c("Low","Medium","High"),c("Escondida", "Sinai"),"","Proportion","Peduncle necrosis","A")

 require(gridExtra)
 plot<-grid.arrange(plot3,plot1,plot2, ncol=3,nrow=1,widths = c(1.7, 1.5, 1.5))
 #
 ggsave(plot,filename="Sevp_vs_danolenticel.png",device="png", dpi=500, width = 41, height = 14, units = "cm")
 #
###############
######## Sevped por cosehcas
 #######################

mat_mean<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo,Datos_mat$Cosecha),mean)
mat_se<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo,Datos_mat$Cosecha),sd)
n<-c(table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Escondida"& Datos_mat$Cosecha=="2019_p"]),
     table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Sinai"& Datos_mat$Cosecha=="2019_p"]),
     table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Escondida"& Datos_mat$Cosecha=="2020_p"]),
     table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Sinai"& Datos_mat$Cosecha=="2020_p"]),
     table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Escondida"& Datos_mat$Cosecha=="2020_t"]),
     table(Datos_mat$Severidad_pedunculo[Datos_mat$UP=="Sinai"& Datos_mat$Cosecha=="2020_t"]))
mat<-data.frame(c(rep("2019_p",6),rep("2020_p",6),rep("2020_t",6)),rep(c("1","2","3"),6), rep(c(rep("Escondida",3),rep("Sinai",3)),3),
                c(mat_mean[1,,1],mat_mean[2,,1],mat_mean[1,,2],mat_mean[2,,2],mat_mean[1,,3],mat_mean[2,,3])
                ,c(mat_se[1,,1],mat_se[2,,1],mat_se[1,,2],mat_se[2,,2],mat_se[1,,3],mat_se[2,,3])/sqrt(n))

colnames(mat)<-c("Cosecha","Sevp","Up","mean","Sd")

plot1 <- barplot_treatments(subset(mat,mat$Cosecha=="2019_p"),mat$Up[mat$Cosecha=="2019_p"],mat$mean[mat$Cosecha=="2019_p"],mat$Sd[mat$Cosecha=="2019_p"],
                            mat$Sevp[mat$Cosecha=="2019_p"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,2.5,"Necrosis ped?nculo")

#plot1<-plot1 + annotate(geom="text", x=0.8, y=2.1, label="2019 Principal", size=4, face="bold")
plot2 <- barplot_treatments(subset(mat,mat$Cosecha=="2020_t"),mat$Up[mat$Cosecha=="2020_t"],mat$mean[mat$Cosecha=="2020_t"],mat$Sd[mat$Cosecha=="2020_t"],
                            mat$Sevp[mat$Cosecha=="2020_t"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,2.5,"Necrosis ped?nculo")
#plot2<-plot2 + annotate(geom="text", x=0.8, y=2.1, label="2020 Traviesa", size=4, face="bold")

plot3 <- barplot_treatments(subset(mat,mat$Cosecha=="2020_p"),mat$Up[mat$Cosecha=="2020_p"],mat$mean[mat$Cosecha=="2020_p"],mat$Sd[mat$Cosecha=="2020_p"],
                            mat$Sevp[mat$Cosecha=="2020_p"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,2.5,"Necrosis ped?nculo")
#plot3<-plot3 + annotate(geom="text", x=0.8, y=2.1, label="2020 Principal", size=4, face="bold")

mat_mean<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo,Datos_mat$Cosecha),mean)
mat_se<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Severidad_pedunculo,Datos_mat$Cosecha),sd)
mat<-data.frame(c(rep("2019_p",6),rep("2020_p",6),rep("2020_t",6)),rep(c("1","2","3"),6), rep(c(rep("Escondida",3),rep("Sinai",3)),3),
                c(mat_mean[1,,1],mat_mean[2,,1],mat_mean[1,,2],mat_mean[2,,2],mat_mean[1,,3],mat_mean[2,,3])
                ,c(mat_se[1,,1],mat_se[2,,1],mat_se[1,,2],mat_se[2,,2],mat_se[1,,3],mat_se[2,,3])/sqrt(n))

colnames(mat)<-c("Cosecha","Sevp","Up","mean","Sd")


plot4 <- barplot_treatments(subset(mat,mat$Cosecha=="2019_p"),mat$Up[mat$Cosecha=="2019_p"],mat$mean[mat$Cosecha=="2019_p"],mat$Sd[mat$Cosecha=="2019_p"],
                            mat$Sevp[mat$Cosecha=="2019_p"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,155,"Necrosis ped?nculo")
#plot4<-plot4 + annotate(geom="text", x=0.8, y=130, label="2019 Principal", size=4, face="bold")
plot5 <- barplot_treatments(subset(mat,mat$Cosecha=="2020_t"),mat$Up[mat$Cosecha=="2020_t"],mat$mean[mat$Cosecha=="2020_t"],mat$Sd[mat$Cosecha=="2020_t"],
                            mat$Sevp[mat$Cosecha=="2020_t"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,155,"Necrosis ped?nculo")
#plot5<-plot5 + annotate(geom="text", x=0.8, y=130, label="2020 Traviesa", size=4, face="bold")
plot6 <- barplot_treatments(subset(mat,mat$Cosecha=="2020_p"),mat$Up[mat$Cosecha=="2020_p"],mat$mean[mat$Cosecha=="2020_p"],mat$Sd[mat$Cosecha=="2020_p"],
                            mat$Sevp[mat$Cosecha=="2020_p"],c("1","2","3"),c("Escondida", "Sinai"),"","Severidad (%)",0,155,"Necrosis ped?nculo")
#plot6<-plot6 + annotate(geom="text", x=0.8, y=130, label="2020 Principal", size=4, face="bold")


require(gridExtra)
 plot<-grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=3,nrow=2)

 ggsave(plot,filename="sevpedunc_up.png",device="png", dpi=500, width = 35, height = 25, units = "cm")


##################### ######## Sevup
 #############

 mat_mean<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Cosecha),mean)
 mat_se<-tapply(Datos_mat$Severidad_0,list(Datos_mat$UP,Datos_mat$Cosecha),sd)
 n<-c(table(Datos_mat$Cosecha[Datos_mat$UP=="Escondida"]),table(Datos_mat$Cosecha[Datos_mat$UP=="Sinai"]))
 mat<-data.frame(c("2019_p","2020_p","2020_t","2021_t","2019_p","2020_p","2020_t","2021_t"), c(rep("Escondida",4),rep("Sinai",4)),
                 c(mat_mean[1,],mat_mean[2,]),c(mat_se[1,],mat_se[2,])/sqrt(n))

 colnames(mat)<-c("Cosecha","Up","mean","Sd")

 plot1 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),c("Escondida", "Sinai"),"","Severity (%)",0,2,"Harvest","a")

 mat_mean<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Cosecha),mean)
 mat_se<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$UP,Datos_mat$Cosecha),sd)
 n<-c(table(Datos_mat$Cosecha[Datos_mat$UP=="Escondida"]),table(Datos_mat$Cosecha[Datos_mat$UP=="Sinai"]))
 mat<-data.frame(c("2019_p","2020_p","2020_t","2021_t","2019_p","2020_p","2020_t","2021_t"), c(rep("Escondida",4),rep("Sinai",4)),
                 c(mat_mean[1,],mat_mean[2,]),c(mat_se[1,],mat_se[2,])/sqrt(n))

 colnames(mat)<-c("Cosecha","Up","mean","Sd")
 plot2 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),c("Escondida", "Sinai"),"","Incidence",0,150,"Harvest","b")

 require(gridExtra)
 plot<-grid.arrange(plot1,plot2, ncol=2,nrow=1)

 ggsave(plot,filename="Timepo0_barplot_up.TIFF",device="tiff", dpi=600, width = 28.5, height = 14, units = "cm")


###
##############Upxlote
####
 mat_mean<-tapply(Datos_mat$Severidad_0,list(Datos_mat$Lote,Datos_mat$Cosecha),mean)
 mat_se<-tapply(Datos_mat$Severidad_0,list(Datos_mat$Lote,Datos_mat$Cosecha),sd)

 n<-c(table(Datos_mat$Cosecha[Datos_mat$Lote=="Bo"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="CR"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="EC"]),
       table(Datos_mat$Cosecha[Datos_mat$Lote=="Eu"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="Fr"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="FV"]),
       table(Datos_mat$Cosecha[Datos_mat$Lote=="Ta"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="To"]))
  mat<-data.frame(rep(c("2019_p","2020_p","2020_t","2021_t"),8), c(rep("Bo",4),rep("CR",4),rep("Ec",4),rep("Eu",4),rep("Fr",4),rep("FV",4),rep("Ta",4),rep("To",4)),
                  c(mat_mean[1,],mat_mean[2,],mat_mean[3,],mat_mean[4,],mat_mean[5,],mat_mean[6,],mat_mean[7,],mat_mean[8,]),
                  c(mat_se[1,],mat_se[2,],mat_se[3,],mat_se[4,],mat_se[5,],mat_se[6,],mat_se[7,],mat_se[8,])/sqrt(n))

n<-c(table(Datos_mat$Cosecha[Datos_mat$Lote=="1"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="3"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="4"]),
table(Datos_mat$Cosecha[Datos_mat$Lote=="5"]),table(Datos_mat$Cosecha[Datos_mat$Lote=="6"]))

mat<-data.frame(rep(c("2019_p","2020_p","2020_t","2021_t"),5), c(rep("1",4),rep("3",4),rep("4",4),rep("5",4),rep("6",4)),
c(mat_mean[1,],mat_mean[2,],mat_mean[3,],mat_mean[4,],mat_mean[5,]),
c(mat_se[1,],mat_se[2,],mat_se[3,],mat_se[4,],mat_se[5,])/sqrt(n))

 colnames(mat)<-c("Cosecha","Lote","mean","Sd")

 plot1 <- barplot_treatments(mat,mat$Lote,mat$mean,mat$Sd,mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),c("1","3","4","5","6"),"","Severity (%)",0,1.8,"Harvest","a")
 #plot1 <- barplot_treatments(mat,mat$Lote,mat$mean,mat$Sd,mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),names( table(mat$Lote)),"","Severity (%)",0,2.8,"Harvest","a")

 mat_mean<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$Lote,Datos_mat$Cosecha),mean)
 mat_se<-tapply(Datos_mat$Incidencia_0,list(Datos_mat$Lote,Datos_mat$Cosecha),sd)

  mat<-data.frame(rep(c("2019_p","2020_p","2020_t","2021_t"),8), c(rep("Bo",4),rep("CR",4),rep("Ec",4),rep("Eu",4),rep("Fr",4),rep("FV",4),rep("Ta",4),rep("To",4)),
                  c(mat_mean[1,],mat_mean[2,],mat_mean[3,],mat_mean[4,],mat_mean[5,],mat_mean[6,],mat_mean[7,],mat_mean[8,]),
                  c(mat_se[1,],mat_se[2,],mat_se[3,],mat_se[4,],mat_se[5,],mat_se[6,],mat_se[7,],mat_se[8,])/sqrt(n))


   mat<-data.frame(rep(c("2019_p","2020_p","2020_t","2021_t"),5), c(rep("1",4),rep("3",4),rep("4",4),rep("5",4),rep("6",4)),
                 c(mat_mean[1,],mat_mean[2,],mat_mean[3,],mat_mean[4,],mat_mean[5,]),
                 c(mat_se[1,],mat_se[2,],mat_se[3,],mat_se[4,],mat_se[5,])/sqrt(n))

 colnames(mat)<-c("Cosecha","Lote","mean","Sd")


 plot2 <- barplot_treatments(mat,mat$Lote,mat$mean,mat$Sd,mat$Cosecha,c("2019_p","2020_t","2020_p","2021_t"),names( table(mat$Lote)),"","Incidence",0,150,"Harvest","b")

 require(gridExtra)
 plot<-grid.arrange(plot1,plot2, ncol=2,nrow=1)

 ggsave(plot,filename="Fig Sumplementary 1.TIFF",device="tiff", dpi=600, width = 34, height = 16, units = "cm")

 ###
 ##############tiempo geneal
 ####
 Datos_mat<-Datos_mat1

 #Datos_mat<-subset(Datos_mat1,Datos_mat1$Cosecha=="2019_p")

 y<-c(round(Datos_mat$Severidad_0),round(Datos_mat$Severidad_1))
 up_t<-c(Datos_mat$UP,Datos_mat$UP)
 medida<-factor(c(rep("0dpc",length(y)/2),rep("21dpc",length(y)/2)))


 mat_mean<-tapply(y,list(medida,up_t),mean)
 mat_se<-tapply(y,list(medida,up_t),sd)
 n<-c(table(Datos_mat$UP),table(Datos_mat$UP))
 mat<-data.frame(rep(c("Escondida","Sinai"),2), c(rep("0dpc",2),rep("21dpc",2)),
                 c(mat_mean[1,],mat_mean[2,]),
                 c(mat_se[1,],mat_se[2,])/sqrt(n))

 colnames(mat)<-c("Up","dpc","mean","Sd")

 plot1 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,mat$dpc,c("0dpc","21dpc"),c("Escondida","Sinai"),"","Severity (%)",0,5,"dpc","a")



  
 #Datos_mat<-subset(Datos_mat1,Datos_mat1$Cosecha=="2021_t")



 y<-c(round(Datos_mat$Incidencia_0),round(Datos_mat$Incidencia_1))
 up_t<-c(Datos_mat$UP,Datos_mat$UP)
 medida<-factor(c(rep("0dpc",length(y)/2),rep("21dpc",length(y)/2)))


 mat_mean<-tapply(y,list(medida,up_t),mean)
 mat_se<-tapply(y,list(medida,up_t),sd)
 n<-c(table(Datos_mat$UP),table(Datos_mat$UP))
 mat<-data.frame(rep(c("Escondida","Sinai"),2), c(rep("0dpc",2),rep("21dpc",2)),
                 c(mat_mean[1,],mat_mean[2,]),
                 c(mat_se[1,],mat_se[2,])/sqrt(n))

 colnames(mat)<-c("Up","dpc","mean","Sd")
 plot2 <- barplot_treatments(mat,mat$Up,mat$mean,mat$Sd,mat$dpc,c("0dpc","21dpc"),c("Escondida","Sinai"),"","Incidence",0,250,"dpc","b")

 require(gridExtra)
 plot<-grid.arrange(plot1,plot2, ncol=2,nrow=1)

 ggsave(plot,filename="Fig 2.TIFF",device="tiff", dpi=600,width = 28.5, height = 14, units = "cm")



