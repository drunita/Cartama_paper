library(datasets)
library(ggplot2)
library(lme4)
library(grid)
library(gridExtra)


###COMPRAR Y GUARDAR EL MODELO QE MEJOR EXPLIQUE EL SISTEMA
###model_comparision(modelo mas complejo,modelo mas simple,nombre de archivo para guardar modelo)

##y<-as.numeric(Datos_mat$Severidad_0)
##modelo<-lmer(y~up + (1|ul)+(1|ula),REML = FALSE)
###modelo1<-lmer(y~1 + (1|ul)+(1|ula),REML = FALSE)

#### PE:model_comparision(modelo,modelo1,"/xx.txt")

model_comparision<-function(a,b,saving_model){
  
  if (anova(a,b)$Pr[2]<0.05){
    print("el modelo mas complejo fue guardado")
    sink(saving_model)
    print(summary(a))
    sink() 
  }else{
    print("Me quedo con el modelo mas simple")
    sink(saving_model)
    
    print(summary(b))
    sink()}
  return(anova(a,b)$Pr[2])
}

########Hacer el grafico boxplot
######boxplot_por_lotes(Datos,factor (x),variable (y), grupos,orden_grupos,factores nombres (ejex),eje_x,eje_y,plot_title,limite menor eje y, limite mayor eje y, titulo legenda)
#####plot1 <- boxplot_treatments(Datos_mat,Datos_mat$UP,Datos_mat$Severidad_0,Datos_mat$Cosecha,c("2019_p","2020_t","2020_p"),c("Escondida", "Sinai"),"","Severidad  (%)","",0,5,"Cosecha")

boxplot_treatments<-function(Datos_mat,factor_x,variable_y,group,levels_order,names_x,eje_x,eje_y,plot_title,y_min,y_max,title_legend){
  
  
  p<-ggplot(Datos_mat,aes(factor(factor_x),as.numeric(variable_y), color = factor(group, levels=levels_order)))
  p<- p + geom_boxplot() + theme_classic() 
  p<- p + geom_boxplot(outlier.colour="black", outlier.size=1)
  p<- p + scale_x_discrete(limits=names_x)+labs(title=plot_title)
  p<- p + theme(plot.title=element_text(size=18,face="bold",hjust = 0.5,family = 'Times'))
  p<- p +  xlab(paste("\n",eje_x,sep="")) + ylab(paste(eje_y,"\n",sep=""))+ylim(y_min, y_max)
  p<- p +theme(axis.title=element_text(size=14,face="bold",family = 'Times'))
  p<- p +  theme(axis.text.x = element_text(face="bold",size=12, angle=0,family = 'Times'),
                 axis.text.y = element_text(face="bold",size=12, angle=0,family = 'Times'))
  p<- p + theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 1.1, 0.5, 1.3, "cm"),
                plot.background = element_rect(fill = "white",colour = "black",size = 0.3))
  p <- p +  labs(color=title_legend)
  p<-  p+ theme(legend.position = "top",legend.text = element_text(size=12, angle=0,family = 'Times'),
                legend.title = element_text(face="bold",size=12, angle=0,family = 'Times'))
  return(p)
  
}
barplot_treatments<-function(Datos_mat,factor_x,variable_y,variable_se,group,levels_order,names_x,eje_x,eje_y,y_min,y_max,title_legend,tag_letter){
  
  p<-ggplot(Datos_mat,aes(factor(factor_x),as.numeric(variable_y),fill = factor(group, levels=levels_order)))
  p<- p  + theme_classic() 
  p<- p +  geom_bar(stat="identity", color="black", position=position_dodge()) 
  p<- p + geom_errorbar(aes(ymin=variable_y-variable_se, ymax=variable_y+variable_se), width=.2, position=position_dodge(.9))
  p<- p + scale_x_discrete(limits=names_x)
  p<- p + theme(plot.title=element_text(size=18,face="bold",hjust = 0.5,family = 'Times'))
  p<- p +  xlab(paste("\n",eje_x,sep="")) + ylab(paste(eje_y,"\n",sep=""))+ylim(y_min, y_max)
  p<- p +theme(axis.title=element_text(size=14,face="bold",family = 'Times'))
  p<- p +  theme(axis.text.x = element_text(face="bold",size=12, angle=0,family = 'Times'),
                 axis.text.y = element_text(face="bold",size=12, angle=0,family = 'Times'))
  p<- p + theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 1.1, 0.5, 1.3, "cm"),
                plot.background = element_rect(fill = "white",colour = "black",size = 0.3))
  p <- p +  labs(fill=title_legend)
  p<-  p+ theme(legend.position = "top",legend.text = element_text(size=12, angle=0,family = 'Times'),
                legend.title = element_text(face="bold",size=12, angle=0,family = 'Times'))+ labs(tag = tag_letter)
  return(p)
  
}
stack_barplot<-function(Datos_mat,factor_x,variable_y,name_cols,levels_order,names_x,eje_x,eje_y,title_legend,tag_letter){
  
  mat<-as.data.frame(table(factor_x,variable_y))
  colnames(mat)<-name_cols
  mat$y[mat$up=="Escondida"]<-mat$y[mat$up=="Escondida"]/(sum(mat$y[mat$up=="Escondida"]))
  mat$y[mat$up=="Sinai"]<-mat$y[mat$up=="Sinai"]/(sum(mat$y[mat$up=="Sinai"]))
  p <- ggplot(mat, aes(fill=factor(sev, levels=levels_order), y=y, x=up))  
  p <- p +  geom_bar(position="stack", stat="identity")
  p<- p + scale_x_discrete(limits=names_x)
  p<- p + theme(plot.title=element_text(size=18,face="bold",hjust = 0.5,family = 'Times'))
  p<- p +  xlab(paste("\n",eje_x,sep="")) + ylab(paste(eje_y,"\n",sep=""))
  p<- p +theme(axis.title=element_text(size=14,face="bold",family = 'Times'))
  p<- p +  theme(axis.text.x = element_text(face="bold",size=12, angle=0,family = 'Times'),
                 axis.text.y = element_text(face="bold",size=12, angle=0,family = 'Times'))
  p<- p + theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 1.1, 0.5, 1.3, "cm"),
                plot.background = element_rect(fill = "white",colour = "black",size = 0.3))
  p <- p +  labs(fill=title_legend)
  p<-  p+ theme(legend.text = element_text(size=12, angle=0,family = 'Times'),
                legend.title = element_text(face="bold",size=12, angle=0,family = 'Times'))+ labs(tag = tag_letter)
  return(p)
  
}

