rm(list=ls(all=TRUE))

#generando estructuras de carpetas 
dir.create(file.path(".","salidas","graficos"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","pdf"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","excel"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","html"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","scrip"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","rmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","qmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","cod"),showWarnings = FALSE, recursive=TRUE)

#Datos
library(readxl)
datosexcel<-readxl::read_excel(file.path(".","BD","dca.xlsx"))
datosexcel<-as.data.frame(datosexcel)
datosexcel$tratamiemto_f<-as.factor(datosexcel$tratamiento)
str(datosexcel)

datosexcel$tratamiento_c<-as.character(datosexcel$tratamiento)
str(datosexcel)


datosexcel$tratamiento_f <- as.factor(datosexcel$tratamiento)
str(datosexcel)
levels(datosexcel$tratamiento_f)
nlevels(datosexcel$tratamiento_f)


#Estadistica descriptiva

#funciones previas
descriptiva <- aggregate(nitrogeno ~ tratamiento,data = datosexcel, mean)
descriptivasd<- aggregate(nitrogeno ~ tratamiento,data = datosexcel, sd)
descriptiva

#funcion personalizando aggregate forma 1 
descriptivasd<-aggregate(nitrogeno~tratamiento,data = datosexcel, function(x){
    media<-mean(x)
    sd <- sd(x)
    n <- length(x)
    #t(alpha/2,gl)*s/raiz(n)
    delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
    return(c(Media=media,Ds=sd,N=n,Delta=delta,Inferior=media-delta,superior=media+delta,Maximo=max(x),Minimo=min(x),Mediana=median(x)))
})
descriptivasd

#poner por fuera las funciones: resumen forma 2 personalizando agregate

resumen<-function(x){
    media<-mean(x)
    sd <- sd(x)
    n <- length(x)
    #t(alpha/2,gl)*s/raiz(n)
    delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
    return(c(Media=media,Ds=sd,N=n,Delta=delta,Inferior=media-delta,superior=media+delta,Maximo=max(x),Minimo=min(x),Mediana=median(x)))
}
descriptiva <- aggregate(nitrogeno ~ tratamiento,data = datosexcel, resumen)
descriptivasd

#forma 3
descriptiva_tapply<-tapply(datosexcel$nitrogeno,datosexcel$tratamiento,resumen)
descriptiva2<-do.call(rbind,descriptiva_tapply)
descriptiva
str(descriptiva2)
barplot(descriptiva2[,"Media"])

##formas de acceder a una lista 
str(descriptiva)
str(descriptiva_tapply)
descriptiva_tapply$`3D0K1`
descriptiva_tapply[[1]]
names(descriptiva_tapply)
descriptiva_tapply[["3D0K1"]]




#tipos de raficos
boxplot(nitrogeno~tratamiento,data=datosexcel)
barras_personalizado<-function (media,mediana,inferior,superior,color_barras,color_datos,
 color_media,color_mediana,names_trt,repeticiones,ydatosexcel,jpg=FALSE){
  num_oro<-(1+sqrt(5))/2
  if (jpg) {
     jpeg(file.path(".", "salidas", "graficos", "barras.jpg"),units="px",
     height=5*600,width=num_oro*5*600,res=600,quality=100)
    }

  bp <- barplot(descriptiva$nitrogeno[,"Media"],col=adjustcolor(color_barras,alpha.f=0.20),border=color_barras,xlab="Cepas de Rhizobium sp.",ylab="Nitrogeno (mg)",ylim=c(0,40),names.arg=descriptiva$tratamiento,cex.axis=1,cex.names=0.9,cex.lab=1.1,width=0.8,space=0.5)
  arrows(bp,descriptiva$nitrogeno[,"Inferior"],bp,descriptiva$nitrogeno[,"superior"],length=0.07,lwd=1.3,code=3,angle=90,col="gray30")

  points(x=rep(bp,each=5),y=datosexcel$nitrogeno[order(datosexcel$tratamiento_f)],pch=21,cex=0.7,col=color_datos,bg=color_datos)
  # Media: rombo
  points(x=bp,y=as.numeric(descriptiva$nitrogeno[,"Media"]),pch=18,cex=1.5,col=color_media)
  # Mediana: triángulo
  points(x=bp,y=as.numeric(descriptiva$nitrogeno[,"Mediana"]),pch=17,cex=1.1,col=color_mediana)

  # Leyenda
  legend("topright",legend=c("Media","Mediana","Datos individuales","IC 95%"),pch=c(18,17,21,NA),lty=c(NA,NA,NA,1),
  col=c(color_media,color_mediana,color_datos,"gray30"),bty="n",cex=0.8)
  
  #Fig 1. Grafico descriptivo para el contenido de Nitrogeno en 6 tratamientos de cepas de Rhizobium.
  #Las barras indican la media del tratamiento.
  #Los rombos representan la media que resalta el color azul y los triangulos la mediana de color naranja
  #Los puntos representan las observaciones individuales.
  #Las barras de error indican los limites del intervalo de confianza al 95%.
 if  (jpg) dev.off()
  
}

barras_personalizado( 
 media    = descriptiva$nitrogeno[, "Media"],
 mediana  = descriptiva$nitrogeno[, "Mediana"],
 inferior = descriptiva$nitrogeno[, "Inferior"],
 superior = descriptiva$nitrogeno[, "Superior"],
 names_trt = descriptiva$tratamiento, 
 color_barras = "#757DF0",
 color_datos  = "#252525",
 color_media  = "#0000FF",   
 color_mediana = "#FFA500", 
 repeticiones=5,ydatosexcel = datos$nitrogeno[order(datosexcel$tratamiento)],jpg=TRUE)


#barplot(descriptiva$nitrogeno[,"Media"])
descriptiva

ls()





