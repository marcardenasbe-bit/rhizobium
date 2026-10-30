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

datosexcel$tratamiento_c<-as.character(datosexcel$tratamiento)
str(datosexcel)


#Datos
library(readxl)
datosexcel<-read_excel(file.path(".","BD","dca.xlsx"))
datosexcel<-as.data.frame(datosexcel)
datosexcel$tratamiemto_f<-as.factor(datosexcel$tratamiento)
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
##formas de acceder a una lista 
str(descriptiva)
str(descriptiva_tapply)
descriptiva_tapply$`3D0K1`
descriptiva_tapply[[1]]
names(descriptiva_tapply)
descriptiva_tapply[["3D0K1"]]
str(descriptiva2)

#tipos de raficos
boxplot(nitrogeno~tratamiento,data=datosexcel)


barplot(descriptiva$nitrogeno[,"Media"])

names(descriptivasd)
str(descriptivasd)

#grafica con color 
num_oro<-(1+sqrt(5))/2
jpeg(file.path(".", "salidas", "graficos", "barras.jpg"),
     units = "px",
     height = 5*1200,
     width = num_oro*5*1200,
     res = 1200,
     quality = 100)

bp <- barplot(descriptivasd$nitrogeno[, "Media"],
              col = adjustcolor("green", alpha.f = 0.3),
              xlab = "Cepas de Rhizobium sp.",
              ylab = "Nitrogeno (mg)",
              ylim = c(0, 40),
              names.arg = descriptiva$tratamiento,
              cex.axis = 0.7)


arrows(bp,descriptiva$nitrogeno[,"Inferior"],bp,descriptiva$nitrogeno[,"superior"],length = 0.05, lwd=1.5,code=3,angle=90)

points(x=rep(bp,each = 5),y=datosexcel$nitrogeno[order(datosexcel$tratamiento_f)],pch=24,cex=0.5)
points(x=bp,y=descriptiva$nitrogeno[,"Mediana"],pch=18,cex=09,col=red)
dev.off()
descriptiva$tratamiento


