# Codigo fuente basado en el post 
# "http://analisisydecision.es/simulacion-estimacion-de-pi-con-el-metodo-montecarlo/"

# PARAMETROS DE ENTRADA
args <- commandArgs(trailingOnly = TRUE)
if (length(args)>=1){
  NumeroDeTizazos=as.integer(args[1])
}else{
  NumeroDeTizazos=1000
}

RadioCircunferencia=1
numeroDeVeces=100

# SCRIPT
options(echo=FALSE)

estimaciones<-vector('numeric')

for (i in 1:numeroDeVeces){
  x=runif(NumeroDeTizazos ,-RadioCircunferencia,RadioCircunferencia)
  y=runif(NumeroDeTizazos ,-RadioCircunferencia,RadioCircunferencia)
  puntos=cbind(x,y)
  simul=data.frame(puntos)
  
  #simul$dist es la distancia de cada punto al origen de coordenadas
  simul$dist=sqrt(x**2+y**2)
  simul$dentro=as.factor((simul$dist<=RadioCircunferencia)*1)
  
  #Estimacion de Pi
  puntosTotales=nrow(simul)
  puntosDentro=nrow(subset(simul,dentro=="1"))
  porcentajeDentro=(puntosDentro/puntosTotales)*100
  piEstimado=(porcentajeDentro/100)*4
  
  estimaciones[i]=round(piEstimado,2)
  # cat("Numero de tizazos a simular = ",NumeroDeTizazos, "\n")
  # cat("Porcentaje de puntos dentro = ",porcentajeDentro,"%\n")
  # cat("El valor estimado de PI es = ",piEstimado,"\n")
}

cat(numeroDeVeces, " simulaciones con ",NumeroDeTizazos," tizazos\n")
media=mean(estimaciones)
cat("El valor medio de PI es ",media,"\n")
mediana=median(estimaciones)
cat("Mediana ",mediana,"\n")
minimo=min(estimaciones)
cat("Minimo ",minimo,"\n")
maximo=max(estimaciones)
cat("MAximo ",maximo,"\n")
hist(estimaciones,breaks = 100)
estimaciones