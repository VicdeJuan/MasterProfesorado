# Codigo fuente basado en el post "http://analisisydecision.es/simulacion-estimacion-de-pi-con-el-metodo-montecarlo/"
# PARAMETROS DE ENTRADA
NumeroDeTizazos <- 10000

RadioCircunferencia <- 1
plotActive=1

# SCRIPT

options(echo=FALSE)

x=runif(NumeroDeTizazos ,0,RadioCircunferencia)
y=runif(NumeroDeTizazos ,0,RadioCircunferencia)
puntos=cbind(x,y)
simul=data.frame(puntos)

#simul$dist es la distancia de cada punto al origen de coordenadas
simul$dist=sqrt(x**2+y**2)
simul$dentro=as.factor((simul$dist<=RadioCircunferencia)*1)

if (plotActive==1){
  cat("ACTIVO = ",plotActive)
  windows()
  plot(simul$x,
       simul$y,
       panel.first=grid(10,10),
       pch=c(1,2)[simul$dentro],
       col=c("red","green")[simul$dentro],
       asp = 1, 
       xlab = "Eje X", 
       ylab = "Eje Y")
}

# Circunferencia es x^2+y^2=R^2 ==> y=sqrt(R^2-x**2)
x=seq(0,RadioCircunferencia**2,length.out=100) # Genero una secuencia de valoers para la x
y=sqrt(RadioCircunferencia**2-x**2) # calculo los valores de y para cada x
circunferencia=data.frame(cbind(x,y))

if (plotActive==1){
  lines(circunferencia,col="white",lwd=5)
}

puntosTotales=nrow(simul)
puntosDentro=nrow(subset(simul,dentro=="1"))
porcentajeDentro=(puntosDentro/puntosTotales)*100
piEstimado=(porcentajeDentro/100)*4

cat("Porcentaje de puntos dentro = ",porcentajeDentro,"%\n")
cat("El valor estimado de PI es = ",piEstimado,"\n")




