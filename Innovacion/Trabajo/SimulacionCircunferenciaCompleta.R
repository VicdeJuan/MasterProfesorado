# Codigo fuente basado en el post 
# "http://analisisydecision.es/simulacion-estimacion-de-pi-con-el-metodo-montecarlo/"

# PARAMETROS DE ENTRADA
args <- commandArgs(trailingOnly = TRUE)
if (length(args)>=1){
  NumeroDeTizazos=as.integer(args[1])
}else{
  NumeroDeTizazos=10000
}

RadioCircunferencia=1
plotActive=1

# SCRIPT
options(echo=FALSE)

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

if (plotActive==1){
  windows()
  # Pintar los tizazos
  plot(simul$x,
       simul$y,
       panel.first=grid(10,10),
       pch=c(1,2)[simul$dentro],
       col=c("red","green")[simul$dentro],
       asp = 1, 
       xlab = "Eje X", 
       ylab = "Eje Y")
  
  # Pintar la circunferencia
  # Circunferencia es x^2+y^2=R^2 ==> y=sqrt(R^2-x**2)
  
  NumeroPuntosCircunferencia=100
  
  x=seq(-(RadioCircunferencia**2),RadioCircunferencia**2,length.out=NumeroPuntosCircunferencia) # Genero una secuencia de valoers para la x
  y=sqrt(RadioCircunferencia**2-x**2) # calculo los valores de y para cada x
  puntosCircunferenciaPositivos=cbind(x,y)
  circunferenciaPositiva=data.frame(puntosCircunferenciaPositivos)
  
  x=seq(-(RadioCircunferencia**2),RadioCircunferencia**2,length.out=NumeroPuntosCircunferencia) # Genero una secuencia de valoers para la x
  y=-sqrt(RadioCircunferencia**2-x**2) # calculo los valores de y para cada x
  puntosCircunferenciaNegativos=cbind(x,y)
  circunferenciaNegativos=data.frame(puntosCircunferenciaNegativos)
  
  lines(circunferenciaPositiva,col="white",lwd=5)
  lines(puntosCircunferenciaNegativos,col="white",lwd=5)
  
  #Pintar los lados del cuadrado
  x=seq(-(RadioCircunferencia**2),RadioCircunferencia**2,length.out=NumeroPuntosCircunferencia) # Genero una secuencia de valoers para la x
  y=RadioCircunferencia
  alto=data.frame(cbind(x,y))
  y=-RadioCircunferencia
  bajo=data.frame(cbind(x,y))
  y=seq(-(RadioCircunferencia**2),RadioCircunferencia**2,length.out=NumeroPuntosCircunferencia) # Genero una secuencia de valoers para la x
  x=RadioCircunferencia
  ladoDerecho=data.frame(cbind(x,y))
  x=-RadioCircunferencia
  ladoIzquierdo=data.frame(cbind(x,y))
  
  lines(alto,col="black",lwd=5)
  lines(bajo,col="black",lwd=5)
  lines(ladoDerecho,col="black",lwd=5)
  lines(ladoIzquierdo,col="black",lwd=5)
  
  title(main="Simulacion de tizazos")
  text(0,RadioCircunferencia/2,cex=2,bquote(.(NumeroDeTizazos)~"tizazos"))
  text(0,RadioCircunferencia/4,cex=2,bquote(.(porcentajeDentro)~"% dentro"))
  text(0,0,cex=2,bquote(~ Pi  %~~% .(piEstimado)))
  
}

cat("Numero de tizazos a simular = ",NumeroDeTizazos, "\n")
cat("Porcentaje de puntos dentro = ",porcentajeDentro,"%\n")
cat("El valor estimado de PI es = ",piEstimado,"\n")
