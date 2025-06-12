
#importamos la libreria de readxl para leer los archivos de excel
library(readxl)
#para la funcion filter
library(stats)
#file.choose() esta funcion muestra la ruta del archivo #DESCOMENTA ESTA FUNCION SI NO CONOCES TU RUTA
ruta_excel <- "AQUI PON TU RUTA DE EXCEL"
excel_sheets(ruta_excel)
#cargamos los datos a nuestras variables por cada ejercicio
ejer1<- read_excel(ruta_excel, sheet="Ejer1")
ejer2<- read_excel(ruta_excel, sheet="Ejer2")
ejer3<- read_excel(ruta_excel, sheet="Ejer3")
ejer4<- read_excel(ruta_excel, sheet="Ejer4")
ejer5<- read_excel(ruta_excel, sheet="Ejer5")
ejer6<- read_excel(ruta_excel, sheet="Ejer6")

#aner variable usada para mandar el error a pema dam y emc dentro de la funcion resultados
#Funcion que calcula los errores
error <- function (observacion, prediccion, numero)
{
  prediccionpara<- prediccion[-(1:numero)]
  observacionpara <- observacion[-(1:numero)]
  errores<- (observacionpara - prediccionpara)
  return (errores)
}

#Función para calcular el error cuadrático medio (EMC)
calcular_emc <- function(observacion, aner) 
{
  errores_cuadraticos <- aner^2
  emc <- mean(errores_cuadraticos)
  return(emc)
}

#Funcion para calcular la Desviacion absoluta media (DAM)
calcular_dam <- function(observacion, aner)
{
  errores_abs <- abs(aner)
  dam <- mean (errores_abs)
  return(dam)
}

#Funcion para calcular el Porcentaje de Error Medio Absoluto (PEMA)
calcular_pema <- function (observacion, aner, numero)
{
  errores_pema <- abs(aner)/ observacion[-(1:numero)]
  pema <- mean(errores_pema)*100
  return(pema)
}

#Funcion que conjunta todo
resultados <- function(observacion, numero)
{
  n<- numero+1
  peso <- rep(1/n, n) #vector de pesos, si numero=1 entonces peso 1/2, 1/2
  PMP <-filter(observacion, peso, sides = 1) #se calcula el pmp
  ERROR <- error(observacion, PMP, numero)
  DAM <- calcular_dam(observacion,ERROR)
  EMC <- calcular_emc(observacion,ERROR)  
  PEMA <- calcular_pema(observacion, ERROR, numero)
  print(PMP)
  cat("Pronostico= ", tail(PMP, n = 1), "\n")
  cat("DAM= ", DAM, "\n")
  cat("EMC=", EMC, "\n")
  cat("PEMA=", PEMA, "%\n")
  # Graficar observacion y PMP
  plot(observacion, type = "l", col = "blue", ylim = range(c(observacion, PMP[-(1:numero)])), xlab = "Tiempo", ylab = "Valor")
  lines(PMP, col = "red")
  legend("topleft", legend = c("Real", "PMP"), col = c("blue", "red"), lty = 1)
}

#nota, si queremos para n=2 escribir 1 en el parametro, si n=3 escribir 2 en el parametro 
resultados(ejer1$VentasPorAccion, 2)
resultados(ejer2$GastoEnPublicidad, 2)
resultados(ejer3$Cierre, 2)
resultados(ejer4$MillDol, 2)
resultados(ejer5$Demanda, 2)
resultados(ejer6$Salario, 2)