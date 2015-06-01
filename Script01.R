##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Arias Navarrete Rubi


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

dir <- "C:/Users/Rubi Arias/Desktop/Programa R/Modelos-Lineales-Trabajos"
setwd(dir)

data<-read.table("data.txt",header=TRUE,dec=",",sep="\t")
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

minimo<-min(data[,"Edad"],na.rm=TRUE)
minimo
media<-mean(data[,"Edad"],na.rm=TRUE)
media
maximo<-max(data[,"Edad"],na.rm=TRUE)
maximo

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

num_femenino<-subset(data,subset=data[,"Genero"]=="Femenino")
table(num_femenino[,"Genero"])
gene_femenino<-nrow(num_femenino)
gene_femenino  


# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

dependientes<-subset(data,subset=data[,"Dependiente"]=="Si")
minimo<-min(dependientes[,"Edad"],na.rm=TRUE)
minimo
media<-mean(dependientes[,"Edad"],na.rm=TRUE)
media
maximo<-max(dependientes[,"Edad"],na.rm=TRUE)
maximo
# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipos<-numeric(ncol(data))
for(i in 1:ncol(data)){
  tipos[i]<-typeof(data[,i])
}
tipos

# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase<-numeric(ncol(data))
for(i in 1:ncol(data)){
  clase[i]<-class(data[,i])
}
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

medias_varinum<-numeric(ncol(data))
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])==TRUE){
    medias_varinum[i]<-mean(data[,i],na.rm=TRUE)
  }
}
medias_varinum


# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

valores_perdidos<-numeric(ncol(data))
porcentaje_valperd<-numeric(ncol(data))
for(i in 1:ncol(data)){
  valores_perdidos[i]<-sum(is.na(data[,i]))
  porcentaje_valperd[i]<-((100*valores_perdidos[i])/nrow(data))
}
porcentaje_valperd


# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

sujs_mayor40<-subset(data,subset=data[,"Edad"]>40)
str(sujs_mayor40)

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

vivienda_propia<-subset(data,subset=data[,"Vivienda"]=="Propia")
str(vivienda_propia) 

# 3.3 Seleccione los sujetos que tienen más (>) de dos cargas familiatres.

cargas_familia<-subset(data,subset=data[,"Cargas"]>2)
str(cargas_familia)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más (>) de 8 Dias_Atraso.

deudsup_diatra<-subset(data,subset=data[,"Deuda"]>=500 & data[,"Dias_Atraso"]>8)
str(deudsup_diatra)

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más (>) de 3 tarjetas de crédito (Numero_TC).

scoremay_edame_tarjcr<-subset(data,subset=data[,"Score"]>=900 & data[,"Edad"]<=35 & data[,"Numero_TC"]>3)
str(scoremay_edame_tarjcr)

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(data[,"Edad"],col="red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(data[,"Edad"],main = "Diagrama de caja de la Variable Edad",xlab = "X ", ylab = "Y Edad",col = "green")