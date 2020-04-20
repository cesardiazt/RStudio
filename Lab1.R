#Parte 1

#P1

correlacion<-function(x,y){
  num<-length(x)*sum(x*y)-sum(x)*sum(y)
  den<-sqrt((length(x)*sum(x*x)-(sum(x)^2))*(length(y)*sum(y*y)-(sum(y)^2)))
  return(num/den)
}

#P2
perimetro<-function(x,y){
  d<-0
  i<-1
 while (i < length(x)){
   aux<-sqrt((x[i+1]-x[i])^2+(y[i+1]-y[i])^2)
   d<-d+aux
   i<-i+1
 }
  return(d+sqrt((x[length(x)]-x[1])^2+(y[length(x)]-y[1])^2))
}

#P3
factorial<-function(n){
  if(n==0){return(1)}
  else{return(n*factorial(n-1))}  
 }
aproxEuler<-function(epsilon){
  e<-0
  i<-0
  while((1/factorial(i))> epsilon){
    delta <- 1/factorial(i)
    e<- e + delta
    i<- i + 1
  } 
  return(e)
}

##
#Parte 2
# P1
data<-read.csv('datos.txt',header = T)
View(data)

# cuenta los NA
sum(is.na(data))
# entregó 0 como resultado por lo que no hay datos NA
# en este set de datos se enumera la cantidad que cada 
# concepto fue mencionado en caso de no ser mencionado solo aparece un 0

# P2
data2<-subset(data,data$a_la_educacion==0)
# Se filtra el dataFrame con la consicion "a_la_educacion==0"
length(data2$localidad)
# Se cuenta la cantidad de localidades
# entrega 21, por lo que 21 localidades no mencionaron a_la_educación

# P3
data$total<-rowSums(data[2:ncol(data)])
# Se crea la nueva columna con nombre "total" donde se asigna 
# la suma de las columnas comprendidas entre la "2" y "ncol(data)" (la última)


# P4
# Mayor Participacion
subset(data,data$total==max(data$total))[1]
# Se obtiene el cubconjunto donde se filtra por el valor maximo de la columna total
# y se obtiene los valores de la primera columna (localidades) en este caso es solo 1
# la localidad con el valor máximo (en caso de haber más de 1 máximo tambien los entregaria)
#Entrega
#localidad
#276 Metropolitana de Santiago/Santiago/Santiago

# Se obtiene el cubconjunto donde se filtra por el valor mínimo de la columna total
# y se obtiene los valores de la primera columna (localidades) en este caso es solo 1
# la localidad con el valor mínimo (en caso de haber más de 1 mínimo tambien los entregaria)
# Menor Participacion
subset(data,data$total==min(data$total))[1]
#                localidad
# 225 Maule/Talca/Empedrado

# P5
# Se ordena el Dataframe utilizando la columna "total" para ordenar
# se utiliza el modo decreciente para que queden de mayor a menor
decreciente <-data[order(data$total,decreasing=TRUE),]
# Muestra las primeras 5 localidades
head(decreciente[1],5)
# Entrega
#localidad
#276    Metropolitana de Santiago/Santiago/Santiago
#267 Metropolitana de Santiago/Santiago/Providencia
#278       Metropolitana de Santiago/Santiago/Nunoa
#259  Metropolitana de Santiago/Santiago/Las Condes
#327               Valparaiso/Valparaiso/Valparaiso

# Se ordena el Dataframe utilizando la columna "total" para ordenar
# se utiliza el modo decreciente desativado para que queden de menor a mayor
creciente <-data[order(data$total,decreasing=TRUE),]
# Muestra las primeras 5 localidades
head(creciente[1],5)
# Entrega
#localidad
#225                                            Maule/Talca/Empedrado
#176                                    Los Lagos/Osorno/Puerto Octay
#75                                           Biobio/Nuble/San Fabian
#36                                        Biobio/Bio Bio/Alto Biobio
#3   Aisen del General Carlos Ibanez del Campo/Capitan Prat/O'higgins


# P6
# Ordena de forma decreciente por la columna de_los_pueblos_indigenas
indigena <-data[order(data$de_los_pueblos_indigenas,decreasing=TRUE),]
# Muestra los primeros 10 registro de localidades
head(indigena[1],10)

# Entrega
#localidad
#276    Metropolitana de Santiago/Santiago/Santiago
#267 Metropolitana de Santiago/Santiago/Providencia
#278       Metropolitana de Santiago/Santiago/Nunoa
#327               Valparaiso/Valparaiso/Valparaiso
#255  Metropolitana de Santiago/Santiago/La Florida
#328             Valparaiso/Valparaiso/Vina del Mar
#112                     La Araucania/Cautin/Temuco
#258    Metropolitana de Santiago/Santiago/La Reina
#259  Metropolitana de Santiago/Santiago/Las Condes
#264       Metropolitana de Santiago/Santiago/Maipu
