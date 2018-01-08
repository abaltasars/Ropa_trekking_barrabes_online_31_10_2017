## Máster en ciencia de Datos
## Tipología y ciclo de vida de los datos
## Ángel Baltasar Sánchez


# importamos fichero csv
# Declaramos el directorio fuente y el directorio de salida
# A actualizar según sistema desde el que se trabaje


PathOrigen="c:\\PRACTICA2\\"
PathSalida="c:\\PRACTICA2\\Output\\"

prod<-read.csv(paste0(PathOrigen,"productos.csv"), header=TRUE, sep=",")

# Una copia del fichero productos.csv se puede encontrar en la siguiente URL
# https://github.com/abaltasars/PRODUCTOS_BARRABES_ONLINE_10_2017


# importamos las columnas categoria, url, precio en un nuevo dataframe


p<-prod[,c("categoria","url","precio")]


# convertimos las columnas a tipos que nos interesan

p$categoria <- as.character(p$categoria)
p$url<- as.character(p$url)
p$precio<-as.character(p$precio)

# ahora deberíamos convertir a numeric el precio

p$precio<-as.numeric(p$precio)

# ahora convertimos categoria a UTF-8 ya que segun el  Sistema Operativo  no lo hace

Encoding(p$categoria) <- "UTF-8"

# hacemos los mismo con url, aunque no sería necesario porque las URL tendrían que estar normalizadas.

Encoding(p$url) <- "UTF-8"

# necesitamos eliminar aquellas que tengan valores nulos en categoría o bien en precios
# cargamos en nuestro dataframe las columnas cuyo precio no es nulo

 p<-p[!is.na(p$precio),]

# eliminaremos filas repetidas

p<-unique(p)


# Ahora tenemos los productos preparados para ser tratados,

# Extraeremos las diferentes categorias que existen y de ellas crearemos subsets 
# para tener los grupos de productos y calcular los precios

categorias<-unique(p$categoria)
categorias<-sort(categorias)


# exportamos las diferentes categorias

write.csv(categorias, file = paste0(PathSalida,"categorias.csv"))

# Ahora crearemos los subsets en funcion las categorias

# Como es posible que algunos precios sean muy exagerados, los vamos a considerar como outliers, 
# vamos a eliminar aquellos valores que esten más alla de la media +/- 3 desviaciones estándards
# calcularemos n, media, mediana, sd y generaremos la normal, también generaremos los gráficos 
# con los Histogramas y con la curva Normal de los grupos de datos estudiados



#### calzado trekking

CalzadoHombre <- p[grep("Calzado de Montaña > Hombre > Botas Trekking",p$categoria),]
nCalzadoHombre<-length(CalzadoHombre$precio)
mediaCalzadoHombre <- mean(CalzadoHombre$precio)
medianaCalzadoHombre <- median(CalzadoHombre$precio)
varCalzadoHombre<- var(CalzadoHombre$precio)
sdCalzadoHombre <- sd(CalzadoHombre$precio)
normCalzadoHombre <- dnorm(CalzadoHombre$precio, mean=mediaCalzadoHombre, sd = sdCalzadoHombre)
CalzadoHombre3sd <-CalzadoHombre[CalzadoHombre$precio > mediaCalzadoHombre-(3*sdCalzadoHombre) &CalzadoHombre$precio < mediaCalzadoHombre+(3*sdCalzadoHombre),]
nCalzadoHombre3sd <- length(CalzadoHombre3sd$precio)
mediaCalzadoHombre3sd <- mean(CalzadoHombre3sd$precio)
medianaCalzadoHombre3sd <- median(CalzadoHombre3sd$precio)
varCalzadoHombre3sd <- var(CalzadoHombre3sd$precio)
sdCalzadoHombre3sd <- sd(CalzadoHombre3sd$precio)
normCalzadoHombre3sd <- dnorm(CalzadoHombre3sd$precio, mean=mediaCalzadoHombre3sd , sd = sdCalzadoHombre3sd)
jpeg(paste0(PathSalida,"CalzadoHombreHist.jpg"))
plot(CalzadoHombre$precio)
dev.off()
jpeg(paste0(PathSalida,"CalzadoHombre3sdHist.jpg"))
plot(CalzadoHombre3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"CalzadoHombreNorm.jpg"))
plot(CalzadoHombre$precio, normCalzadoHombre)
dev.off()
jpeg(paste0(PathSalida,"CalzadoHombre3sdNorm.jpg"))
plot(CalzadoHombre3sd$precio, normCalzadoHombre3sd)
dev.off()

### calcetines trekking

CalcetinesHombre <- p[grep("Calzado de Montaña > Hombre > Calcetines > Trekking",p$categoria),]
nCalcetinesHombre<-length(CalcetinesHombre$precio)
mediaCalcetinesHombre <- mean(CalcetinesHombre$precio)
medianaCalcetinesHombre <- median(CalcetinesHombre$precio)
varCalcetinesHombre<- var(CalcetinesHombre$precio)
sdCalcetinesHombre <- sd(CalcetinesHombre$precio)
normCalcetinesHombre <- dnorm(CalcetinesHombre$precio, mean=mediaCalcetinesHombre, sd = sdCalcetinesHombre)
CalcetinesHombre3sd <-CalcetinesHombre[CalcetinesHombre$precio > mediaCalcetinesHombre-(3*sdCalcetinesHombre) &CalcetinesHombre$precio < mediaCalcetinesHombre+(3*sdCalcetinesHombre),]
nCalcetinesHombre3sd <- length(CalcetinesHombre3sd$precio)
mediaCalcetinesHombre3sd <- mean(CalcetinesHombre3sd$precio)
medianaCalcetinesHombre3sd <- median(CalcetinesHombre3sd$precio)
varCalcetinesHombre3sd <- var(CalcetinesHombre3sd$precio)
sdCalcetinesHombre3sd <- sd(CalcetinesHombre3sd$precio)
normCalcetinesHombre3sd <- dnorm(CalcetinesHombre3sd$precio, mean=mediaCalcetinesHombre3sd , sd = sdCalcetinesHombre3sd)
jpeg(paste0(PathSalida,"CalcetinesHombreHist.jpg"))
plot(CalcetinesHombre$precio)
dev.off()
jpeg(paste0(PathSalida,"CalcetinesHombre3sdHist.jpg"))
plot(CalcetinesHombre3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"CalcetinesHombreNorm.jpg"))
plot(CalcetinesHombre$precio, normCalcetinesHombre)
dev.off()
jpeg(paste0(PathSalida,"CalcetinesHombre3sdNorm.jpg"))
plot(CalcetinesHombre3sd$precio, normCalcetinesHombre3sd)
dev.off()

## pantalon hombre

PantalonHombre <- p[grep("Ropa Montaña Hombre > Pantalones > Trekking >",p$categoria),]
nPantalonHombre<-length(PantalonHombre$precio)
mediaPantalonHombre <- mean(PantalonHombre$precio)
medianaPantalonHombre <- median(PantalonHombre$precio)
varPantalonHombre<- var(PantalonHombre$precio)
sdPantalonHombre <- sd(PantalonHombre$precio)
normPantalonHombre <- dnorm(PantalonHombre$precio, mean=mediaPantalonHombre, sd = sdPantalonHombre)
PantalonHombre3sd <-PantalonHombre[PantalonHombre$precio > mediaPantalonHombre-(3*sdPantalonHombre) &PantalonHombre$precio < mediaPantalonHombre+(3*sdPantalonHombre),]
nPantalonHombre3sd <- length(PantalonHombre3sd$precio)
mediaPantalonHombre3sd <- mean(PantalonHombre3sd$precio)
medianaPantalonHombre3sd <- median(PantalonHombre3sd$precio)
varPantalonHombre3sd <- var(PantalonHombre3sd$precio)
sdPantalonHombre3sd <- sd(PantalonHombre3sd$precio)
normPantalonHombre3sd <- dnorm(PantalonHombre3sd$precio, mean=mediaPantalonHombre3sd , sd = sdPantalonHombre3sd)
jpeg(paste0(PathSalida,"PantalonHombreHist.jpg"))
plot(PantalonHombre$precio)
dev.off()
jpeg(paste0(PathSalida,"PantalonHombre3sdHist.jpg"))
plot(PantalonHombre3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"PantalonHombreNorm.jpg"))
plot(PantalonHombre$precio, normPantalonHombre)
dev.off()
jpeg(paste0(PathSalida,"PantalonHombre3sdNorm.jpg"))
plot(PantalonHombre3sd$precio, normPantalonHombre3sd)
dev.off()


#CamisetaHombre

CamisetaHombre <- p[grep("Ropa Montaña Hombre > Camisetas ",p$categoria),]
nCamisetaHombre<-length(CamisetaHombre$precio)
mediaCamisetaHombre <- mean(CamisetaHombre$precio)
medianaCamisetaHombre <- median(CamisetaHombre$precio)
varCamisetaHombre<- var(CamisetaHombre$precio)
sdCamisetaHombre <- sd(CamisetaHombre$precio)
normCamisetaHombre <- dnorm(CamisetaHombre$precio, mean=mediaCamisetaHombre, sd = sdCamisetaHombre)
CamisetaHombre3sd <-CamisetaHombre[CamisetaHombre$precio > mediaCamisetaHombre-(3*sdCamisetaHombre) &CamisetaHombre$precio < mediaCamisetaHombre+(3*sdCamisetaHombre),]
nCamisetaHombre3sd <- length(CamisetaHombre3sd$precio)
mediaCamisetaHombre3sd <- mean(CamisetaHombre3sd$precio)
medianaCamisetaHombre3sd <- median(CamisetaHombre3sd$precio)
varCamisetaHombre3sd <- var(CamisetaHombre3sd$precio)
sdCamisetaHombre3sd <- sd(CamisetaHombre3sd$precio)
normCamisetaHombre3sd <- dnorm(CamisetaHombre3sd$precio, mean=mediaCamisetaHombre3sd , sd = sdCamisetaHombre3sd)
jpeg(paste0(PathSalida,"CamisetaHombreHist.jpg"))
plot(CamisetaHombre$precio)
dev.off()
jpeg(paste0(PathSalida,"CamisetaHombre3sdHist.jpg"))
plot(CamisetaHombre3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"CamisetaHombreNorm.jpg"))
plot(CamisetaHombre$precio, normCamisetaHombre)
dev.off()
jpeg(paste0(PathSalida,"CamisetaHombre3sdNorm.jpg"))
plot(CamisetaHombre3sd$precio, normCamisetaHombre3sd)
dev.off()

#capa intermerdia

CapaIntermedia <- p[grep("Ropa Montaña Hombre > Chaquetas > Forros Polares > ",p$categoria),]
nCapaIntermedia<-length(CapaIntermedia$precio)
mediaCapaIntermedia <- mean(CapaIntermedia$precio)
medianaCapaIntermedia <- median(CapaIntermedia$precio)
varCapaIntermedia<- var(CapaIntermedia$precio)
sdCapaIntermedia <- sd(CapaIntermedia$precio)
normCapaIntermedia <- dnorm(CapaIntermedia$precio, mean=mediaCapaIntermedia, sd = sdCapaIntermedia)
CapaIntermedia3sd <-CapaIntermedia[CapaIntermedia$precio > mediaCapaIntermedia-(3*sdCapaIntermedia) &CapaIntermedia$precio < mediaCapaIntermedia+(3*sdCapaIntermedia),]
nCapaIntermedia3sd <- length(CapaIntermedia3sd$precio)
mediaCapaIntermedia3sd <- mean(CapaIntermedia3sd$precio)
medianaCapaIntermedia3sd <- median(CapaIntermedia3sd$precio)
varCapaIntermedia3sd <- var(CapaIntermedia3sd$precio)
sdCapaIntermedia3sd <- sd(CapaIntermedia3sd$precio)
normCapaIntermedia3sd <- dnorm(CapaIntermedia3sd$precio, mean=mediaCapaIntermedia3sd , sd = sdCapaIntermedia3sd)
jpeg(paste0(PathSalida,"CapaIntermediaHist.jpg"))
plot(CapaIntermedia$precio)
dev.off()
jpeg(paste0(PathSalida,"CapaIntermedia3sdHist.jpg"))
plot(CapaIntermedia3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"CapaIntermediaNorm.jpg"))
plot(CapaIntermedia$precio, normCapaIntermedia)
dev.off()
jpeg(paste0(PathSalida,"CapaIntermedia3sdNorm.jpg"))
plot(CapaIntermedia3sd$precio, normCapaIntermedia3sd)
dev.off()

#Chaqueta exterior 

Chaqueta <- p[grep("Ropa Montaña Hombre > Chaquetas",p$categoria),]
nChaqueta<-length(Chaqueta$precio)
mediaChaqueta <- mean(Chaqueta$precio)
medianaChaqueta <- median(Chaqueta$precio)
varChaqueta<- var(Chaqueta$precio)
sdChaqueta <- sd(Chaqueta$precio)
normChaqueta <- dnorm(Chaqueta$precio, mean=mediaChaqueta, sd = sdChaqueta)
Chaqueta3sd <-Chaqueta[Chaqueta$precio > mediaChaqueta-(3*sdChaqueta) &Chaqueta$precio < mediaChaqueta+(3*sdChaqueta),]
nChaqueta3sd <- length(Chaqueta3sd$precio)
mediaChaqueta3sd <- mean(Chaqueta3sd$precio)
medianaChaqueta3sd <- median(Chaqueta3sd$precio)
varChaqueta3sd <- var(Chaqueta3sd$precio)
sdChaqueta3sd <- sd(Chaqueta3sd$precio)
normChaqueta3sd <- dnorm(Chaqueta3sd$precio, mean=mediaChaqueta3sd , sd = sdChaqueta3sd)
jpeg(paste0(PathSalida,"ChaquetaHist.jpg"))
plot(Chaqueta$precio)
dev.off()
jpeg(paste0(PathSalida,"Chaqueta3sdHist.jpg"))
plot(Chaqueta3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"ChaquetaNorm.jpg"))
plot(Chaqueta$precio, normChaqueta)
dev.off()
jpeg(paste0(PathSalida,"Chaqueta3sdNorm.jpg"))
plot(Chaqueta3sd$precio, normChaqueta3sd)
dev.off()

# Impermeable

Impermeable <- p[grep("Ropa Montaña Hombre > Chaquetas > Impermeables > Media Montaña",p$categoria),]
nImpermeable<-length(Impermeable$precio)
mediaImpermeable <- mean(Impermeable$precio)
medianaImpermeable <- median(Impermeable$precio)
varImpermeable<- var(Impermeable$precio)
sdImpermeable <- sd(Impermeable$precio)
normImpermeable <- dnorm(Impermeable$precio, mean=mediaImpermeable, sd = sdImpermeable)
Impermeable3sd <-Impermeable[Impermeable$precio > mediaImpermeable-(3*sdImpermeable) &Impermeable$precio < mediaImpermeable+(3*sdImpermeable),]
nImpermeable3sd <- length(Impermeable3sd$precio)
mediaImpermeable3sd <- mean(Impermeable3sd$precio)
medianaImpermeable3sd <- median(Impermeable3sd$precio)
varImpermeable3sd <- var(Impermeable3sd$precio)
sdImpermeable3sd <- sd(Impermeable3sd$precio)
normImpermeable3sd <- dnorm(Impermeable3sd$precio, mean=mediaImpermeable3sd , sd = sdImpermeable3sd)
jpeg(paste0(PathSalida,"ImpermeableHist.jpg"))
plot(Impermeable$precio)
dev.off()
jpeg(paste0(PathSalida,"Impermeable3sdHist.jpg"))
plot(Impermeable3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"ImpermeableNorm.jpg"))
plot(Impermeable$precio, normImpermeable)
dev.off()
jpeg(paste0(PathSalida,"Impermeable3sdNorm.jpg"))
plot(Impermeable3sd$precio, normImpermeable3sd)
dev.off()


#Gorros

Gorros <- p[grep("Ropa Montaña Hombre > Gorros y Tubulares",p$categoria),]
nGorros<-length(Gorros$precio)
mediaGorros <- mean(Gorros$precio)
medianaGorros <- median(Gorros$precio)
varGorros<- var(Gorros$precio)
sdGorros <- sd(Gorros$precio)
normGorros <- dnorm(Gorros$precio, mean=mediaGorros, sd = sdGorros)
Gorros3sd <-Gorros[Gorros$precio > mediaGorros-(3*sdGorros) &Gorros$precio < mediaGorros+(3*sdGorros),]
nGorros3sd <- length(Gorros3sd$precio)
mediaGorros3sd <- mean(Gorros3sd$precio)
medianaGorros3sd <- median(Gorros3sd$precio)
varGorros3sd <- var(Gorros3sd$precio)
sdGorros3sd <- sd(Gorros3sd$precio)
normGorros3sd <- dnorm(Gorros3sd$precio, mean=mediaGorros3sd , sd = sdGorros3sd)
jpeg(paste0(PathSalida,"GorrosHist.jpg"))
plot(Gorros$precio)
dev.off()
jpeg(paste0(PathSalida,"Gorros3sdHist.jpg"))
plot(Gorros3sd$precio)
dev.off()
jpeg(paste0(PathSalida,"GorrosNorm.jpg"))
plot(Gorros$precio, normGorros)
dev.off()
jpeg(paste0(PathSalida,"Gorros3sdNorm.jpg"))
plot(Gorros3sd$precio, normGorros3sd)
dev.off()


# Generamos el dataframe con los datos obtenidos


NombreFilas = c("CalzadoHombre","CalcetinesHombre","PantalonHombre","CamisetaHombre","CapaIntermedia","Chaqueta","Impermeable","Gorros")
NombreColumnas=c("N","Media","Mediana","Varianza","N (3sd)", "Media (3sd)","Mediana (3sd)","Varianza (3sd)")

n = c(nCalzadoHombre,nCalcetinesHombre,nPantalonHombre,nCamisetaHombre,nCapaIntermedia,nChaqueta,nImpermeable,nGorros)
n3sd = c(nCalzadoHombre3sd,nCalcetinesHombre3sd,nPantalonHombre3sd,nCamisetaHombre3sd,nCapaIntermedia3sd,nChaqueta3sd,nImpermeable3sd,nGorros3sd)
media = c(mediaCalzadoHombre,mediaCalcetinesHombre,mediaPantalonHombre,mediaCamisetaHombre,mediaCapaIntermedia,mediaChaqueta,mediaImpermeable,mediaGorros)
media3sd = c(mediaCalzadoHombre3sd,mediaCalcetinesHombre3sd,mediaPantalonHombre3sd,mediaCamisetaHombre3sd,mediaCapaIntermedia3sd,mediaChaqueta3sd,mediaImpermeable3sd,mediaGorros3sd)
mediana = c(medianaCalzadoHombre,medianaCalcetinesHombre,medianaPantalonHombre,medianaCamisetaHombre,medianaCapaIntermedia,medianaChaqueta,medianaImpermeable,medianaGorros)
mediana3sd = c(medianaCalzadoHombre3sd,medianaCalcetinesHombre3sd,medianaPantalonHombre3sd,medianaCamisetaHombre3sd,medianaCapaIntermedia3sd,medianaChaqueta3sd,medianaImpermeable3sd,medianaGorros3sd)
varianza = c(varCalzadoHombre,varCalcetinesHombre,varPantalonHombre,varCamisetaHombre,varCapaIntermedia,varChaqueta,varImpermeable,varGorros)
varianza3sd = c(varCalzadoHombre3sd,varCalcetinesHombre3sd,varPantalonHombre3sd,varCamisetaHombre3sd,varCapaIntermedia3sd,varChaqueta3sd,varImpermeable3sd,varGorros3sd)

estadisticas<-data.frame(n,media,mediana,varianza,n3sd,media3sd,mediana3sd,varianza3sd)
rownames(estadisticas)<-NombreFilas
colnames(estadisticas)<-NombreColumnas


#mostramos por pantalla

estadisticas
write.csv(estadisticas, file = paste0(PathSalida,"estadisticas.csv"))

# finalmente obtnemos los dos costes que buscábamos
# una vez eliminados nuestros "outliers" 
#(que en realidad no lo son pero hemos decidido eliminar todo lo que esté más allá de la media +/- 3sd)
# pero es una forma de simular una limpieza de datos sobre el dataset
# coste del equipamiento siguiendo según la mediana
# coste del equipamiento según la media de precios

TotalequipamientoMedia= sum(estadisticas[,"Media (3sd)",])
TotalequipamientoMediana= sum(estadisticas[,"Mediana (3sd)",])


Resultado <-data.frame(c(TotalequipamientoMedia),c(TotalequipamientoMediana))
rownames(Resultado) <-c("Total")
colnames(Resultado) <- c("Precio segun media","Precio según Mediana")

Resultado
write.csv(Resultado, file = paste0(PathSalida,"","resultado.csv"))





