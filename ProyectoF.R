# EQUIPO 1

dir()
setwd()
#PostWorks: Programación y Estadística con R - BEDU

rm(list = ls())
library(ggplot2)
library(dplyr)
library(fbRanks)
library(DescTools)


##################### Introducción a R y Software #############################

# Base de Datos: Estadísticas de las 22 mejores Ligas Europeas

# 1: Importación de la Base de Datos (BD) y descripción de la misma:

SP1 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

View(SP1)
(names(SP1))
(summary(SP1))
(head(SP1))


# 2: Al importar nuestra BD se extraen las columnas que contienen los números de 
# goles anotados por los equipos que jugaron en casa (FTHG) y los goles 
# anotados por los equipos que jugaron como visitante (FTAG)

partidos <- SP1[ , c("FTHG", "FTAG")] 
View(partidos)

# 3: Usando tablas de frecuencias se calculan las siguientes probabilidades

### 3.1: La probabilidad (marginal) de que el equipo que juega en casa 
### anote x goles (x = 0, 1, 2, ...)

ld<-table(partidos$FTHG)
ldf<-data.frame(ld)
ldf <- mutate(ldf, pm = Freq/sum(Freq))
ldf <- rename(ldf,  FTHG=Var1)

# Las probabilidades Marginales serian
View(ldf)


### 3.2: La probabilidad (marginal) de que el equipo que juega como visitante 
### anote y goles (y = 0, 1, 2, ...)

vis<-table(partidos$FTAG)
visdf<-data.frame(vis)
visdf<-mutate(visdf, pm = Freq/sum(Freq))
visdf <- rename(visdf,  FTAG = Var1 )

# Las Probabilidades Marginales serian
View(visdf)


### 3.3: La probabilidad (conjunta) de que el equipo que juega en casa anote x 
### goles y el equipo que juega como visitante anote y goles 
### (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

frecuencia<- table(partidos)
df<-data.frame(frecuencia)
df <- mutate(df, probabilidad = Freq/sum(Freq))
df <- rename(df, Gloc = FTHG, Gvis = FTAG)

# Las Probabilidades Conjuntas serían
View(df)




################## Programación y Manipulación de Datos ######################

# 1: Importamos datos de soccer de las temporadas 2017/2018, 2018/2019 y 
# 2019/2020 de la primera división de la liga española

SP1
SP2 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
SP3 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")

# 2: Analizando las BD's

str(SP1); str(SP2);str(SP3)

head(SP1); head(SP2); head(SP3)

view(SP1);view(SP2);view(SP3)

summary(SP1);summary(SP2);summary(SP3)


# 3: Se seleccionan las columnas: Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR
# De las BD's

SP3<-select(SP3,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
SP2<-select(SP2,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
SP1<-select(SP1,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

View(SP1); View(SP2); View(SP3)

# 4: Cambio de formatos de las fechas:

SP1<-mutate(SP1, Date=as.Date(Date,"%d/%m/%Y"))
SP2<-mutate(SP2, Date=as.Date(Date,"%d/%m/%Y"))
SP3<- mutate(SP3, Date=as.Date(Date,"%d/%m/%y")) 


# Unión de los Data Frames

SP4<-rbind(SP3,SP2,SP1)

View(SP4)
summary(SP4)
str(SP4)




############### Análisis Exploratorio de Datos (AED o EDA) ####################


# 1: Haciendo uso del Data Frame "SP4", obtenemos las siguientes probabilidades

### 1.1: La probabilidad (marginal) de que el equipo que juega en casa anote x 
### goles (x = 0, 1, 2)

ld<-table(SP4$FTHG)
ldf<-data.frame(ld)
ldf <- mutate(ldf, pm = Freq/sum(Freq))
ldf <- rename(ldf,  FTHG=Var1)

View(ldf)

# Las Probabilidades Marginales x (para x= 0,1,2) serían:

ProbL <- as.list(ldf[c(1:3),3])

(ProbL <- list(Gol1 = Prob[[1]] , Goles2 = Prob[[2]], Goles3 = Prob[[3]]))



### 1.2: La probabilidad (marginal) de que el equipo que juega como visitante 
### anote y goles (y=0,1,2)

vis<-table(SP4$FTAG)
visdf<-data.frame(vis)
visdf<-mutate(visdf, pm = Freq/sum(Freq))
visdf <- rename(visdf,  FTAG = Var1 )

View(visdf)

# Las Probabilidades Marginales y (para y= 0,1,2) serían:

ProbV <- as.list(visdf[c(1:3),3])

(ProbV <- list(Gol1 = Prob[[1]] , Goles2 = Prob[[2]], Goles3 = Prob[[3]]))


### 1.3: La probabilidad (conjunta) de que el equipo que juega en casa anote x 
### goles y el equipo que juega como visitante anote y goles (x=0,1,2 ; y=0,1,2)

frecuencia<- table(SP4$FTHG, SP4$FTAG)
df<-data.frame(frecuencia)
df <- mutate(df, probabilidad = Freq/sum(Freq))
df <- rename(df, Gloc = Var1, Gvis = Var2)

View(df$probabilidad)

# Las Probabilidades Conjuntas x,y (para x=0,1,2 ; y= 0,1,2) serían:

prop.table(frecuencia) [c(1:3),c(1:3)]


# 2: A continuacion, se presentan estas probabilidades en Graficos:

### 2.1: Probabilidades marginales del número de goles que anota el equipo de casa.

ldf %>%
     ggplot() +
     aes(x = FTHG, y = pm, colours= "Gray") +
     geom_bar(stat = 'identity') +
     ggtitle("FTHG probability") +
     xlab("Goles") +
     ylab("Probabilidad")+
     theme(plot.title = element_text(size=20, hjust = 0.5))  +
     theme(axis.text.x = element_text(size = 10,hjust = 1),
           axis.text.y = element_text(size = 10, hjust = 1),
           axis.title = element_text(face = "bold" , size = 13, hjust = 0.5))


### 2.2: Probabilidades marginales del número de goles que anota el equipo visitante.

visdf %>%
     ggplot() +
     aes(x=FTAG,y=pm, colours= "Gray") +
     geom_bar(stat = 'identity') +
     ggtitle("FTAG probability") +
     xlab("Goles") +
     ylab("Probabilidad")+
     theme(plot.title = element_text(size=20, hjust = 0.5))  +
     theme(axis.text.x = element_text(size = 10,hjust = 1),
           axis.text.y = element_text(size = 10, hjust = 1),
           axis.title = element_text(face = "bold" , size = 13, hjust = 0.5))

### 2.3: Probabilidades conjuntas de los números de goles que anotan el equipo 
### de casa y el equipo visitante en un partido.

df%>%
     ggplot()+
     aes(x = Gloc, y = Gvis, fill = probabilidad)+
     geom_tile()+
     ggtitle("HeatMap Goles")+
     ylab("Goles Visitante")+
     xlab("Goles Local") +
     labs(fill="Probabilidad")+
     theme(plot.title = element_text(size=20, hjust = 0.5))  +
     theme(axis.text.x = element_text(size = 10,hjust = 1),
           axis.text.y = element_text(size = 10, hjust = 1),
           axis.title = element_text(face = "bold" , size = 13, hjust = 0.5),
           legend.title = element_text(face = "bold"))





##### Distribuciones, Teorema Central del Limite y Contraste de Hipotesis #####


# 1: Obtencion de una tabla de cocientes al dividir las probabilidades conjuntas 
# por el producto de las probabilidades marginales correspondientes.

cocientes <- (merge(df, visdf, by.x = "Gvis", by.y = "FTAG", all = TRUE))
cocientes <- (select(cocientes, Gvis, Gloc, Freq.x, probabilidad,pm))
cocientes <- rename(cocientes,conjunta =  probabilidad ,  margvis= pm)
cocientes <- merge(cocientes, ldf, by.x = "Gloc", by.y = "FTHG")
cocientes <- select (cocientes, Gloc, Gvis, Freq.x, conjunta, margvis, pm)
cocientes <- rename(cocientes, margloc = pm)
cocientes <- (mutate(cocientes, cociente = conjunta/(margvis*margloc)))

# Entonces se tiene como resultado:

View(cocientes$cociente)

# Cuya media y derviacion estandar resultan ser:

mean(cocientes$cociente)
sd(cocientes$cociente)


# 2: Calculando mediante un procedimiento de boostrap, se obtienen más cocientes 
# similares a los obtenidos en la tabla del punto anterior.

nboot <- 1000
cociente.boot <- numeric(nboot)
for (i in 1:nboot) {
     dat.boot <- sample(cocientes$cociente, replace=TRUE)
     cociente.boot[i] <- mean(dat.boot)
}

# Realizando un histograma, resulta:

as.data.frame(cociente.boot) %>%
     ggplot() +
     aes(cociente.boot) +
     geom_histogram(binwidth = .05) +
     ggtitle("Histograma de Cocientes (Boostrap)")+
     ylab("Frecuencia")+
     xlab("Cociente") +
     theme(plot.title = element_text(size=20, hjust = 0.5))  +
     theme(axis.text.x = element_text(size = 10,hjust = 1),
           axis.text.y = element_text(size = 10, hjust = 1),
           axis.title = element_text(face = "bold" , size = 13, hjust = 0.5)) 

# Tenemos que la media resulta ser:

mean(cociente.boot)

pnorm(1.01, mean = mean(cociente.boot),sd = sd(cociente.boot)) -
     pnorm(0.99, mean = mean(cociente.boot),sd = sd(cociente.boot))

# Entonces,  la probabilidad de que los eventos resulten independientes, 
# (que el cociente sea igual a uno) es solo de 3.46% por lo que podemos concluir
# que en general que un equipo anote "x" cantidad de goles depende de los goles 
# anotados por el equipo contrario.


#################   Regresi�n Lineal y Clasificaci�n   ######################

SP3<-select(SP3,Date, HomeTeam, FTHG, AwayTeam, FTAG)
SP2<-select(SP2,Date, HomeTeam,  FTHG, AwayTeam, FTAG)
SP1<-select(SP1,Date, HomeTeam,  FTHG, AwayTeam, FTAG)

View(SP1); View(SP2); View(SP3)

# 4: Cambio de formatos de las fechas:

SP1<-mutate(SP1, Date=as.Date(Date,"%d/%m/%Y"))
SP2<-mutate(SP2, Date=as.Date(Date,"%d/%m/%Y"))
SP3<- mutate(SP3, Date=as.Date(Date,"%d/%m/%y")) 

# Unión de los Data Frames

SmallData<-rbind(SP3,SP2,SP1)


SmallData <- rename(SmallData, date = Date, home.team = HomeTeam, home.score = FTHG, 
                    away.team = AwayTeam, away.score = FTAG)

write.csv(SmallData, "soccer.csv", row.names = F)

#install.packages("fbRanks")

listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")
str(listasoccer)

anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

str(anotaciones)
str(equipos)

rank.teams(scores = anotaciones,teams=equipos)

?unique
fecha <- unique(anotaciones$date)
str(fecha)
fecha - sort(fecha)

n <- length(fecha)
?rank.teams
ranking <- rank.teams(scores = anotaciones,teams=equipos,max.date = fecha[n-1], 
           min.date = fecha[1])


Predicciones <- predict(ranking, date=fecha[n])


########################  Series de Tiempo  ####################################
#Postwork 6

#Descargar data
soccer.data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

#Modificando formato fecha y agregando suma de goles
soccer.data <- mutate(soccer.data, date = as.Date(date, "%Y-%m-%d"),
                      sumagoles = home.score + away.score, juegos = 1)

#Agrupando por a�o y mes y formato de columnas
monthAvgGoals <- soccer.data %>% group_by(Year(date), Month(date, fmt = "mm"))%>%
        summarise(mean = mean(sumagoles))
names(monthAvgGoals) <- c("year","month", "avrgGoals")

#Acotando datos de 2010 a 2019
ts.dataset <- as.data.frame(monthAvgGoals[- c(97:101),])

#Creando ts y graficando
avgSumaGoles.ts <- ts(ts.dataset$avrgGoals, start = 2010, frequency = 10)

plot(avgSumaGoles.ts, xaxt = "n", main= "Promedio de suma de goles mensual",
     ylab="Promedio", xlab="Temporadas")
axis(1, at = seq(from = 2010, to = 2019, by=1) , 
     labels = paste("Aug-", seq(from = 2010, to = 2019, by=1), sep = ""))
abline(v=seq(from = 2010, to = 2019, by=1), col="black", lty=2, lwd=1)
abline(h = mean(ts.dataset$avrgGoals), col = "red", lwd=2)


####################  Conexiones con BD's y Datos Externos  ####################

#Conectandose a MongoDB
#Recuerda configurar tu cluster segun tu IP
dmd <- mongo(
        collection = "match",
        db = "match_games",
        url = "mongodb+srv://nachorz:contrase�a@cluster0.wmpq4.mongodb.net/test",
)
#Creando Query de los juegos de local el 20 de Dic del 2015 del real madrid
localdata <- dmd$find(
        query = '{"home.team" : "Real Madrid", "date":"2015-12-20"}',
        limit = 1)
#Creando Query de los juegos de visita el 20 de Dic del 2015 del real madrid
visitdata <-  dmd$find(
        query = '{"away.team" : "Real Madrid", "date":"2015-12-20"}',
        limit = 1)
print(visitdata)
print(localdata)