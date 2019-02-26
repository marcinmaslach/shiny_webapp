#USDPLN

setwd("C:\\Users\\iga\\Desktop\\projekt")
dolar <- read.csv("dolar.csv", fill = TRUE)
dolar1 <- dolar[,3]
dolar1 <- c(dolar1[1:125], dolar1[127:length(dolar1)])
USDPLN <- dolar1[length(dolar1):1] #Popieram wektor który mnie interesuje i odwracam go
R_USDPLN <- c((2.7877-2.7517)/2.7877) #Liczę zwroty
for (i in 2:length(USDPLN)) R_USDPLN[i] <- (USDPLN[i]-USDPLN[i-1])/USDPLN[i-1]
mean_USDPLN <- mean(R_USDPLN)  
sd_USDPLN <- sd(R_USDPLN)

dt <- 1/252
mi_USDPLN <- mean_USDPLN/dt
sigma_USDPLN <- sd_USDPLN/sqrt(dt)

#GOLD USD

setwd("C:\\Users\\iga\\Desktop\\projekt")
z <- read.csv("z�oto.csv", fill = TRUE)
gold <- z[,5] #Popieram wektor który mnie interesuje
gold <- c(gold[1:34],gold[35:90],gold[92:98],gold[100:136],gold[138:217],gold[221:244],gold[246:length(gold)])
R_gold <- c() #Liczę zwroty
for (i in 2:length(gold)) R_gold[i] <- (gold[i]-gold[i-1])/gold[i-1]
R_gold <- R_gold[2:length(R_gold)] #Wziąłem od 29czerwca2011 żeby mieć wszystkie zwroty a teraz usuwam ten dzień
mean_gold <- mean(R_gold)  
sd_gold <- sd(R_gold)

dt <- 1/253
mi_gold <- mean_gold/dt
sigma_gold <- sd_gold/sqrt(dt)

#GOLD PLN
USDPLN <- c(2.7877,USDPLN)
gold_PLN <- USDPLN*gold
R_gold_PLN <- c()
for (i in 2:length(gold_PLN)) R_gold_PLN[i] <- (gold_PLN[i]-gold_PLN[i-1])/gold_PLN[i-1]
R_gold_PLN <- R_gold_PLN[2:length(R_gold_PLN)] 
mean_gold_PLN <- mean(R_gold_PLN)  
sd_gold_PLN <- sd(R_gold_PLN)

mi_gold_PLN <- mean_gold_PLN/dt
sigma_gold_PLN <- sd_gold_PLN/sqrt(dt)

#KORELACJA

fiA<-cor(R_gold, R_USDPLN)
EpsilonA <- matrix(c(1, fiA, fiA, 1),2,2)
DetEA <- 1 - (fiA)^2
fiB<-cor(R_USDPLN, R_gold_PLN)

r_USA <- 2.071/100
r_PL <- 5.645/100