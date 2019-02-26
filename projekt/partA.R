#HEdging#
symulacja <- function(t, S, m, si, rnorm) {
  return (S*exp((m -1/2*si^2)*t + si*rnorm*sqrt(t)))
}

VA <- function(S, Tk, t, fi, sigma1, sigma2, r1, r2){
  #pnorm(ING)=1 bo log(S/E)
  D <- r1-r2+fi*sigma1*sigma2
  V <- S*exp((-1)*D*(Tk-t))
  return(V)
}

deltaA <- function(Tk, t, fi, sigma1, sigma2, r1, r2){
  D <- r1-r2+fi*sigma1*sigma2
  delta <- exp((-1)*D*(Tk-t))
  return(delta)
}
#reh to liczba rehedgingów, 
#sigma1 jest do trajektorii złota
#r1 to polski, 
#S i S0 to to samo i jest to cena złota
#X to USDPLN
#deltaA to tylko część delty ale musi być przemnożona przez stałą
Pi <- function(S, Tk, fi, sigma1, sigma2, r1, r2, reh, mi1, mi2, S0, X){ #reh to liczba rehedgingów, sigma1 jest do trajektorii! r1 to polski, S i S0 to to samo
  akcja <- VA(S, Tk, 0, fi, sigma1, sigma2, r1, r2)*100/S0
  delta <- deltaA(Tk, 0, fi, sigma1, sigma2, r1, r2)
  delta_z <- delta*1/X*100/S0
  delta_dol <- delta*(-1)*S/X*100/S0
  gotówka <- akcja-S*X*delta_z-X*delta_dol
  dt <- Tk/(reh+1)
  for(i in 1:(reh)){
    S <- symulacja(dt, S, mi1, sigma1, rnorm(1))
    X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
    new.delta <- deltaA(Tk, i*dt, fi, sigma1, sigma2, r1, r2)
    new.delta_z <- new.delta*1/X*100/S0
    new.delta_dol <- new.delta*(-1)*S/X*100/S0
    gotówka <- gotówka*exp(r1*dt) - (new.delta_z-delta_z)*S*X - (new.delta_dol-delta_dol*exp(r2*dt))*X
    delta <- new.delta
    delta_z <- new.delta_z
    delta_dol <- new.delta_dol
  }
  S <- symulacja(dt, S, mi1, sigma1, rnorm(1))
  X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
  gotówka <- gotówka*exp(r1*dt)
  payoff <- 100*S/S0
  
  strata <- delta_z*S*X + delta_dol*X*exp(r2*dt) + gotówka - payoff
}

PiA100 <- c()
for (i in 1:1000) PiA100[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 100, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

PiA10 <- c()
for (i in 1:1000) PiA10[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 10, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

PiA252 <- c()
for (i in 1:1000) PiA252[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 252, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

PiA1 <- c()
for (i in 1:1000) PiA1[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 1, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

PiA50 <- c()
for (i in 1:1000) PiA50[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 50, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

PiA126 <- c()
for (i in 1:1000) PiA126[i]<-Pi(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 126, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])

par(mfrow=c(2,2))
hist(PiA10)
hist(PiA100)
hist(PiA252)
hist(PiA1)

#histogramy ggplot2
library(ggplot2)


dane<-data.frame(PiA10, PiA100, PiA252, PiA1)

library(dplyr)
data_frame(v1 = PiA1) %>%
  ggplot(., aes(v1)) + 
  geom_histogram(color='darkblue',fill='lightblue', binwidth=0.5)+
  labs(x='zysk/strata', y='liczebno??', title = 'Histogram zysk?w i strat dla 1 rehedgingu')+
  geom_vline(aes(xintercept = mean(v1, na.rm = TRUE), color='mean'), show.legend=TRUE,size=1)+
  geom_vline(aes(xintercept = median(v1, na.rm = TRUE), color='median'), show.legend=TRUE, size=1)+
  scale_color_manual(name='statystyki', values = c(mean='red', median='black'))+xlim(c(-10, 10))+
  ylim(c(0,450))

data_frame(v2 = PiA10) %>%
  ggplot(., aes(v2)) + 
  geom_histogram(color='darkblue',fill='lightblue', binwidth=0.5)+
  labs(x='zysk/strata', y='liczebno??', title = 'Histogram zysk?w i strat dla 10 rehedging?w')+
  geom_vline(aes(xintercept = mean(v2, na.rm = TRUE), color='mean'), show.legend=TRUE,size=1)+
  geom_vline(aes(xintercept = median(v2, na.rm = TRUE), color='median'), show.legend=TRUE, size=1)+
  scale_color_manual(name='statystyki', values = c(mean='red', median='black'))+xlim(c(-10, 10))+
  ylim(c(0,450))

data_frame(v3 = PiA100) %>%
  ggplot(., aes(v3)) + 
  geom_histogram(color='darkblue',fill='lightblue', binwidth=0.5)+
  labs(x='zysk/strata', y='liczebno??', title = 'Histogram zysk?w i strat dla 100 rehedging?w')+
  geom_vline(aes(xintercept = mean(v3, na.rm = TRUE), color='mean'), show.legend=TRUE,size=1)+
  geom_vline(aes(xintercept = median(v3, na.rm = TRUE), color='median'), show.legend=TRUE, size=1)+
  scale_color_manual(name='statystyki', values = c(mean='red', median='black'))+xlim(c(-10, 10))+
  ylim(c(0,450))

data_frame(v4 = PiA10) %>%
  ggplot(., aes(v4)) + 
  geom_histogram(color='darkblue',fill='lightblue', binwidth=0.5)+
  labs(x='zysk/strata', y='liczebno??', title = 'Histogram zysk?w i strat dla 252 rehedging?w')+
  geom_vline(aes(xintercept = mean(v4, na.rm = TRUE), color='mean'), show.legend=TRUE,size=1)+
  geom_vline(aes(xintercept = median(v4, na.rm = TRUE), color='median'), show.legend=TRUE, size=1)+
  scale_color_manual(name='statystyki', values = c(mean='red', median='black'))+xlim(c(-10, 10))+
  ylim(c(0,450))

#KWANTYLE ZYSKÓW

a<-as.factor(PiA10)
quantoA <- matrix(0,6,1000)
quantoA[1,] <- PiA1
quantoA[2,] <- PiA10
quantoA[3,] <- PiA50
quantoA[4,] <- PiA100
quantoA[5,] <- PiA126
quantoA[6,] <- PiA252

kwantyleA <- matrix(0, 6, 5)
for (i in 1:6){
  kwantyleA[i,1]<-quantile(quantoA[i,], probs = 0.1)
  kwantyleA[i,2]<-quantile(quantoA[i,], probs = 0.25)
  kwantyleA[i,3]<-quantile(quantoA[i,], probs = 0.5)
  kwantyleA[i,4]<-quantile(quantoA[i,], probs = 0.75)
  kwantyleA[i,5]<-quantile(quantoA[i,], probs = 0.9)
}
par(mfrow=c(1,1))
matplot(kwantyleA, type = "l", lwd = 2, lty = 1)
#kwantyle ggplot2
library(reshape2)

data1 <- as.data.frame(kwantyleA)
data1$id <- 1:nrow(data1)
plot_data1 <- melt(kwantyleA,Var1.var="id")

ggplot(plot_data1, aes(x=Var1,y=value,group=Var2,colour=Var2)) +
  geom_point()+
  geom_line()+
  labs(y='kwantyle', title = 'Wykres kwantyli')+xlim(c(1, 6))+
  scale_x_continuous(breaks = 1:6)
