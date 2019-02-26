symulacja <- function(t, S, m, si, rnorm) {
  return (S*exp((m -1/2*si^2)*t + si*rnorm*sqrt(t)))
}

VB <- function(Y, X, Tk, t, fi, sigma1, sigma2, r1, r2){
  #pnorm(ING)=1 bo log(S/E)
  D <- (-1)*(sigma2^2-fi*sigma1*sigma2+r2)+r1
  V <- Y/X*exp((-1)*D*(Tk-t))
  return(V)
}

deltaB <- function(Tk, t, fi, sigma1, sigma2, r1, r2){
  D <- (-1)*(sigma2^2-fi*sigma1*sigma2+r2)+r1
  delta <- exp((-1)*D*(Tk-t))
  return(delta)
}
#reh to liczba rehedgingów, 
#sigma1 jest do trajektorii złota
#r1 to polski, 
#S i S0 to to samo i jest to cena złota
#X to USDPLN
#deltaA to tylko część delty ale musi być przemnożona przez stałą
Pi2 <- function(Y, Tk, fi, sigma1, sigma2, r1, r2, reh, mi1, mi2, X){ #reh to liczba rehedgingów, sigma1 jest do trajektorii! r1 to polski, S i S0 to to samo
  S0 <- Y/X
  D <- r1 - r2 + fi*sigma1*sigma2-sigma2^2
  akcja <- exp(-D)*100
  #delta <- deltaB(Tk, 0, fi, sigma1, sigma2, r1, r2)
  delta_z <- 1/X*exp(-D)*100/S0
  delta_dol <- (-1)*Y/(X^2)*exp(-D)*100/S0
  gotówka <- akcja-Y*delta_z-X*delta_dol
  dt <- Tk/(reh+1)
  for(i in 1:(reh)){
    Y <- symulacja(dt, Y, mi1, sigma1, rnorm(1))
    X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
    #new.delta <- deltaB(Tk, i*dt, fi, sigma1, sigma2, r1, r2)
    new.delta_z <- exp(-D*dt)*1/X*100/S0
    new.delta_dol <- exp(-D*dt)*(-1)*Y/(X^2)*100/S0
    gotówka <- gotówka*exp(r1*dt) - (new.delta_z-delta_z)*Y - (new.delta_dol-delta_dol*exp(r2*dt))*X
    #delta <- new.delta
    delta_z <- new.delta_z
    delta_dol <- new.delta_dol
  }
  Y <- symulacja(dt, Y, mi1, sigma1, rnorm(1))
  X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
  gotówka <- gotówka*exp(r1*dt)
  payoff <- 100*(Y/X)/S0
  
  strata <- delta_z*Y + delta_dol*X*exp(r2*dt) + gotówka - payoff
}


PiB100 <- c()
for (i in 1:1000) PiB100[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 100, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
PiB10 <- c()
for (i in 1:1000) PiB10[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 10, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
PiB50 <- c()
for (i in 1:1000) PiB50[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 50, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
PiB1 <- c()
for (i in 1:1000) PiB1[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 1, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
PiB126 <- c()
for (i in 1:1000) PiB126[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 126, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
PiB252 <- c()
for (i in 1:1000) PiB252[i]<-Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 252, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5

par(mfrow=c(2,2))
hist(PiB1, xlim=c(-10,10))
hist(PiB10,  xlim=c(-10,10))
hist(PiB100,  xlim=c(-10,10))
hist(PiB252,  xlim=c(-10,10))
mean(PiB100)

a<-as.factor(PiB10)
quantoB <- matrix(0,6,1000)
for (i in 1:1000){
  quantoB[1,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 1, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
  quantoB[2,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 10, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
  quantoB[3,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 50, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
  quantoB[4,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 100, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
  quantoB[5,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 126, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
  quantoB[6,i] <- Pi2(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 252, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])+2.5
}

kwantyleB <- matrix(0, 6, 5)
for (i in 1:6){
  kwantyleB[i,1]<-quantile(quantoB[i,], probs = 0.1)
  kwantyleB[i,2]<-quantile(quantoB[i,], probs = 0.25)
  kwantyleB[i,3]<-quantile(quantoB[i,], probs = 0.5)
  kwantyleB[i,4]<-quantile(quantoB[i,], probs = 0.75)
  kwantyleB[i,5]<-quantile(quantoB[i,], probs = 0.9)
}
par(mfrow=c(1,1))

matplot(kwantyleB, type = "l", lwd = 2, lty = 1, las = 1, xaxt = "n")
axis(1,at = c(1,2,3,4,5,6), labels =c(1,10,50,100,126,252), las = 1) 
