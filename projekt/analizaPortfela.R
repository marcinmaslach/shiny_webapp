Portfel <- function(S, Tk, fi, sigma1, sigma2, r1, r2, reh, mi1, mi2, S0, X){ #reh to liczba rehedgingów, sigma1 jest do trajektorii! r1 to polski, S i S0 to to samo
  portfel <- matrix(0,reh+2,4)
  akcja <- VA(S, Tk, 0, fi, sigma1, sigma2, r1, r2)*100/S0
  delta <- deltaA(Tk, 0, fi, sigma1, sigma2, r1, r2)
  delta_z <- delta*1/X*100/S0
  delta_dol <- delta*(-1)*S/X*100/S0
  gotówka <- akcja-S*X*delta_z-X*delta_dol
  dt <- Tk/(reh+1)
  portfel[1,1] <- delta_z*X*S+delta_dol*X #suma
  portfel[1,2] <- gotówka
  portfel[1,3] <- delta_z
  portfel[1,4] <- delta_dol
  for(i in 1:(reh)){
    S <- symulacja(dt, S, mi1, sigma1, rnorm(1))
    X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
    new.delta <- deltaA(Tk, i*dt, fi, sigma1, sigma2, r1, r2)
    new.delta_z <- new.delta*1/X*100/S0
    new.delta_dol <- new.delta*(-1)*S/X*100/S0
    gotówka <- gotówka*exp(r1*dt) - (new.delta_z-delta_z)*S*X - (new.delta_dol-delta_dol*exp(r2*dt))*X
    portfel[i+1,1] <-  (new.delta_z-delta_z)*S*X + (new.delta_dol-delta_dol*exp(r2*dt))*X #ilosc kasy na opcje w tym kroku
    portfel[i+1,2] <- gotówka
    delta <- new.delta
    delta_z <- new.delta_z
    delta_dol <- new.delta_dol
    portfel[i+1,3] <- delta_z
    portfel[i+1,4] <- delta_dol
  }
  S <- symulacja(dt, S, mi1, sigma1, rnorm(1))
  X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
  gotówka <- gotówka*exp(r1*dt)
  payoff <- 100*S/S0
  
  strata <- delta_z*S*X + delta_dol*X*exp(r2*dt) + gotówka - payoff
  portfel[reh+2,1] <- delta_z*S*X + delta_dol*X*exp(r2*dt) #suma
  portfel[reh+2,2] <- gotówka
  portfel[reh+2,3] <- delta_z
  portfel[reh+2,4] <- delta_dol
  return(portfel)
}
Analiza <- Portfel(gold[length(gold)], 253/253, fiA, sigma_gold, sigma_USDPLN, r_PL, r_USA, 126, mi_gold, mi_USDPLN, gold[length(gold)], USDPLN[length(USDPLN)])
matplot(Analiza[,4], type = "l")
