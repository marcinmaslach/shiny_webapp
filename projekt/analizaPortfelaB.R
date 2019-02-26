PortfelB <- function(Y, Tk, fi, sigma1, sigma2, r1, r2, reh, mi1, mi2, X){ #reh to liczba rehedgingów, sigma1 jest do trajektorii! r1 to polski, S i S0 to to samo
  portfel <- matrix(0,reh+2,4)
  S0 <- Y/X
  akcja <- VB(Y, X, Tk, 0, fi, sigma1, sigma2, r1, r2)*100/S0
  delta <- deltaB(Tk, 0, fi, sigma1, sigma2, r1, r2)
  delta_z <- 1/X*delta*100/S0
  delta_dol <- (-1)*Y/(X^2)*delta*100/S0
  gotówka <- akcja-Y*delta_z-X*delta_dol
  dt <- Tk/(reh+1)
  portfel[1,1] <- delta_z*Y+delta_dol*X #suma
  portfel[1,2] <- gotówka
  portfel[1,3] <- delta_z
  portfel[1,4] <- delta_dol
  for(i in 1:(reh)){
    Y <- symulacja(dt, Y, mi1, sigma1, rnorm(1))
    X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
    new.delta <- deltaB(Tk, i*dt, fi, sigma1, sigma2, r1, r2)
    new.delta_z <- new.delta*1/X*100/S0
    new.delta_dol <- new.delta*(-1)*Y/(X^2)*100/S0
    gotówka <- gotówka*exp(r1*dt) - (new.delta_z-delta_z)*Y - (new.delta_dol-delta_dol*exp(r2*dt))*X
    portfel[i+1,1] <- (new.delta_z-delta_z)*Y + (new.delta_dol-delta_dol*exp(r2*dt))*X #ilosc kasy na opcje w tym kroku
    portfel[i+1,2] <- gotówka
    delta <- new.delta
    delta_z <- new.delta_z
    delta_dol <- new.delta_dol
    portfel[i+1,3] <- delta_z
    portfel[i+1,4] <- delta_dol
  }
  Y <- symulacja(dt, Y, mi1, sigma1, rnorm(1))
  X <- symulacja(dt, X, mi2, sigma2, rnorm(1))
  gotówka <- gotówka*exp(r1*dt)
  payoff <- 100*(Y/X)/S0
  
  strata <- delta_z*Y + delta_dol*X*exp(r2*dt) + gotówka - payoff
  portfel[reh+2,1] <- delta_z*Y + delta_dol*X*exp(r2*dt) #suma
  portfel[reh+2,2] <- gotówka
  portfel[reh+2,3] <- delta_z
  portfel[reh+2,4] <- delta_dol
  return(portfel)
}
AnalizaB <- PortfelB(gold_PLN[length(gold_PLN)], 253/253, fiB, sigma_gold_PLN, sigma_USDPLN, r_PL, r_USA, 126, mi_gold_PLN, mi_USDPLN, USDPLN[length(USDPLN)])
matplot(AnalizaB[,3], type = "l")
