#function to write JAGS models
writeJAGSmodels<-function(){

  #######################################################################################################
  #Build JAGS models

  message("Building JAGS model for linear models without Observer covariates and saving as .txt file.")
  #First linear model with no observer effects.
  modelstring="

  model{

  #Priors
  b0_mu ~ dnorm(0,0.01)
  b0_tau ~ dgamma(0.01,0.01)
  b1_mu ~ dnorm(-1,1)
  b1_tau ~ dgamma(0.01,0.01)
  sG <- pow(m,2)/pow(d,2)
  rG <- m/pow(d,2)
  m ~ dgamma(0.1,0.1)
  d ~ dgamma(1,0.1)


  for ( s in 1 : Npos.pins ) {
  b0[s] ~ dnorm( b0_mu , b0_tau)
  b1[s] ~ dnorm( b1_mu , b1_tau)
  tau[s] ~ dgamma( sG , rG )
  }

  for(i in 1:Npos){
  position_tau[i] ~ dgamma(sG,rG)
  }

  for(i in 1:Nstations){
  station_tau[i] ~ dgamma(sG,rG)
  }

  #mean slope at each pin among years
  for(r in 1:Ndata) {
  y[r] ~ dnorm( mu[r] , tau[pos.pins[r]] )
  mu[r] <- b0[pos.pins[r]] + b1[pos.pins[r]] * year[r] #linear model with no observer covariate
  }


  #Derived parameters
  for(j in 1:Npos){
  position_mu[j] ~ dnorm(b1[j], position_tau[pos[j]])
  }

  pinList<-c(1,2, 3, 4, 5, 6, 7, 8, 9)
  posList<-c(0, 9, 18, 27)
  for(t in 1:Npos){
  positionMean[t] = mean(b1[pinList+posList[t]])
  }

  for(j in 1:Nstations){
  station_mu[j] ~ dnorm(position_mu[j], station_tau[stations[j]])
  }

  #arithmetic mean for station
  stationMean<-mean(positionMean)


  #end model
  }"
writeLines(modelstring, con="modelNoObs.txt")

  #######################################################################################################
  message("Building JAGS model for linear models without Observer covariates and saving as .txt file.")
  #Model with observer effects.
  modelstring="

  model{

  #Priors
  b0_mu ~ dnorm(0,0.01)
  b0_tau ~ dgamma(0.01,0.01)
  b1_mu ~ dnorm(-1,1)
  b1_tau ~ dgamma(0.01,0.01)
  sG <- pow(m,2)/pow(d,2)
  rG <- m/pow(d,2)
  m ~ dgamma(0.1,0.1)
  d ~ dgamma(1,0.1)


  for ( s in 1 : Npos.pins ) {
  b0[s] ~ dnorm( b0_mu , b0_tau)
  b1[s] ~ dnorm( b1_mu , b1_tau)
  tau[s] ~ dgamma( sG , rG )
  }

  for(i in 1:Npos){
  position_tau[i] ~ dgamma(sG,rG)
  }

  for(i in 1:Nstations){
  station_tau[i] ~ dgamma(sG,rG)
  }

  #mean slope at each pin among years
  for(r in 1:Ndata) {
  y[r] ~ dnorm( mu[r] , tau[pos.pins[r]] )
  mu[r] <- b0[obs[pos.pins[r]]] + b1[pos.pins[r]] * year[r]  #linear model with observer covariate as random effect
  }


  #Derived parameters
  for(j in 1:Npos){
  position_mu[j] ~ dnorm(b1[j], position_tau[pos[j]])
  }

  pinList<-c(1,2, 3, 4, 5, 6, 7, 8, 9)
  posList<-c(0, 9, 18, 27)
  for(t in 1:Npos){
  positionMean[t] = mean(b1[pinList+posList[t]])
  }

  for(j in 1:Nstations){
  station_mu[j] ~ dnorm(position_mu[j], station_tau[stations[j]])
  }

  #arithmetic mean for station
  stationMean<-mean(positionMean)


  #end model
  }"
writeLines(modelstring, con="modelRandObs.txt")


}


