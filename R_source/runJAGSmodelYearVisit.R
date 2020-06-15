#function to run JAGS model
runJAGSmodelYearVisit<-function(dataIn, modelIn){

  new.data<-dataIn

  stationName<-as.character(unique(new.data$Plot_Name))

  posList<-sort(unique(as.character(new.data$Position_Name)))

  #number of year.visits within of data
  Ndata<-nrow(new.data)

  #number of Stations
  stations<-as.integer(factor(new.data[,"Plot_Name"], levels=unique(new.data[,"Plot_Name"])))
  Nstations<-length(unique(stations))

  #number of Years
  Nyears<-length(unique(new.data$Year))
  year=as.numeric(as.factor(new.data[,"Year"]))

  #number of within-year sites (visits)
  visit<-as.integer(factor(new.data[,"Visit"], levels=unique(new.data[,"Visit"])))
  Nvisits<-length(unique(visit))

  #number of positions (n=4)
  pos<-as.integer(factor(new.data[,"Position_Name"], levels=unique(new.data[,"Position_Name"])))
  Npos<-length(unique(pos))

  #number of pins per position (n=9)
  pins<-as.integer(factor(new.data[,"variable"], levels=unique(new.data[,"variable"])))
  Npins<-length(unique(pins))

  #number of pos.pin (n=36)
  pos.pins<-as.integer(factor(new.data[,"pos.pin"], levels=unique(new.data[,"pos.pin"])))
  Npos.pins<-length(unique(pos.pins))

  #observer
  obs<-as.integer( factor( new.data[,"Last_Name"] ,levels=unique(new.data[,"Last_Name"])))
  head(new.data)

  #try with year.visit
  #year=as.integer(as.factor(new.data[,"year.visit"]))
  y=as.numeric(new.data[,"value"])

  ###############################################################################################################################
  # Standardize data (divide by SD) to make initialization easier.


  # Specify data, as a list.
  dataList = list(
    Ndata = Ndata,
    Nstations = Nstations,
    stations = stations,
    visit = visit,
    Nvisits=Nvisits,
    Npos = Npos,
    pos = pos,
    Npins = Npins,
    pins = pins,
    Npos.pins=Npos.pins,
    pos.pins=pos.pins,
    obs=obs,
    Nyears=Nyears,
    year = year ,
    y = y
  )



  # b0 = b1 = tau = rep(0, length=Npos.pins)
  # for (j in 1:Npos.pins ) {
  #   yVec = dataList$y[dataList$pos.pins==j]
  #   year = dataList$year[dataList$pos.pins==j]
  #   lmInfo = lm( yVec ~ year)
  #   b0[j] <-lmInfo$coef[1]
  #   b1[j] <- lmInfo$coef[2]
  #   tau[j] <- length(yVec) / sum(lmInfo$res^2)
  # }
  #
  #
  # b0_mu = mean(b0)
  # b0_tau = 1/sd(b0)^2
  # b1_mu = mean(b1)
  # b1_tau = 1/sd(b1)^2
  # m = mean(tau)
  # d = sd(tau)
  #
  #
  # initsList = list( b0=b0 , b1=b1 , tau=tau ,
  #                   b0_mu=b0_mu , b0_tau=b0_tau ,
  #                   b1_mu=b1_mu , b1_tau=b1_tau,
  #                   m=m , d=d)

  ###############################

  params = c("station_mu","station_tau","position_mu","position_tau","b0","b1" , "b0_mu","b0_tau", "b1_mu","b1_tau","positionMean","stationMean")
  adaptSteps = 1000              # Number of steps to "tune" the samplers.
  burnInSteps = 10000            # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps=50000           # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  # Create, initialize, and adapt the model:
  jagsModel=NULL

  #get sum of y
  sum.y<-sum(dataList$y)

ifelse(sum.y==0,
       jagsModel<-NULL,
       jagsModel<-jags.model( modelIn , data=dataList , inits=NULL, n.chains=nChains , n.adapt=adaptSteps)
       )

  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update.jags( jagsModel , n.iter=burnInSteps )

  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  out = coda.samples( jagsModel , variable.names=params ,
                      n.iter=nPerChain , thin=thinSteps)
  #gelman.diag(out)

  #plot(out)
  return(out)
}
