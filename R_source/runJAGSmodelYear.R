#function to run JAGS model
runJAGSmodelYear<-function(dataIn, modelIn){

  sub.data.2<-dataIn

  stationName<-as.character(unique(sub.data.2$Plot_Name))

  posList<-sort(unique(as.character(sub.data.2$PipePosition)))

  # for(k in 1:length(posList)){
  #
  #   sub.data.3<-subset(sub.data.2, PipePosition==posList[k])

  #number of rows of data
  Ndata<-nrow(sub.data.2)

  #number of Stations
  stations<-as.integer(factor(sub.data.2[,"Plot_Name"], levels=unique(sub.data.2[,"Plot_Name"])))
  Nstations<-length(unique(stations))

  #number of within-year sites (visits)
  visits<-as.integer(factor(sub.data.2[,"Visit"], levels=unique(sub.data.2[,"Visit"])))
  Nvisits<-length(unique(visits))

  #number of positions (n=4)
  pos<-as.integer(factor(sub.data.2[,"PipePosition"], levels=unique(sub.data.2[,"PipePosition"])))
  Npos<-length(unique(pos))

  #number of pins per position (n=9)
  pins<-as.integer(factor(sub.data.2[,"variable"], levels=unique(sub.data.2[,"variable"])))
  Npins<-length(unique(pins))

  #number of pos.pin (n=36)
  pos.pins<-as.integer(factor(sub.data.2[,"pos.pin"], levels=unique(sub.data.2[,"pos.pin"])))
  Npos.pins<-length(unique(pos.pins))

  #observer
  obs<-as.integer( factor( sub.data.2[,"Last_Name"] ,levels=unique(sub.data.2[,"Last_Name"])))

  #data (x=years, y=delta SET values)
  x=as.numeric(sub.data.2[,"Year"])
  y=as.numeric(sub.data.2[,"value"])

  ###############################################################################################################################
  # Standardize data (divide by SD) to make initialization easier.

  xM = mean( x )
  xSD = sd( x )
  yM = mean( y )
  ySD = sd( y )
  zx = ( x - xM ) / xSD
  zy = ( y - yM ) / ySD

  # Specify data, as a list.
  dataList = list(
    Ndata = Ndata,
    Nstations = Nstations,
    stations = stations,
    Npos = Npos,
    pos = pos,
    Npins = Npins,
    pins = pins,
    obs=obs,
    x = zx ,
    y = zy
  )

  ###############################

  params = c("station_mu","station_tau","position_mu","position_tau","b0","b1","tau" , "b0_mu","b0_tau", "b1_mu","b1_tau","m","d")
  adaptSteps = 5000              # Number of steps to "tune" the samplers.
  burnInSteps = 20000            # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps=100000           # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  # Create, initialize, and adapt the model:
  jagsModel=NULL

  jagsModel = jags.model( modelIn , data=dataList , inits=NULL ,
                          n.chains=nChains , n.adapt=adaptSteps )

  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )

  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  out = coda.samples( jagsModel , variable.names=params ,
                      n.iter=nPerChain , thin=thinSteps )

  return(out)
}
