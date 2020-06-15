#SET data analysis using Bayesian hierarchical linear models

#clear environment
rm(list=ls())

###############################################################################################################################
#set wd
setwd("~/Dropbox (ZachTeam)/ZachGreg (1)/SET/SET_analysis_demo")

###############################################################################################################################
#load libraries
library(rjags)

###############################################################################################################################
#Build JAGS models

#First linear model with no observer effects.
modelstring="
model{

#Priors
b0_mu ~ dnorm(0,.01)
b0_tau ~ dgamma(.1,.1)
b1_mu ~ dnorm(0,.01)
b1_tau ~ dgamma(.1,.1)
sG <- pow(m,2)/pow(d,2)
rG <- m/pow(d,2)
m ~ dgamma(1,.1)
d ~ dgamma(1,.1)

for ( s in 1 : Npins ) {
b0[s] ~ dnorm( b0_mu , b0_tau )
b1[s] ~ dnorm( b1_mu , b1_tau)
tau[s] ~ dgamma( sG , rG )
}

for(i in 1:Npos){
position_tau[i] ~ dgamma(sG,rG)
}

for(j in 1:Nstations){
station_tau[j] ~ dgamma(sG,rG)
}

#Likelihood
#mean slope for station
for(i in 1:Nstations){
station_mu[i] ~ dnorm(position_mu[i], station_tau[stations[i]])
}

#mean slope across positions
for(k in 1:Npos){
position_mu[k] ~ dnorm(y[k], position_tau[pos[k]])
}

#mean slope at each pin among years
for(r in 1:Ndata) {
y[r] ~ dnorm( mu[r] , tau[ pins[r] ] )
mu[r] <- b0[pins[r]] + b1[pins[r]] * x[r]  #linear model with no observer covariate

}


#end model
}"
writeLines(modelstring, con="modelNoObs.txt")

######################################################################################################################
#Now build model with Observer as a random effect within a linear mixed-effects model.
modelstring="
model{

#Priors
b0_mu ~ dnorm(0,.01)
b0_tau ~ dgamma(.1,.1)
b1_mu ~ dnorm(0,5)
b1_tau ~ dgamma(.1,.1)
sG <- pow(m,2)/pow(d,2)
rG <- m/pow(d,2)
m ~ dgamma(1,.1)
d ~ dgamma(1,.1)

for ( s in 1 : Npins ) {
b0[s] ~ dnorm( b0_mu , b0_tau )
b1[s] ~ dnorm( b1_mu , b1_tau)
tau[s] ~ dgamma( sG , rG )
}

for(i in 1:Npos){
position_tau[i] ~ dgamma(sG,rG)
}

for(j in 1:Nstations){
station_tau[j] ~ dgamma(sG,rG)
}

#Likelihood
#mean slope for station
for(i in 1:Nstations){
station_mu[i] ~ dnorm(position_mu[i], station_tau[stations[i]])
}

#mean slope across positions
for(k in 1:Npos){
position_mu[k] ~ dnorm(y[k], position_tau[pos[k]])
}

#mean slope at each pin among years
for(r in 1:Ndata) {
y[r] ~ dnorm( mu[r] , tau[ pins[r] ] )
mu[r] <- b0[ obs[pins[r]] ] + b1[pins[r] ] * x[r]  #linear model with observer covariate as random effect
#mu[r] <- b0[ pins[r] ] + b1[pins[r] ] * x[r] + b1[pins[r] ] *obs[pins[r]] #linear model with observer covariate as fixed effect

}


#end model
}"
writeLines(modelstring, con="modelRandObs.txt")
###############################################################################################################################
#read in data
data<-read.csv(paste(getwd(), "Data", "SET.delta.melt.csv",sep="/"),header=TRUE)

#get stationList
stationList<-unique(sort(as.character(data$Plot_Name)))
stationList

#get unitList
unitList<-unique(sort(as.character(data$Unit_Code)))
###############################################################################################################################
#run Bayesian analysis with no Observer effects (year).

#build function
runBayesNoObsYear<-function(dataIn){

  data<-dataIn

  #get stationList
  stationList<-unique(sort(as.character(data$Plot_Name)))

  #get unitList
  unitList<-unique(sort(as.character(data$Unit_Code)))

  for(i in 1:length(unitList)){

    sub.data<-subset(data, Unit_Code==unitList[i])
    temp.unit<-as.character(unique(sub.data$Unit_Code))
    print(temp.unit)

    new.stationList<-unique(sort(as.character(sub.data$Plot_Name)))

    for(j in 1:length(new.stationList)){
      print(j)
      sub.data.2<-subset(sub.data, Plot_Name==new.stationList[j])

      stationName<-as.character(unique(sub.data.2$Plot_Name))

      #runJAGSmodel function
      out<-tryCatch({runJAGSmodelYear(dataIn=sub.data.2, modelIn="modelNoObs.txt")
      },error=function(cond2){
        cond2=NA
      })

      #save MCMC output
      try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"noObs.mcmc.output.R",sep="."), sep="/")))

      try(summary.jags.output<-summary(window(out)))
      try(dput(summary.jags.output,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"noObs.summary.jags.output.R",sep="."), sep="/")))

    }
  }
}

###############################################################################################################################
#run Bayesian analysis with no Observer effects (year.visit).

#build function
runBayesNoObsYearVisit<-function(dataIn){

  data<-dataIn

  #get stationList
  stationList<-unique(sort(as.character(data$Plot_Name)))

  #get unitList
  unitList<-unique(sort(as.character(data$Unit_Code)))

  for(i in 1:length(unitList)){

    sub.data<-subset(data, Unit_Code==unitList[i])
    temp.unit<-as.character(unique(sub.data$Unit_Code))
    print(temp.unit)

    new.stationList<-unique(sort(as.character(sub.data$Plot_Name)))

    for(j in 1:length(new.stationList)){
      print(j)
      sub.data.2<-subset(sub.data, Plot_Name==new.stationList[j])

      stationName<-as.character(unique(sub.data.2$Plot_Name))

      #runJAGSmodel function
      out<-tryCatch({runJAGSmodelYearVisit(dataIn=sub.data.2, modelIn="modelNoObs.txt")
                  },error=function(cond2){
                        cond2=NA
                    })

      #save MCMC output
      try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"noObs.year.visit.mcmc.output.R",sep="."), sep="/")))

      try(summary.jags.output<-summary(window(out)))
      try(dput(summary.jags.output,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"noObs.year.visit.summary.jags.output.R",sep="."), sep="/")))

      }
  }
}

########################################################################################################################
#runBayesNoObs
runBayesNoObsYear(dataIn=data)

#runBayesNoObs
runBayesNoObsYearVisit(dataIn=data)

########################################################################################################################
#build function for running Bayesian model with Observer effects
runBayesWithObsYear<-function(dataIn){

  data<-dataIn

  #get stationList
  stationList<-unique(sort(as.character(data$Plot_Name)))

  #get unitList
  unitList<-unique(sort(as.character(data$Unit_Code)))

  for(i in 1:length(unitList)){

    sub.data<-subset(data, Unit_Code==unitList[i])
    temp.unit<-as.character(unique(sub.data$Unit_Code))
    print(temp.unit)

    new.stationList<-unique(sort(as.character(sub.data$Plot_Name)))

    for(j in 1:length(new.stationList)){
      print(j)
      sub.data.2<-subset(sub.data, Plot_Name==new.stationList[j])

      stationName<-as.character(unique(sub.data.2$Plot_Name))

      #runJAGSmodel function
      out<-tryCatch({runJAGSmodelYear(dataIn=sub.data.2, modelIn="modelRandObs.txt")
      },error=function(cond2){
        cond2=NA
      })

      #save MCMC output
      try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"randObs.mcmc.output.R",sep="."), sep="/")))

      try(summary.jags.output<-summary(window(out)))
      try(dput(summary.jags.output,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"randObs.summary.jags.output.R",sep="."), sep="/")))
    }
  }
}

########################################################################################################################

#build function for running Bayesian model with Observer effects
runBayesWithObsYearVisit<-function(dataIn){

  data<-dataIn

  #get stationList
  stationList<-unique(sort(as.character(data$Plot_Name)))

  #get unitList
  unitList<-unique(sort(as.character(data$Unit_Code)))

  for(i in 1:length(unitList)){

    sub.data<-subset(data, Unit_Code==unitList[i])
    temp.unit<-as.character(unique(sub.data$Unit_Code))
    print(temp.unit)

    new.stationList<-unique(sort(as.character(sub.data$Plot_Name)))

    for(j in 1:length(new.stationList)){
      print(j)
      sub.data.2<-subset(sub.data, Plot_Name==new.stationList[j])

      stationName<-as.character(unique(sub.data.2$Plot_Name))

      #runJAGSmodel function
      out<-tryCatch({runJAGSmodelYearVisitTest(dataIn=sub.data.2, modelIn="modelRandObs.txt")
      },error=function(cond2){
        cond2=NA
      })

      #save MCMC output
      try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"randObs.year.visit.mcmc.output.R",sep="."), sep="/")))

      try(summary.jags.output<-summary(window(out)))
      try(dput(summary.jags.output,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"randObs.year.visit.summary.jags.output.R",sep="."), sep="/")))
    }
  }
}

########################################################################################################################
#runBayesNoObs
runBayesWithObsYear(dataIn=data)

#runBayesNoObs
runBayesWithObsYearVisit(dataIn=data)


#runBayesNoObs  Test
runBayesWithObsYearVisit(dataIn=data)

########################################################################################################################












#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

# checkConvergence = TRUE
# if ( checkConvergence ) {
#   openGraph(width=7,height=7)
#   autocorr.plot( codaSamples[[1]] , ask=FALSE )
#   show( gelman.diag( codaSamples ) )
#   effectiveChainLength = effectiveSize( codaSamples )
#   show( effectiveChainLength )
# }

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )

# Extract chain values for subsequent examination:
zb0_musamp = mcmcChain[, "b0_mu" ]
zb1_musamp = mcmcChain[, "b1_mu" ]
zb0samp = NULL
zb1samp = NULL
for ( pinIdx in 1:Npins ) {
  zb0samp = rbind( zb0samp , mcmcChain[, paste("b0[",pinIdx,"]",sep="") ])
  zb1samp = rbind( zb1samp , mcmcChain[, paste("b1[",pinIdx,"]",sep="") ])
}

#get station slope
zstation_musamp=mcmcChain[,"station_mu"]
# Convert to original scale:
head(zstation_musamp)
mean(zstation_musamp)

# Convert to original scale:
b0_musamp = zb0_musamp * ySD + yM - zb1_musamp * ySD * xM / xSD
b1_musamp = zb1_musamp * ySD / xSD
b0samp   = zb0samp   * ySD + yM - zb1samp   * ySD * xM / xSD
b1samp   = zb1samp   * ySD / xSD


includeRowVec = is.finite( data[,"value"] )
dataMat = data[ includeRowVec , ]
# Retain only the Position A data:
dataMat = dataMat[dataMat[,"Position_Name"]=="B", ]
# Convert to log10(retention):
#dataMat[,"value"] = log10( dataMat[,"value"] )
# Column names and plot labels
yColName = "value" ; yPlotLab = "delta SET (mm)"
xColName = "Year" ; xPlotLab = "Year"
pinColName = "variable" ; pinPlotLab = "Pin"
obsColName = "Last_Name";obsPlotLab ="Obs"

# Display believable intercept and slope values
openGraph(10,5.5)
par( mar=c(4,4,1.75,1)+0.1 , mgp=c(2.5,0.8,0) )
layout( matrix(1:2,nrow=1) )
thinIdx = round(seq(1,length(b0_musamp),length=700))
plot( zb1_musamp[thinIdx] , zb0_musamp[thinIdx] , cex.lab=1.75 ,
      ylab="Standardized Intercept" , xlab="Standardized Slope" ,
      col="skyblue")
plot( b1_musamp[thinIdx] , b0_musamp[thinIdx] , cex.lab=1.0 ,
      ylab=paste("Intercept (",yPlotLab," when ",xPlotLab," =0)",sep="") ,
      xlab=paste("Slope (change in",yPlotLab,"per unit",xPlotLab,")") ,
      col="skyblue")
saveGraph(file=paste(getwd(), "Results_Bayes",temp.unit, new.stationList[j],paste(temp.unit, new.stationList[j],"Position",posList[k],"Hierarchical_SET","SlopeIntercept",sep="_"),sep="/"),type="pdf")

# Make graphs of data and corresponding believable slopes:
openGraph(16,6)
par( mar=c(4,4,1.75,1.5)+0.1 , mgp=c(2.5,0.8,0),oma=c(2,0,0,5) )
layout(matrix(c(1:10,1:10,11:20),nrow=3,byrow=T))
xlims = c( min( dataMat[,xColName] ) ,  max( dataMat[,xColName] ) )
ylims = c( min( dataMat[,yColName] ) ,  max( dataMat[,yColName] ) )
sIdVec = unique( dataMat[,pinColName] )
# Plot data of individual subjects:
npinPlots = 9 # number of representative subject plots to make
pinIdxVec = round(seq(1,length(sIdVec),length=npinPlots))
for ( sIdx in pinIdxVec ) {
  rVec = ( dataMat[,pinColName] == sIdVec[sIdx] )
  new.plot<-plot( dataMat[rVec,xColName] , dataMat[rVec,yColName] , type="o" ,
                  ylim=ylims , ylab=yPlotLab , xlab=xPlotLab , cex.lab=1.5 ,
                  pch=sIdx%%26 , lty=sIdx , main=bquote(.(pinPlotLab) *" "* .(sIdx)) ,
                  cex.main=1.75 )
  #abline( b0_musamp[sIdx] , b1_musamp[sIdx] , col="grey" )
  abline(lm(dataMat[rVec,yColName]~ dataMat[rVec,xColName]), col = "skyblue", lwd = 2)
}
# Plot data of all subjects superimposed
plot( NULL,NULL, xlab=xPlotLab,xlim=xlims , ylab=yPlotLab,ylim=ylims ,
      cex.lab=1.5 , main=paste("All ",pinPlotLab,"s",sep="") , cex.main=1.75 )

for ( sIdx in 1:length(sIdVec) ) {
  rVec = ( dataMat[,pinColName] == sIdVec[sIdx] )
  lines( dataMat[rVec,xColName] , dataMat[rVec,yColName] ,
         lty=sIdx , pch=sIdx%%26 , type="o")
  #abline( b0samp[sIdx] , b1samp[sIdx] , col="blue" )

}
# Plot histograms of corresponding posterior slopes:
xLim=quantile(c(b1samp,b1_musamp),probs=c(0.001,0.999))
for ( sIdx in pinIdxVec ) {
  histInfo = plotPost( b1samp[sIdx,] , xlab="Slope" , compVal=0,
                       HDItextPlace=0.9 , xlim=xLim,col="gray",border="gray")
}
histInfo = plotPost( b1_musamp , xlab="Slope, Group Level" , compVal=0,
                     HDItextPlace=0.9 , xlim=xLim,col="gray",border="gray" )


saveGraph(file=paste(getwd(), "Results_Bayes",temp.unit, new.stationList[j],paste(temp.unit, new.stationList[j],"Position",posList[k],"Obs_randomEffect","Data",sep="_"),sep="/"),type="pdf")
dev.off()
}
  }
}