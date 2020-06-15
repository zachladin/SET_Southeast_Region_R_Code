#build function for running Bayesian model with Observer effects
runBayesYearVisitObs<-function(dataIn){

  data<-dataIn

  #get stationList
  stationList<-unique(sort(as.character(data$Plot_Name)))


  #get unitList
  unitList<-unique(sort(as.character(data$Unit_Code)))

  #get smiUnitList
  smiUnitList<-unique(sort(data$Site_Name))

  for(i in 1:length(unitList)){

    sub.data<-subset(data, Unit_Code==unitList[i])
    temp.unit<-as.character(unique(sub.data$Unit_Code))
    print(temp.unit)

    new.stationList<-unique(sort(as.character(sub.data$Plot_Name)))

    for(k in 1:length(new.stationList)){


    for(j in 1:length(new.stationList)){
      print(j)
      sub.data.2<-subset(sub.data, Plot_Name==new.stationList[j])

      stationName<-as.character(unique(sub.data.2$Plot_Name))

      #runJAGSmodel function
      out<-tryCatch({runJAGSmodelYearVisit(dataIn=sub.data.2, modelIn="modelRandObs.txt")
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
}
