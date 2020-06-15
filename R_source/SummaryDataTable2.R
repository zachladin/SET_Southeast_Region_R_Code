#Summary Table 1 function

#make a table of NWRs included in data, and # of SET stations (Table 1.)

SummaryDataTable2<-function(dataIn){

  #make table of Refuge-level slopes

  #read in estimates without observer effects
  refuge.noObs<-read.csv(paste(getwd(),"Results","Tables","All_Refuge-level_slopes.csv",sep="/"))
  refuge.noObs.1<-refuge.noObs[,c("State","Unit_Code","n","mean","SE", "CV")]
  colnames(refuge.noObs.1)<-c("State","Unit_Code","nSET","Mean","SE","CV")


  #read in estimates with observer effects
  refuge.Obs<-read.csv(paste(getwd(),"Results","Tables","All_Refuge-level_slopes.Obs.csv",sep="/"))
  refuge.Obs.1<-refuge.Obs[,c("Unit_Code","mean","SE","CV")]
  colnames(refuge.Obs.1)<-c("Unit_Code","Mean.obs","SE.obs","CV.obs")

  #combine into one table
  Freq.table<-merge(refuge.noObs.1, refuge.Obs.1, by="Unit_Code")

  #add # of unique observers
  range.years.obs<-range.years[,c("Unit_Code","nObs")]

  Freq.table.2<-merge(Freq.table, range.years.obs, by="Unit_Code")

  #reorder table columns
  Freq.table.3<-Freq.table.2[,c("Unit_Code","State","nSET","nObs","Mean","SE","CV","Mean.obs", "SE.obs","CV.obs")]

  #now sort by Latitude
  #create integer column of order for NWRs
  Freq.table.3$orderUnits<-c(15,12,11,14,6,1,4,7, 9, 3, 13, 2,8, 5, 10)
  Freq.table.3$orderUnits<-as.integer(Freq.table.3$orderUnits)
  Freq.table.4<-Freq.table.3[ order(Freq.table.3$orderUnits),]
  row.names(Freq.table.4)<-NULL

  #save Table as .csv file
  write.csv(Freq.table.4,file=paste(getwd(),"Results","Tables","Table2_Frequentist_NWR_means.csv",sep="/"),row.names=FALSE)


return(Freq.table.4)

}
