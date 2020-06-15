#Summary Table 1 function

#make a table of NWRs included in data, and # of SET stations (Table 1.)

SummaryDataTable1<-function(dataIn){

  data<-dataIn

#remove "NWR" from RefugeName
data$RefugeName2<-gsub("NWR","",data$RefugeName)

SummaryTable.1<-unique(data[,c("State","RefugeName2","Unit_Code")])

#get freqency of SET stations per refuge

sub.data<-unique(data[,c("State","RefugeName2","Unit_Code","Site_Name","Plot_Name")])
count.stations<-table(sub.data$Unit_Code, sub.data$Plot_Name)
count.stations.total<-rowSums(count.stations)
count.stations.total.1<-data.frame(Unit_Code=names(count.stations.total), nSET=as.data.frame(count.stations.total))

#get years sampled (start and end), and get number of observers
sub.data.years<-unique(data[,c("Unit_Code","Year","Last_Name")])

#get refugeList
refugeList<-unique(sort(as.character(sub.data.years$Unit_Code)))

#loop over refugeList to get range of years per refuge
range.years<-list()
for(i in 1:length(refugeList)){
  #subset data for 1 refuge
  new.data<-subset(sub.data.years, Unit_Code==refugeList[i])
  #get Unit_Code
  refugeName<-as.character(unique(new.data$Unit_Code))
  #get starting year
  startYear<-min(new.data$Year)
  #get ending year
  endYear<-max(new.data$Year)
  #get total years
  nYears<-endYear-startYear
  #get n unique observers (data recorders)
  nObs<-length(unique(new.data$Last_Name))

  range.years.1<-data.frame(Unit_Code=refugeName, startYear=startYear, endYear=endYear, nYears=nYears,nObs=nObs)
  range.years<-rbind(range.years, range.years.1)

}


#combine with SummaryTable.1
SummaryTable.2<-merge(SummaryTable.1, count.stations.total.1, by="Unit_Code")

#now combine with range.years
SummaryTable.3<-merge(SummaryTable.2, range.years, by="Unit_Code")

#Now sort table by Latitude (reorder Unit_Code factor)
#create integer column of order for NWRs
SummaryTable.3$orderUnits<-c(15,12,11,14,6,1,4,7, 9, 3, 13, 2,8, 5, 10)
SummaryTable.3$orderUnits<-as.integer(SummaryTable.3$orderUnits)
SummaryTable.4<-SummaryTable.3[ order(SummaryTable.3$orderUnits),]

#rename column headers
colnames(SummaryTable.4)<-c("Unit_Code", "State","Refuge_Name",
                            "nSET","Start_Year","End_Year","nYears","nObservers","orderUnits")

#reorder columns
SummaryTable.5<-SummaryTable.4[,c("Refuge_Name","Unit_Code","State","nSET","nObservers","nYears","Start_Year","End_Year")]

#remove row.names from table
row.names(SummaryTable.5)<-NULL


#create new Results folders to save table
dir.create(paste(getwd(),"Results",sep="/"))
dir.create(paste(getwd(),"Results","Tables",sep="/"))

#save Table as .csv file
write.csv(SummaryTable.5,file=paste(getwd(),"Results","Tables","Table1_SummaryOfData.csv",sep="/"),row.names=FALSE)

return(SummaryTable.5)
}
