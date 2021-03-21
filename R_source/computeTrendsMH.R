computeTrendsMH<-function(dataIn){
  
  mh.data<-dataIn
  
  #get list of unique NWRs
  refugeList<-sort(unique(as.character(mh.data$Refuge)))
  
  all.mh.stations<-list()
  all.mh.sites<-list()
  all.mh.refuges<-list()
  for(j in 1:length(refugeList)){
    
    #print refuge to show progress
    print(refugeList[j])
    
    refuge.data<-subset(mh.data,Refuge==refugeList[j])
    
    #get refugeName
    refugeName<-as.character(unique(refuge.data$Refuge))
    
    #get siteList
    siteList<-sort(unique(as.character(refuge.data$Site_Name)))
    
    refuge.stations<-list()
    refuge.sites<-list()
    for(k in 1:length(siteList)){
      
      site.data<-subset(refuge.data, Site_Name==siteList[k])
      
      siteName<-as.character(unique(site.data$Site_Name))
      
      #print Site_Name to show progress
      print(siteName)
      
      #get stationList
      stationList<-sort(unique(as.character(site.data$Station_Name)))
      
      #get refuge covariates
      refugeCovs<-unique(site.data[,c("State","Refuge",
                                      "Site_Name","Station_Name","Latitude","Longitude")])
      
      #add Start_Year and End_Year
      refugeCovs$Start_Year<-min(as.integer(refuge.data$Year))
      refugeCovs$End_Year<-max(as.integer(refuge.data$Year))
      
      #get siteCovs
      siteCovs<-unique(site.data[,c("State","Refuge","Site_Name")])
      
      #add Start_Year and End_Year
      siteCovs$Start_Year<-min(as.integer(site.data$Year))
      siteCovs$End_Year<-max(as.integer(site.data$Year))
      
      site.stations<-list()
      for(i in 1:length(stationList)){
        #print(stationList[i])
        
        #print Station_Name (Station) to show progress
        print(stationList[i])
        
        new.data<-subset(site.data, Station_Name==stationList[i])
        
        stationName<-as.character(unique(new.data$Station_Name))
        
        #get mhList
        mhList<-unique(sort(as.character(new.data$MarkerHorizonID)))
        
        #get covariates
        stationCovs<-unique(new.data[,c("State","Refuge","Site_Name","Station_Name","Latitude","Longitude")])
        
        #add Start_Year and End_Year
        stationCovs$Start_Year<-min(as.integer(new.data$Year))
        stationCovs$End_Year<-max(as.integer(new.data$Year))
        
        
        mhCovs<-unique(new.data[,c("State","Refuge","Site_Name","Station_Name","MarkerHorizonID","Year","Latitude","Longitude")])
        
        message("Getting slope and intercept from delta at each pin vs. time regressions.")
        regResults.out<-list()
        for(k in 1:length(mhList)){
          
          #print(pospinList[k])
          sub.data<-unique(subset(new.data, MarkerHorizonID==mhList[k]))

          #sub.data<-merge(sub.data.2, sub.dataCovsYear, by="MarkerHorizonID",all.x=TRUE)
          
          sub.dataCovs<-unique(sub.data[,c("State","Refuge","Site_Name","Station_Name")])
          #add Start_Year and End_Year
          sub.dataCovs$Start_Year<-min(as.integer(sub.data$Year))
          sub.dataCovs$End_Year<-max(as.integer(sub.data$Year))
          
          #get the mean depth for each sampling date
          sub.data.sampling.event<-aggregate(DepthToBenchmark_mm~EventDateTimeWithOffset, FUN=mean,data=sub.data)
          #rename columns
          colnames(sub.data.sampling.event)<-c("Date","Mean_Delta_Depth_mm")
          
          #make Date a Date object
          sub.data.sampling.event$Date<-as.Date(as.character(sub.data.sampling.event$Date), tryFormats = c("%m/%d/%Y"))
          
          #make Year column
          sub.data.sampling.event$Year<-year(sub.data.sampling.event$Date)
          
          #linear model with Visit nested within Year, and if there is only 1 visit, fit just Year
          mod.1<-tryCatch({
            lm(Mean_Delta_Depth_mm ~ Year, data=sub.data.sampling.event)
          },error=function(cond2){
            cond2=NA
          })
          
          #get slope over years
          slope<-NULL
          try(slope<-coef(summary(mod.1))[ , "Estimate"][2])
          intercept<-NULL
          try(intercept<-coef(summary(mod.1))[ , "Estimate"][1])
          
          #combine slope with station covs
          regResults<-NULL
          regResults<-tryCatch({
            data.frame(sub.dataCovs,slope=slope,intercept=intercept)
          },error=function(cond2){
            cond2=data.frame(sub.dataCovs,slope=NA,intercept=NA)
            cond2
          })
          
          #compile regResults 
          regResults.out<-rbind(regResults.out, regResults)
        }
        
        #get means for each position
        station.mean<-summaryFunction(dataIn=regResults.out, factor="Station_Name", response="slope")
        station.mh.out.1<-merge(station.mean, stationCovs, by="Station_Name")
        station.mh.out<-station.mh.out.1[,c("State","Refuge","Site_Name","Station_Name","Latitude","Longitude","Start_Year","End_Year","n","mean","var","SD","SE","CV","lwr","upr")]
        
        #compile station means
        site.stations<-rbind(site.stations, station.mh.out)
      }
      
      #get site means
      site.slopes<-summaryFunction(dataIn=site.stations, factor="Site_Name",response="mean")
      site.merge.1<-merge(site.slopes, siteCovs, by="Site_Name")
      site.merge<-site.merge.1[,c("State","Refuge","Site_Name","Start_Year","End_Year", "n","mean","var","SD","SE","CV","lwr","upr")]
      
      refuge.stations<-rbind(refuge.stations, site.stations)
      
      refuge.sites<-rbind(refuge.sites, site.merge)
      
      #get refuge-level means
      refuge.slopes<-summaryFunction(dataIn=refuge.stations, factor="Refuge",response="mean")
      refuge.merge.1<-merge(refuge.slopes, refugeCovs, by="Refuge")
      refuge.merge<-refuge.merge.1[,c("State","Refuge", "Start_Year","End_Year","n","mean","var","SD","SE","CV","lwr","upr")]
    }
    

    all.mh.stations<-rbind(all.mh.stations, refuge.stations)
    
    all.mh.sites<-rbind(all.mh.sites, refuge.sites)
    
    all.mh.refuges<-rbind(all.mh.refuges,refuge.merge)
    
  }
  
  all.mh.stations<<-unique(all.mh.stations)
  all.mh.sites<<-unique(all.mh.sites)
  all.mh.refuges<<-unique(all.mh.refuges)

}