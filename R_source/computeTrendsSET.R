computeTrendsSET<-function(dataIn){
  
  set.data<-dataIn
  
  #get list of unique NWRs
  refugeList<-sort(unique(as.character(set.data$Refuge)))
  
  all.set.pins<-list()
  all.set.positions<-list()
  all.set.stations<-list()
  all.set.sites<-list()
  all.set.refuges<-list()
  for(j in 1:length(refugeList)){
    
    #print refuge to show progress
    print(refugeList[j])
    
    refuge.data<-subset(set.data,Refuge==refugeList[j])
    
    #get refugeName
    refugeName<-as.character(unique(refuge.data$Refuge))
    
    #get siteList
    siteList<-sort(unique(as.character(refuge.data$Site_Name)))
    
    refuge.pins<-list()
    refuge.positions<-list()
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
      
      #get siteCovs
      siteCovs<-unique(site.data[,c("State","Refuge","Site_Name")])
      
      site.pin.slopes<-list()
      site.positions<-list()
      site.stations<-list()
      for(i in 1:length(stationList)){
        #print(stationList[i])
        
        #print Station_Name (Station) to show progress
        print(stationList[i])
        
        new.data<-subset(site.data, Station_Name==stationList[i])
        
        stationName<-as.character(unique(new.data$Station_Name))
        
        #get pospinList
        pospinList<-unique(sort(as.character(new.data$pos.pin)))
        
        #get covariates
        stationCovs<-unique(new.data[,c("State","Refuge","Site_Name","Station_Name","Latitude","Longitude")])
        
        positionCovs<-unique(new.data[,c("State","Refuge","Site_Name","Station_Name","Position_Name","Latitude","Longitude")])
        
        message("Getting slope and intercept from delta at each pin vs. time regressions.")
        regResults.out<-list()
        for(k in 1:length(pospinList)){
          #print(pospinList[k])
          sub.data<-subset(new.data, pos.pin==pospinList[k])
          sub.data$value<-as.numeric(sub.data[,"value"])
          sub.data$ReaderFullName<-as.factor(as.character(sub.data$ReaderFullName))
          sub.dataCovs<-unique(sub.data[,c("State","Refuge","Site_Name","Station_Name","Position_Name","variable","pos.pin")])
          
          #linear model with Visit nested within Year, and if there is only 1 visit, fit just Year
          mod.1<-tryCatch({
            lm(value ~ Year, data=sub.data)
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
          
          #compile regResults over each pin
          regResults.out<-rbind(regResults.out, regResults)
        }
        
        #get means for each position
        station.positions<-summaryFunction(dataIn=regResults.out, factor="Position_Name", response="slope")
        station.positions.out.1<-merge(station.positions, positionCovs, by="Position_Name")
        station.positions.out<-station.positions.out.1[,c("State","Refuge","Site_Name","Station_Name","Latitude","Longitude","Position_Name","n","mean","var","SD","SE","CV","lwr","upr")]
        
        #get station means
        station.slopes<-summaryFunction(dataIn=station.positions.out, factor="Station_Name",response="mean")
        station.merge.1<-merge(station.slopes, refugeCovs, by="Station_Name")
        station.merge<-station.merge.1[,c("State","Refuge","Site_Name","Station_Name","Latitude","Longitude", "n","mean","var","SD","SE","CV","lwr","upr")]
        
        #compile pin-level slopes for each site
        site.pin.slopes<-rbind(site.pin.slopes,regResults.out)
        
        #compile position means
        site.positions<-rbind(site.positions,station.positions.out)
        
        #compile station means
        site.stations<-rbind(site.stations, station.merge)
      }
      
      #get site means
      site.slopes<-summaryFunction(dataIn=site.stations, factor="Site_Name",response="mean")
      site.merge.1<-merge(site.slopes, siteCovs, by="Site_Name")
      site.merge<-site.merge.1[,c("State","Refuge","Site_Name", "n","mean","var","SD","SE","CV","lwr","upr")]
      
      refuge.pins<-rbind(refuge.pins,site.pin.slopes)
      
      refuge.positions<-rbind(refuge.positions,site.positions)
      
      refuge.stations<-rbind(refuge.stations, site.stations)
      
      refuge.sites<-rbind(refuge.sites, site.merge)
      
      #get refuge-level means
      refuge.slopes<-summaryFunction(dataIn=refuge.stations, factor="Refuge",response="mean")
      refuge.merge.1<-merge(refuge.slopes, sub.dataCovs, by="Refuge")
      refuge.merge<-refuge.merge.1[,c("State","Refuge", "n","mean","var","SD","SE","CV","lwr","upr")]
    }
    
    #compile all results
    all.set.pins<-rbind(all.set.pins, refuge.pins)
    
    all.set.positions<-rbind(all.set.positions, refuge.positions)
    
    all.set.stations<-rbind(all.set.stations, refuge.stations)
    
    all.set.sites<-rbind(all.set.sites, refuge.sites)
    
    all.set.refuges<-rbind(all.set.refuges,refuge.merge)
    
  }
  
  all.set.pins<<-all.set.pins
  all.set.positions<<-all.set.positions
  all.set.stations<<-all.set.stations
  all.set.sites<<-all.set.sites
  all.set.refuges<<-all.set.refuges
}
