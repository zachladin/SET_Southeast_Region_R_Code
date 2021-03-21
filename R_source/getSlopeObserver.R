getSlopeObserver<-function(dataIn){
  set.data<-dataIn


  #create folders
  dir.create(paste(getwd(), "Results","Refuge_Results",sep="/"))
  dir.create(paste(getwd(), "Results","Site_Results",sep="/"))
  dir.create(paste(getwd(), "Results","Station_Results",sep="/"))

  #extract covariates
  dataCovs<-data.frame(unique(set.data[,c("State","RefugeName")]))
  
  ######################################################
  message("Compiling lists from data.")
  #get refugeList
  refugeList<-sort(unique(as.character(set.data$RefugeName)))

  ######################################################

  message("Fitting linear model to get slopes of delta height (mm) at each pin, and averaging for each SET station.")

  all.pins<-list()
  all.positions<-list()
  all.stations<-list()
  all.refuges<-list()
  for(j in 1:length(refugeList)){
    refuge.data<-subset(set.data,RefugeName==refugeList[j])

    refugeName<-as.character(unique(refuge.data$RefugeName))
    #get siteList
    siteList<-sort(unique(as.character(refuge.data$Site_Name)))

  refuge.pins<-list()
  refuge.positions<-list()
  refuge.stations<-list()
  for(k in 1:length(siteList)){

    site.data<-subset(refuge.data, Site_Name==siteList[k])

    siteName<-as.character(unique(site.data$Site_Name))

    #get stationList
    stationList<-sort(unique(as.character(site.data$Plot_Name)))

    #get refuge covariates
    refugeCovs<-unique(site.data[,c("State","RefugeName",
                                    "Site_Name","Plot_Name","Latitude","Longitude")])


  site.pin.slopes<-list()
  site.positions<-list()
  site.stations<-list()
  for(i in 1:length(stationList)){

    new.data<-subset(site.data, Plot_Name==stationList[i])

    refugeName<-as.character(unique(new.data$RefugeName))
    siteName<-as.character(unique(new.data$Site_Name))
    stationName<-as.character(unique(new.data$Plot_Name))

    #get pospinList
    pospinList<-unique(sort(as.character(new.data$pos.pin)))

    #get covariates
    stationCovs<-unique(new.data[,c("State","RefugeName","Site_Name","Plot_Name","Latitude","Longitude")])

    positionCovs<-unique(new.data[,c("State","RefugeName","Site_Name","Plot_Name","PipePosition","Latitude","Longitude")])

    regResults.out<-list()
    for(k in 1:length(pospinList)){
      sub.data<-subset(new.data, pos.pin==pospinList[k])
      sub.data$ReaderFullName<-as.factor(as.character(sub.data$ReaderFullName))
      sub.data$Year<-as.integer(as.factor(sub.data[,"Year"]))
      sub.data$Visit<-as.integer(as.factor(sub.data[,"Visit"]))
      sub.data$year.visit<-as.integer(as.factor(sub.data$year.visit))

        sub.dataCovs<-unique(sub.data[,c("State","RefugeName",
                                         "Site_Name","Plot_Name","PipePosition","variable","pos.pin")])

        #linear model with Visit nested within Year, and Observer treated as random effect - with error handling
        mod.1<-tryCatch({
          lmer(value ~ Year + (1|ReaderFullName),data=sub.data)
        },error=function(cond2){
          cond2=lm(value ~ Year, data=sub.data)
        },error=function(cond3){
          cond3=NA
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


    #save station results
    dir.create(paste(getwd(), "Results","Station_Results",refugeName,sep="/"))
    dir.create(paste(getwd(), "Results","Station_Results",refugeName,siteName,sep="/"))
    dir.create(paste(getwd(), "Results","Station_Results",refugeName,siteName,stationName,sep="/"))

     #save station results
     write.csv(regResults.out, file=paste(getwd(), "Results","Station_Results",refugeName,siteName,stationName,paste(stationName,"pin-level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

     #get means for each position
     station.positions<-summaryFunction(dataIn=regResults.out, factor="PipePosition", response="slope")
     station.positions.out.1<-merge(station.positions, positionCovs, by="PipePosition")
     station.positions.out<-station.positions.out.1[,c("State","RefugeName","Site_Name","Plot_Name","Latitude","Longitude",
                                                   "PipePosition","n","mean","var","SD","SE","CV","lwr","upr")]

     #save compiled Position-level slopes
     write.csv(station.positions.out, file=paste(getwd(),"Results","Station_Results",refugeName,siteName,stationName,paste(stationName,"Position_level_slopes_Obs.csv",sep="_"),sep="/"),row.names=FALSE)

     #get station means
     station.slopes<-summaryFunction(dataIn=station.positions.out, factor="Plot_Name",response="mean")
     station.merge.1<-merge(station.slopes, refugeCovs, by="Plot_Name")
     station.merge<-station.merge.1[,c("State","RefugeName","Site_Name","Plot_Name","Latitude","Longitude",
                                       "n","mean","var","SD","SE","CV","lwr","upr")]

     write.csv(station.merge, file=paste(getwd(), "Results","Station_Results",refugeName,siteName,stationName,paste(stationName,"Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)


     #compile pin-level slopes for each site
     site.pin.slopes<-rbind(site.pin.slopes,regResults.out)

      #compile position means
      site.positions<-rbind(site.positions,station.positions.out)

      #compile station means
      site.stations<-rbind(site.stations, station.merge)

   }

  message("Saving output to .csv file.")
  dir.create(paste(getwd(), "Results","Site_Results",refugeName,sep="/"))
  dir.create(paste(getwd(), "Results","Site_Results",refugeName,siteName,sep="/"))

  message("Saving output to .csv file.")
  write.csv(site.pin.slopes, file=paste(getwd(), "Results","Site_Results",refugeName,siteName, paste(siteName,"pin-level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

  write.csv(site.positions, file=paste(getwd(), "Results","Site_Results",refugeName,siteName, paste(siteName,"position","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

  #compile results for each refuge

  dir.create(paste(getwd(),"Results","Refuge_Results",refugeName,sep="/"))

  refuge.pins<-rbind(refuge.pins,site.pin.slopes)
  write.csv(refuge.pins, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Pin-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  refuge.positions<-rbind(refuge.positions,site.positions)
  write.csv(refuge.positions, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Position-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  refuge.stations<-rbind(refuge.stations, site.stations)
  write.csv(refuge.stations, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  makePlot<-function(dataIn){

    plot.refuge.data.1<-dataIn

    #remove NAs
    plot.refuge.data<-plot.refuge.data.1[is.na(plot.refuge.data.1$mean)==FALSE,]

    minSET<-round(min(plot.refuge.data$mean, na.rm=TRUE)-10,0)
    maxSET<-round(max(plot.refuge.data$mean, na.rm=TRUE)+10,0)

    plot.refuge.data$Plot_Name<-as.factor(as.character(plot.refuge.data$Plot_Name))

    #make sure SET stations are in correct order (by Site)
    plot.refuge.data$Plot_Name <- factor(plot.refuge.data$Plot_Name, levels =plot.refuge.data$Plot_Name )

    plot1=NULL
    plot1<-ggplot(data=plot.refuge.data)+
      coord_flip()+
      aes(x=Plot_Name, y=mean, ymin=mean-SE, ymax=mean+SE,group=Site_Name)+

      geom_hline(yintercept=0,lty=2,color="darkgray")+
      geom_bar(stat="identity", position=position_dodge(),aes(fill=Site_Name),size=2,alpha=0.9)+
      geom_errorbar(width=0.1, size=0.5, position=position_dodge(width=0.95),aes(color=Site_Name))+
      # scale_fill_manual(values=c("darkorange2","royalblue4","palegreen3"),guide=guide_legend(title=""))+
      # scale_color_manual(values=c("darkorange2","royalblue4","palegreen3"),guide=guide_legend(title=""))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=1.2, size=12, color="black"),
            axis.text.y = element_text(angle=0,size=10, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(minSET,maxSET)+
      labs(x="SET station",y="Change in SET height (mm/yr)")+
      scale_x_discrete(limits = rev(levels(plot.refuge.data$Plot_Name)))+
      scale_fill_discrete(guide_legend(title="Site"))+
      guides(color=FALSE)+
      ggtitle(paste("Frequentist-estimated mean and SE for change in\nmarsh elevation height (mm/year) at" ,refugeName))

    print(plot1)
    ggsave(plot1, filename=paste(refugeName,"Frequentist_SET_mean.Obs.png",sep="_"),path=paste(getwd(),"Results","Figures","SET_Figures",refugeName,sep="/"), width=9,height=6.5, limitsize=FALSE)

    return(plot1)
  }



  #get refuge-level means
  refuge.slopes<-summaryFunction(dataIn=refuge.stations, factor="RefugeName",response="mean")
  refuge.merge.1<-merge(refuge.slopes, dataCovs, by="RefugeName")
  refuge.merge<-refuge.merge.1[,c("State","RefugeName", "n","mean","var","SD","SE","CV","lwr","upr")]
  write.csv(refuge.merge, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Refuge-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  }

  dir.create(paste(getwd(),"Results","Figures",sep="/"))
  dir.create(paste(getwd(),"Results","Figures","SET_figures",sep="/"))
  dir.create(paste(getwd(),"Results","Figures","SET_figures",refugeName,sep="/"))

  #run makePlot function
  try(plot1<-makePlot(dataIn=refuge.stations))


  #compile all results
  all.pins<-rbind(all.pins, refuge.pins)

  all.positions<-rbind(all.positions, refuge.positions)

  all.stations<-rbind(all.stations, refuge.stations)

  all.refuges<-rbind(all.refuges,refuge.merge )

  }
  #save files
  write.csv(all.pins, file=paste(getwd(), "Results","Tables",paste("All","Pin-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.positions, file=paste(getwd(), "Results","Tables",paste("All","Position-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.stations, file=paste(getwd(), "Results","Tables",paste("All","Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.refuges, file=paste(getwd(), "Results","Tables",paste("All","Refuge-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)


  return(all.stations)
}
