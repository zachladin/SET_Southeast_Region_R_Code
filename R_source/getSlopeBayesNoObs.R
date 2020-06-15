getSlopeBayesNoObs<-function(dataIn){
  set.data<-dataIn

  #create folders
  dir.create(paste(getwd(), "Results","Results_Bayes",sep="/"))

  dir.create(paste(getwd(), "Results","Results_Bayes","Refuge_Results",sep="/"))
  dir.create(paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",sep="/"))
  dir.create(paste(getwd(), "Results", "Results_Bayes","Station_Results",sep="/"))

  #extract covariates
  dataCovs<-unique(set.data[,c("State","Unit_Code")])

  ######################################################
  message("Compiling lists from data.")

  #get refugeList
  refugeList<-sort(as.character(unique(set.data$Unit_Code)))
  
  ######################################################

  message("Fitting linear mixed-effects model to get slopes of delta height (mm) at each pin, and averaging for each SET station.")

  all.pins<-list()
  all.positions<-list()
  all.stations<-list()
  all.refuges<-list()
  for(j in 1:length(refugeList)){
    refuge.data<-subset(set.data,Unit_Code==refugeList[j])

    refugeName<-as.character(unique(refuge.data$Unit_Code))
    print(refugeName)

    #get smiUnitList
    smiUnitList<-sort(unique(as.character(refuge.data$Site_Name)))
    print(smiUnitList)

    refuge.pins<-list()
    refuge.positions<-list()
    refuge.stations<-list()
    for(k in 1:length(smiUnitList)){

      smi.data<-subset(refuge.data, Site_Name==smiUnitList[k])

      smiUnitName<-as.character(unique(smi.data$Site_Name))
      print(smiUnitName)

      #get stationList
      stationList<-sort(unique(as.character(smi.data$Plot_Name)))
      print(stationList)

      #get refuge covariates
      refugeCovs<-unique(smi.data[,c("State","Unit_Code",
                                     "Site_Name","Plot_Name","Lat","Long")])


      smiUnit.pin.slopes<-list()
      smiUnit.positions<-list()
      smiUnit.stations<-list()
      for(i in 1:length(stationList)){

        new.data<-subset(smi.data, Plot_Name==stationList[i])

        refugeName<-as.character(unique(new.data$Unit_Code))
        smiUnitName<-as.character(unique(new.data$Site_Name))
        stationName<-as.character(unique(new.data$Plot_Name))

        print(stationName)

        #get positionList
        positionList<-unique(sort(as.character(new.data$Position_Name)))
        print(positionList)

        #get pospinList
        pospinList<-unique(sort(as.character(new.data$pos.pin)))
        print(pospinList)

        #get covariates
        stationCovs<-unique(new.data[,c("State","Unit_Code",
                                        "Site_Name","Plot_Name","Lat","Long")])

        positionCovs<-unique(new.data[,c("State","Unit_Code","Site_Name","Plot_Name","Position_Name","Lat","Long")])

        pos.pinCovs<-unique(new.data[,c("State","Unit_Code","Site_Name","Plot_Name","Position_Name","variable","pos.pin","Lat","Long")])

        #runJAGSmodel function
        out<-tryCatch({runJAGSmodel(dataIn=new.data, modelIn="modelNoObs.txt")
        },error=function(cond2){
          cond2=NA
        })

        head(out)
        #save station results
        dir.create(paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,sep="/"))
        dir.create(paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,sep="/"))
        dir.create(paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,sep="/"))

        #save MCMC output
        try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,paste(stationName,"Bayes.mcmc.output.Obs.R",sep="."), sep="/")))

        message("Compiling JAGS MCMC output, and getting posterior summary statistics.")

        #get summary of output
        new.bayes<-tryCatch({summary(window(out,start=11001))
        },error=function(cond2){
          cond2=data.frame(Mean=NA, SD=NA, Naive.SE=NA, Time.series.SE=NA)
        })

        try(new.bayes.means<-as.data.frame(new.bayes$statistics))
        try(new.bayes.quantiles<-as.data.frame(new.bayes$quantiles))
        try(new.bayes.out<-data.frame(new.bayes.means, new.bayes.quantiles))

        #save summary of mcmc chains
        try(dput(new.bayes.out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,paste(stationName,"Bayes.summary.output.Obs.R",sep="."), sep="/")))

        #get summary of intercepts (b0)
        try(pos.pin.intercepts<-new.bayes$statistics[1:36,])
        try(pos.pin.intercepts.quant<-new.bayes$quantiles[1:36,])
        try(pos.pin.intercepts.all.out<-cbind(pos.pin.intercepts, pos.pin.intercepts.quant))

        #get summary of slopes (b1)
        try(pos.pin.slopes<-new.bayes$statistics[39:74,])
        try(pos.pin.slopes.quant<-new.bayes$quantiles[39:74,])
        try(pos.pin.slopes.all.out<-cbind(pos.pin.slopes, pos.pin.slopes.quant))

        #combine summary of b0 and b1
        try(pos.pin.out<-data.frame(Slope=pos.pin.slopes[,1],Intercept=pos.pin.intercepts[,1]))

        #add pin.pos covs
        pos.pin.out.1<-NULL

        try(pos.pin.out.1<-data.frame(pos.pinCovs, pos.pin.out))

        ifelse(is.null(pos.pin.out.1)==TRUE,
               pos.pin.out.2<-data.frame(pos.pinCovs, Slope=NA, Intercept=NA),
               pos.pin.out.2<-data.frame(pos.pin.out.1)
        )

        #compile results (position-level slopes)
        try(position.means<-new.bayes$statistics[77:80,])

        #compile results (position-level slopes)
        try(position.sd<-new.bayes$statistics[77:80,])


        position.out<-NULL
        try(position.out<-data.frame(positionCovs, position.means))

        ifelse(is.null(position.out)==TRUE,
               position.out.2<-data.frame(positionCovs, Mean=NA, SD=NA, Naive.SE=NA,Time.series.SE=NA),
               position.out.2<-data.frame(position.out)
        )

        #compile results (station-level slopes)
        try(station.mean<-new.bayes$statistics[89,])

        station.out<-NULL
        try(station.out<-data.frame(stationCovs, t(station.mean)))

        ifelse(is.null(station.out)==TRUE,
               station.out.2<-data.frame(stationCovs, Mean=NA, SD=NA, Naive.SE=NA,Time.series.SE=NA),
               station.out.2<-data.frame(station.out)
        )

        #save station results
        try(write.csv(pos.pin.out.2, file=paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,paste(stationName,"Bayes_Pin-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))


        #save compiled Position-level slopes
        try(write.csv(position.out.2, file=paste(getwd(),"Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,paste(stationName,"Bayes_Position-level_slopes.csv",sep="_"),sep="/"),row.names=FALSE))

        try(write.csv(station.out.2, file=paste(getwd(), "Results","Results_Bayes","Station_Results",refugeName,smiUnitName,stationName,paste(stationName,"Bayes_Station-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))


        #compile pin-level slopes for each smiUnit
        try(smiUnit.pin.slopes<-rbind(smiUnit.pin.slopes,pos.pin.out.2))

        #compile position means
        try(smiUnit.positions<-rbind(smiUnit.positions,position.out.2))

        #compile station means
        try( smiUnit.stations<-rbind(smiUnit.stations, station.out.2))

      }

      message("Saving output to .csv file.")

      dir.create(paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",refugeName,sep="/"))
      dir.create(paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",refugeName,smiUnitName,sep="/"))

      try(write.csv(smiUnit.pin.slopes, file=paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",refugeName,smiUnitName, paste(smiUnitName,"Bayes_Pin-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))

      try(write.csv(smiUnit.positions, file=paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",refugeName,smiUnitName, paste(smiUnitName,"Bayes_Position-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))

      #compile results for each refuge

      dir.create(paste(getwd(),"Results","Results_Bayes","Refuge_Results",refugeName,sep="/"))

      try(refuge.pins<-rbind(refuge.pins,smiUnit.pin.slopes))
      try(write.csv(refuge.pins, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Pin-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))

      try(refuge.positions<-rbind(refuge.positions,smiUnit.positions))
      try(write.csv(refuge.positions, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Position-level_slopes.csv",sep="_"),sep="/"),row.names=FALSE))


      try(refuge.stations<-rbind(refuge.stations, smiUnit.stations))
      try(write.csv(refuge.stations, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Station-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))

      ########################################
      #Plot station-level estimates by refuge.


      makePlot<-function(dataIn){

        plot.refuge.data.1<-dataIn

        #remove NAs
        plot.refuge.data<-plot.refuge.data.1[is.na(plot.refuge.data.1$Mean)==FALSE,]

        minSET<-round(min(plot.refuge.data$Mean, na.rm=TRUE)-10,0)
        maxSET<-round(max(plot.refuge.data$Mean, na.rm=TRUE)+10,0)

        plot.refuge.data$Plot_Name<-as.factor(as.character(plot.refuge.data$Plot_Name))

        #make sure SET stations are in correct order (by SMI_Unit)
        plot.refuge.data$Plot_Name <- factor(plot.refuge.data$Plot_Name, levels =plot.refuge.data$Plot_Name )

        plot1<-ggplot(data=plot.refuge.data)+
          coord_flip()+
          aes(x=Plot_Name, y=Mean, ymin=Mean-Time.series.SE, ymax=Mean+Time.series.SE,group=Site_Name)+

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
          labs(x="SET station",y="Change in SET height (mm/year)")+
          scale_x_discrete(limits = rev(levels(plot.refuge.data$Plot_Name)))+
          scale_fill_discrete(guide_legend(title="SMI Unit"))+
          guides(color=FALSE)+
          ggtitle(paste("Bayesian-estimated mean and SE for change in\nmarsh elevation height (mm/year) at" ,refugeName))

        print(plot1)


        return(plot1)
      }

      #get refuge-level means
      try(refuge.slopes<-summaryFunction(dataIn=refuge.stations, factor="Unit_Code",response="Mean"))
      try(refuge.merge.1<-merge(refuge.slopes, dataCovs, by="Unit_Code"))
      try(refuge.merge<-refuge.merge.1[,c("State","Unit_Code", "n","mean","var","SD","SE","CV","lwr","upr")])
      try(write.csv(refuge.merge, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Refuge-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))

    }

    #run makePlot function
    plot1=NULL
    try(plot1<-makePlot(dataIn=refuge.stations))

    dir.create(paste(getwd(),"Results","Results_Bayes","Figures",sep="/"))
    dir.create(paste(getwd(),"Results","Results_Bayes","Figures","SET_figures",sep="/"))
    dir.create(paste(getwd(),"Results","Results_Bayes","Figures","SET_figures",refugeName,sep="/"))
    ggsave(plot1, filename=paste(refugeName,"Bayes_SET_mean.png",sep="_"),path=paste(getwd(),"Results","Results_Bayes","Figures","SET_figures",refugeName,sep="/"), width=9,height=6.5, limitsize=FALSE)




    #compile all results
    try(all.pins<-rbind(all.pins, refuge.pins))

    try(all.positions<-rbind(all.positions, refuge.positions))

    try(all.stations<-rbind(all.stations, refuge.stations))

    try(all.refuges<-rbind(all.refuges,refuge.merge ))

  }
  #save files

  dir.create(paste(getwd(), "Results","Results_Bayes","Tables",sep="/"))

  try(write.csv(all.pins, file=paste(getwd(), "Results","Results_Bayes","Tables",paste("All","Bayes_Pin-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))
  try(write.csv(all.positions, file=paste(getwd(), "Results","Results_Bayes","Tables",paste("All","Bayes_Position-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))
  try(write.csv(all.stations, file=paste(getwd(), "Results","Results_Bayes","Tables",paste("All","Bayes_Station-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))
  try(write.csv(all.refuges, file=paste(getwd(), "Results","Results_Bayes","Tables",paste("All","Bayes_Refuge-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE))


  return(all.stations)
}
