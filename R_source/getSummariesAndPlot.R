getSummariesAndPlot<-function(dataIn){

  set.data<-dataIn

  ######################################################
  #get refugeList
  refugeList<-sort(unique(as.character(set.data$RefugeName)))

  #get siteList
  siteList<-sort(unique(as.character(set.data$Site_Name)))

  #get stationList
  stationList<-unique(set.data$Plot_Name)
  
  #replace "/" with underscore so not mess up filepath calling
  set.data$Plot_Name<-gsub("/","_",set.data$Plot_Name)
  
  stationList<-sort(unique(as.character(set.data$Plot_Name)))

  ##########################################################
  #create folder
  dir.create(paste(getwd(),"Results","Data_Summary_Results",sep="/"))

  ###################################################################
  #get overall estimates of delta SET (Northeast USFWS Region 5)
  message("Saving Region-wide estimates and generating plots.")

  summary.all<-summaryDelta(new.dataIn=set.data)
  summary.all<-data.frame(RefugeName="All",summary.all)
  
  #set correnct order of year.visit
  year.visit.order<-formatC((unique(sort(as.double(as.character(summary.all$year.visit))))), digits = 2, format = "f")
  
  #set factor levels
  summary.all$year.visit<-factor(as.character(summary.all$year.visit), levels=year.visit.order)
  
  
  write.csv(summary.all, file=paste(getwd(), "Results","Data_Summary_Results",paste("All", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  #plot overall estimates
  nStations=length(unique(set.data$Plot_Name))

  SETplot.cummulative<-ggplot(summary.all)+
    aes(x=year.visit, y=mean,group=1)+
    geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white", fill=alpha("deepskyblue",0.2))+
    geom_errorbar(ymax=summary.all$mean+summary.all$SE, ymin=summary.all$mean-summary.all$SE, width=0, size=0.6, color=alpha("deepskyblue",0.7))+
    geom_line(color="deepskyblue",linetype=1, size=0.8)+
    geom_point(size=1.2, color="deepskyblue")+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    ylim(c(min(summary.all$mean-summary.all$SE)-2,max(summary.all$mean+summary.all$SE)+2))+
    #xlim(2002.1,2016.2)
    labs(x="Year (visits)", y="Elevation change (mm)")
  #scale_x_discrete(limits=c(2002.1,2016.1),breaks=10, "Year")
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.1<-SETplot.cummulative+
    ggtitle(paste("USFWS Southeast Region 4"," SET data (n = ", nStations,")",sep=""))

  print(plot.1)

  #save figure
  myFilepath<-paste(getwd(), "Results","Data_Summary_Results",sep="/")
  ggsave(plot.1, filename=paste("All", "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  ggsave(plot.1, filename=paste("All", "_delta_SET_year_visit.png",sep=""),path=myFilepath, width=8,height=6, limitsize=FALSE)

  ###################################################################
  #get estimates of delta SET by refuge (RefugeName)
  message("Saving delta height (mm) estimates for each Refuge (RefugeName) and generating plots.")
  unit.delta.SET<-list()
  for(i in 1:length(refugeList)){

    new.data<-subset(set.data, RefugeName==refugeList[i])
    refugeName<-unique(as.character(new.data$RefugeName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(new.dataIn=new.data)
    new.summary<-data.frame("State"=stateName[1],"RefugeName"=refugeName, new.summary)

    dir.create(paste(getwd(), "Results","Data_Summary_Results","Refuge_Summary",sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Refuge_Summary",refugeName,sep="/"))
    write.csv(new.summary, file=paste(getwd(), "Results","Data_Summary_Results","Refuge_Summary",refugeName, paste(refugeName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    unit.delta.SET<-rbind(unit.delta.SET, new.summary)

    #plot refuge-wide estimates
    nStations=length(unique(new.data$Plot_Name))

     yminLim<-ifelse(min(new.summary$mean-new.summary$SE)<5,
                     min(new.summary$mean-new.summary$SE)-25,
                     min(new.summary$mean-new.summary$SE)-30)
     #yminLim<-min(new.summary$mean-new.summary$SE)*1.01
    
     ymaxLim<-ifelse(max(new.summary$mean+new.summary$SE)==0,
                     max(new.summary$mean+new.summary$SE)+10,
                     max(new.summary$mean+new.summary$SE)*1.7)
    
    #now plot unit results
    SETplot.unit<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white", fill=alpha("royalblue2",0.3))+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0, size=0.6, color=alpha("royalblue2",0.7))+
      geom_line(color="royalblue2",linetype=1, size=1)+
      geom_point(size=2, color="royalblue2")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      #ylim(c(yminLim, ymaxLim))+
      #scale_x_continuous(expand=c(0,0), limits=c(0,10)) +
      scale_y_continuous(expand=c(0,0), limits=c(0-ymaxLim, ymaxLim)) +
      coord_cartesian(xlim=c(min(as.numeric(new.summary$year.visit)),max(as.numeric(new.summary$year.visit))), ylim=c(yminLim,ymaxLim))+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Elevation change (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.4<-SETplot.unit+
      ggtitle(paste(refugeName," SET data (n = ", nStations,")",sep=""))

    print(plot.4)

    #save figure
    myFilepath<-paste(getwd(), "Results","Data_Summary_Results","Refuge_Summary",refugeName,sep="/")
    dir.create(myFilepath)
    ggsave(plot.4, filename=paste(refugeName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)
    ggsave(plot.4, filename=paste(refugeName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)

  }

  write.csv(unit.delta.SET, file=paste(getwd(), "Results","Data_Summary_Results", paste("All_NWRs", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)
  
  
  ###################################################################
  #get estimates of delta SET by Site
  message("Saving delta height (mm) estimates for each Site and generating plots.")

  site.delta.SET<-list()
  for(i in 1:length(siteList)){

    new.data<-subset(set.data, Site_Name==siteList[i])
    siteName<-unique(as.character(new.data$Site_Name))
    refugeName<-unique(as.character(new.data$RefugeName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(new.dataIn=new.data)
    new.summary<-data.frame("State"=stateName[1],"RefugeName"=refugeName,"Site_Name"=siteName, new.summary)


    dir.create(paste(getwd(), "Results","Data_Summary_Results","Site_Summary",sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Site_Summary",refugeName,sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Site_Summary",refugeName,siteName,sep="/"))

    write.csv(new.summary, file=paste(getwd(), "Results","Data_Summary_Results","Site_Summary",refugeName,siteName, paste(siteName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    site.delta.SET<-rbind(site.delta.SET, new.summary)

    #plot regional estimates
    nStations=length(unique(new.data$Plot_Name))

    yminLim<-ifelse(min(new.summary$mean-new.summary$SE)<5,
                    min(new.summary$mean-new.summary$SE)-25,
                    min(new.summary$mean-new.summary$SE)-30)
    #yminLim<-min(new.summary$mean-new.summary$SE)*1.01
    
    ymaxLim<-ifelse(max(new.summary$mean+new.summary$SE)==0,
                    max(new.summary$mean+new.summary$SE)+10,
                    max(new.summary$mean+new.summary$SE)*1.7)
    
    #now plot unit results
    SETplot.site<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white",fill=alpha("seagreen4",0.7))+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0, size=0.6, color=alpha("seagreen4",0.7))+
      geom_line(color="seagreen4",linetype=1, size=1)+
      geom_point(size=2, color="seagreen4")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      scale_y_continuous(expand=c(0,0), limits=c(0-ymaxLim, ymaxLim)) +
      coord_cartesian(xlim=c(min(as.numeric(new.summary$year.visit)),max(as.numeric(new.summary$year.visit))), ylim=c(yminLim,ymaxLim))+
      #ylim(min(new.summary$mean-new.summary$SE)-30,max(new.summary$mean+new.summary$SE)+30)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Elevation change (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.5<-SETplot.site+
      ggtitle(paste(siteName," SET data (n = ", nStations,")",sep=""))

    print(plot.5)

    #save figure
    myFilepath<-paste(getwd(), "Results","Data_Summary_Results","Site_Summary",refugeName,siteName,sep="/")
    ggsave(plot.5, filename=paste(siteName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)


  write.csv(site.delta.SET, file=paste(getwd(), "Results","Data_Summary_Results", paste("All_Sites", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)
}
  ###################################################################
  #get estimates of delta SET by Station
  message("Saving delta height (mm) estimates for each Station (Plot_Name) and generating plots.")

  station.delta.SET<-list()
  for(i in 1:length(stationList)){

    new.data<-subset(set.data, Plot_Name==stationList[i])
    stationName<-unique(as.character(new.data$Plot_Name))
    siteName<-unique(as.character(new.data$Site_Name))
    refugeName<-unique(as.character(new.data$RefugeName))

    new.summary<-summaryDelta(new.dataIn=new.data)
    new.summary<-data.frame("State"=stateName[1],"RefugeName"=refugeName,"Site_Name"=siteName,"Plot_Name"=stationName, new.summary)

    #create folders
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Station_Summary",sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName,sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName,siteName,sep="/"))
    dir.create(paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName,siteName,stationName,sep="/"))

    write.csv(new.summary, file=paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName,siteName,stationName, paste(stationName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    station.delta.SET<-rbind(station.delta.SET, new.summary)
    
    yminLim<-ifelse(min(new.summary$mean-new.summary$SE)<5,
                    min(new.summary$mean-new.summary$SE)-25,
                    min(new.summary$mean-new.summary$SE)-30)
    #yminLim<-min(new.summary$mean-new.summary$SE)*1.01
    
    ymaxLim<-ifelse(max(new.summary$mean+new.summary$SE)==0,
                    max(new.summary$mean+new.summary$SE)+10,
                    max(new.summary$mean+new.summary$SE)*1.7)
    

    #now plot unit results
    SETplot.station<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white", fill=alpha("goldenrod3",0.3))+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0, size=0.6, color=alpha("goldenrod3",0.7))+
      geom_line(color="goldenrod3",linetype=1, size=1)+
      geom_point(size=2, color="goldenrod3")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      scale_y_continuous(expand=c(0,0), limits=c(0-ymaxLim, ymaxLim)) +
      coord_cartesian(xlim=c(min(as.numeric(new.summary$year.visit)),max(as.numeric(new.summary$year.visit))), ylim=c(yminLim,ymaxLim))+
      #ylim(min(new.summary$mean-new.summary$SE)-30,max(new.summary$mean+new.summary$SE)+30)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Elevation change (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.6<-SETplot.station+
      ggtitle(paste(stationName," SET data",sep=""))

    print(plot.6)

    #save figure
    myFilepath<-paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName,siteName,stationName,sep="/")
    ggsave(plot.6, filename=paste(stationName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)

    #############################################################################
    #create plots of pin-level linear models (to look at slopes)

    #Rcolorbrewer function
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))

    PinRegressionPlot<-ggplot(new.data)+
      aes(x=Year, y=value)+
      #geom_errorbar(ymax=summary$mean+summary$SE, ymin=summary$mean-summary$SE, width=0.1, size=0.5, color=I("grey50"))+
      geom_line(aes(group=variable, color=variable),linetype=2, size=0.5, alpha=0.3)+
      geom_smooth(method="lm",aes(group=variable,color=variable),se=FALSE)+
      geom_point(size=2, aes(color=variable),alpha=0.7)+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      #theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust=0.2, size=12, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
      #scale_color_manual("Pin", values=getPalette(9))+
      #scale_fill_manual("Pin", values=getPalette(9))+
      #scale_x_continuous(limits=c(2004,2010),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
      ylab("Elevation change (mm)")

    plot.7<-PinRegressionPlot+facet_grid(Plot_Name~PipePosition)+
      theme(panel.spacing = unit(1, "lines"))+
      ggtitle(paste(stationName, " SET pin-level linear regression (Position x Station)"))

    print(plot.7)

    #save figure
    myFilepath<-paste(getwd(), "Results","Data_Summary_Results","Station_Summary",refugeName, siteName, stationName,sep="/")
    ggsave(plot.7, filename=paste(stationName, "_Pin_slope_year.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)


  }

  write.csv(station.delta.SET, file=paste(getwd(), "Results","Data_Summary_Results", paste("All_Stations", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #generate small multiple plot for each refuge of each station
  
  
  #get trend (positive, negative, neutral)
  

  trend.save<-list()
  for(k in 1:length(stationList)){
    
    stationData<-subset(station.delta.SET, Plot_Name==stationList[k])
    mod<-NULL
    mod<-lm(mean~as.numeric(year.visit), data=stationData)
    slope<-mod$coefficients[2]
    
    summary.mod<-summary(mod)
    
    mod.stats<-as.data.frame(summary.mod$coefficients)
    pVal<-mod.stats$`Pr(>|t|)`[2]
    
    Sig<-ifelse(pVal<0.05, "Significant","Not_significant" )
    
    stationData<-data.frame(stationData, "Slope"=slope, "P_value"=pVal,"Sig"=Sig)
    
    stationData$Trend<-if(stationData$Sig =="Significant" & stationData$Slope>0){
      "Positive"
    }else{
      if(stationData$Sig =="Significant" & stationData$Slope<0){
        "Negative"
      }else{
        "No Trend"
      }
    }
    
    trend.save<-rbind(trend.save, stationData)
    
  }
  

  yminLim<-ifelse(min(trend.save$mean-trend.save$SE)<5,
                  min(trend.save$mean-trend.save$SE)-25,
                  min(trend.save$mean-trend.save$SE)-30)
  #yminLim<-min(new.summary$mean-new.summary$SE)*1.01
  
  ymaxLim<-ifelse(max(trend.save$mean+trend.save$SE)==0,
                  max(trend.save$mean+trend.save$SE)+10,
                  max(trend.save$mean+trend.save$SE)*1.7)
  
  #reorcer factor levels of year.visit
  year.visit.order<-as.character(sort(as.numeric(as.character(unique(trend.save$year.visit)))))
  trend.save$year.visit<-factor(trend.save$year.visit, levels=year.visit.order)
  
  levels(trend.save$year.visit)
  
  #convert year.visit to numeric
  #trend.save$year.visit<-as.numeric(as.character(trend.save$year.visit))

  #now plot unit results
  all.station.plots<-ggplot(trend.save)+
    aes(x=year.visit, y=mean, group=RefugeName)+
    geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white", aes(fill=RefugeName))+
    geom_errorbar(ymax=trend.save$mean+trend.save$SE, ymin=trend.save$mean-trend.save$SE, width=0, size=0.6, aes(color=RefugeName))+
    geom_line(aes(color=RefugeName),linetype=1, size=1)+
    geom_point(size=2, aes(color=RefugeName))+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=7, color="black"),
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    #scale_y_continuous(expand=c(0,0), limits=c(0-ymaxLim, ymaxLim)) +
    #coord_cartesian(xlim=c(min(as.numeric(trend.save$year.visit)),max(as.numeric(trend.save$year.visit))), ylim=c(yminLim,ymaxLim))+
    #ylim(min(trend.save$mean-trend.save$SE)-30,max(trend.save$mean+trend.save$SE)+30)+
    #xlim(200.8,2014.2)
    #scale_x_continuous(limits=c(2010.01,2019.01),breaks=c(2010,2013,2016,2019), "Year")+
    #scale_x_discrete(limits=c("2010.01","2019.01"),labels=c("2010","2014","2015"), "Year")+
    labs(x="Year (visits)", y="Elevation change (mm)")
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.8<- all.station.plots+
    ggtitle("USFWS Region 4 SET data by station")
  
  print(plot.8)
  
  unique(station.delta.SET$Plot_Name)
  

  stationPlotFacet<-plot.8+facet_wrap(.~Plot_Name, ncol=5, scales="free_y")
  stationPlotFacet
  
  ggsave(stationPlotFacet, filename=paste("Small_Multiples", "_delta_SET_year_visit.png",sep=""), path=myFilepath, width=12,height=14, limitsize=FALSE)
  
##########################################################################################
#color plot by trend
  
  myColors<-c("royalblue3",I("gray30"),"red")
  
  #order Trend factor
  trend.save$Trend<-factor(trend.save$Trend, levels = c("Positive","No Trend","Negative"))
  levels(trend.save$Trend)
  
  #now plot unit results
  trendPlot<-ggplot(trend.save)+
    aes(x=year.visit, y=mean, group=Trend)+
    geom_hline(yintercept = 0, color=I("gray10"), linetype=2, alpha=0.4)+
    geom_smooth(method=lm,fullrange=FALSE, linetype=1, color="white", aes(fill=Trend))+
    geom_errorbar(ymax=trend.save$mean+trend.save$SE, ymin=trend.save$mean-trend.save$SE, width=0, size=0.6, aes(color=Trend))+
    geom_line(aes(color=Trend),linetype=1, size=1)+
    geom_point(size=2, aes(color=Trend))+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=8, color="black"),
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13),
          plot.title=element_text(size=22),
          plot.subtitle = element_text(size=18))+
    scale_color_manual(values=myColors)+
    scale_fill_manual(values=myColors)+
    #scale_y_continuous(expand=c(0,0), limits=c(0-ymaxLim, ymaxLim)) +
    #coord_cartesian(xlim=c(min(as.numeric(trend.save$year.visit)),max(as.numeric(trend.save$year.visit))), ylim=c(yminLim,ymaxLim))+
    #ylim(min(trend.save$mean-trend.save$SE)-30,max(trend.save$mean+trend.save$SE)+30)+
    #xlim(200.8,2014.2)
    #scale_x_continuous(limits=c(2010.01,2019.01),breaks=c(2010,2013,2016,2019), "Year")+
    labs(x="Year (visits)", y="Elevation change (mm)")
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.9<- trendPlot+
    ggtitle("USFWS Region 4 SET data by station",
            subtitle="Linear trends shown in blue indicate significance (P < 0.05). Note differing scales on y-axes.")
  
  print(plot.9)
  
  trendPlotFacet<-plot.9+facet_wrap(.~Plot_Name, ncol=5, scales="free_y")
  trendPlotFacet
  
  ggsave(trendPlotFacet, filename=paste("Trends_Small_Multiples", "_delta_SET_year_visit.png",sep=""), path=myFilepath, width=17,height=14, limitsize=FALSE)
  
  
  
}
