#function to produce maps of SET trends (requires internet conncection for basemaps)

getMapsBayes<-function(dataIn){

  message("Generating and saving maps for all SET stations.")
  sub.data.1<-dataIn


  #remove NAs
  sub.data<-sub.data.1[is.na(sub.data.1$Mean)==FALSE,]

  #Add Trend column
  sub.data$Trend<-ifelse(sub.data$Mean<0,"Negative",
                            ifelse(sub.data$Mean>0,"Positive","Zero"))
  sub.data$Trend<-as.factor(as.character(sub.data$Trend))
  sub.data$Long<-as.numeric(as.character(sub.data$Long))
  sub.data$Lat<-as.numeric(as.character(sub.data$Lat))

  #change column header names of data
  colnames(sub.data)<-c("State","Unit_Code","Site_Name","Plot_Name","Lat","Long","Mean","SD","Naive.SE","Time.series.SE","Model","Trend")
  #names(sub.data)
  new.slope.data<-sub.data

  maxSize<-max(new.slope.data$Mean,na.rm=TRUE)
  minSize<-min(new.slope.data$Mean,na.rm=TRUE)

  #get extent
  minLon=min(new.slope.data$Long,na.rm=TRUE)
  maxLon=max(new.slope.data$Long,na.rm=TRUE)
  minLat=min(new.slope.data$Lat,na.rm=TRUE)
  maxLat=max(new.slope.data$Lat,na.rm=TRUE)

  bbox.1<-c(left=minLon-1, bottom=minLat-1,right=maxLon+1,top=maxLat+1)

  #get base map (requires internet connection)
  map.1 <- get_map(location = bbox.1,maptype="toner-lite")
  #ggmap(map.1)

  #set point colors
  mapColors<-c("coral3","limegreen","goldenrod2")

  nStations<-length(unique(new.slope.data$Plot_Name))

  map.all<-ggmap(map.1, extent = "panel", maprange=FALSE,alpha=0.8)+
    geom_point(data=new.slope.data,stat="identity",aes(x=Long, y=Lat, color=Trend,size=abs(Mean)),alpha=0.6)+
    #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
    scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
    guides(alpha=FALSE)+
    labs(x="Longitude",y="Latitude")+
    ggtitle(paste("SET stations (n = ",nStations,")",sep=""))+
    theme()+
    theme(panel.border = element_rect(color = "black", fill=NA, linetype="solid",size=1))+
    scale_radius(guide=guide_legend(title="Magnitude:\nMean delta height\n(mm/year)",order=2),range=c(1,4))
  map.all

  print(map.all)

  #save Map
  dir.create(paste(getwd(), "Results","Results_Bayes","Figures",sep="/"))
  dir.create(paste(getwd(), "Results","Results_Bayes","Figures","Maps",sep="/"))

  ggsave(map.all, filename="SET_trend_map.png",path=paste(getwd(),"Results","Results_Bayes","Figures","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)

  #refugeList<-sort(unique(as.character(slopes.obs.out$Unit_Code)))
  #hard code list ordered by Latitude
  refugeList<-c("MEC","RHC","PKR","SPT","JHC","OYS","WRT","EBF","BMH","PMH", "PMH_shallow","ESV","BKB")


  #produce refuge maps
  message("Generating and saving refuge maps.")
  for(i in 1:length(refugeList)){

    map1<-NULL
    sub.data<-subset(new.slope.data, Unit_Code==refugeList[i])

    refugeName<-refugeList[i]

    maxSize<-max(sub.data$Mean,na.rm=TRUE)
    minSize<-min(sub.data$Mean,na.rm=TRUE)

    nStations<-length(unique(sub.data$Plot_Name))

    #get extent
    minLon=min(sub.data$Long,na.rm=TRUE)
    maxLon=max(sub.data$Long,na.rm=TRUE)
    minLat=min(sub.data$Lat,na.rm=TRUE)
    maxLat=max(sub.data$Lat,na.rm=TRUE)

    bbox.1<-c(left=minLon-0.1, bottom=minLat-0.1,right=maxLon+0.1,top=maxLat+0.1)

    #get base map
    map.1 <- get_map(location = bbox.1,maptype="toner-lite")
    #ggmap(map.1)

    #set point colors
    mapColors<-c("salmon3","limegreen","goldenrod4")

    map1<-ggmap(map.1, extent = "panel", maprange=FALSE,alpha=0.8)+
      geom_point(data=sub.data,stat="identity",aes(x=Long, y=Lat, color=Trend, size=abs(Mean)),alpha=0.6)+
      #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
      scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
      guides(alpha=FALSE)+
      labs(x="Longitude",y="Latitude")+
      ggtitle(paste("SET stations in ",refugeName," (n = ",nStations,")",sep=""))+
      theme()+
      theme(panel.border = element_rect(colour = "black", linetype="solid", fill=NA, size=1))+
      scale_radius(guide=guide_legend(title="Magnitude:\nMean delta height\n(mm/year)",order=2),range=c(1,4))
    map1
    #map.out<-map1+facet_wrap(~Model)
    print(map1)

    ggsave(map1, filename=paste(refugeName,"_SET_trend_map.png",sep=""),path=paste(getwd(),"Results","Results_Bayes","Figures","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)
    #cat("\n\n\\pagebreak\n")
  }



}