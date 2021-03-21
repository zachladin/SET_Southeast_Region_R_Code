#function to produce maps of SET trends (requires internet conncection for basemaps)

getMaps<-function(dataIn){

  message("Generating and saving maps for all SET stations.")
  sub.data.1<-dataIn

  #remove NAs
  sub.data<-sub.data.1[is.na(sub.data.1$var)==FALSE,]

  #Add Trend column
  sub.data$Trend<-ifelse(sub.data$mean<0,"Negative",
                            ifelse(sub.data$mean>0,"Positive","Zero"))
  sub.data$Trend<-as.factor(as.character(sub.data$Trend))
  sub.data$Longitude<-as.numeric(as.character(sub.data$Longitude))
  sub.data$Latitude<-as.numeric(as.character(sub.data$Latitude))

  #change column header names of data
  colnames(sub.data)<-c("State","RefugeName","Site_Name","Plot_Name","Latitude","Longitude","n","Mean","Var","SD","SE","CV","lwr","upr","Model","Trend")

  new.slope.data<-sub.data

  maxSize<-max(new.slope.data$Mean,na.rm=TRUE)
  minSize<-min(new.slope.data$Mean,na.rm=TRUE)

  #get extent
  minLon=min(new.slope.data$Longitude,na.rm=TRUE)
  maxLon=max(new.slope.data$Longitude,na.rm=TRUE)
  minLat=min(new.slope.data$Latitude,na.rm=TRUE)
  maxLat=max(new.slope.data$Latitude,na.rm=TRUE)

  bbox.1<-c(left=minLon-1, bottom=minLat-1,right=maxLon+1,top=maxLat+1)

  #get base map (requires internet connection)
  map.1 <- get_map(location = bbox.1,maptype="toner-lite")
  #ggmap(map.1)

  #set point colors
  #mapColors<-c("red","royalblue1","goldenrod2")
  
  mapColors<-if(length(unique(new.slope.data$Trend))== 2){
    c("red","royalblue1")
  }else{
    if(unique(new.slope.data$Trend) == "Positive"){
      c("royalblue1")
    }else{
      c("red")
    }}
    
  nStations<-length(unique(new.slope.data$Plot_Name))

  map.all<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
    geom_point(data=new.slope.data,stat="identity",aes(x=Longitude, y=Latitude, color=Trend,size=abs(Mean)),alpha=0.6)+
    #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
    scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
    guides(alpha=FALSE)+
    labs(x="Longitude",y="Latitude")+
    ggtitle(paste("SET stations (n = ",nStations,")",sep=""))+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
    scale_radius(guide=guide_legend(title="Magnitude:\nMean delta height\n(mm/year)",order=2),range=c(1,4))
  map.all

  print(map.all)

  #create folder to save results
  dir.create(paste(getwd(), "Results","Figures","Maps", sep="/"))

  #save Map
  ggsave(map.all, filename="SET_trend_map.png",path=paste(getwd(),"Results","Figures","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)


  #get refugeList
  refugeList<-sort(as.character(unique(sub.data.1$RefugeName)))
  #refugeList<-c("MEC","RHC","PKR","SPT","JHC","OYS","WRT","EBF","BMH","PMH", "ESV","BKB")

  #produce refuge maps
  message("Generating and saving refuge maps.")
  for(i in 1:length(refugeList)){
    print(refugeList[i])
    map1<-NULL
    sub.data<-subset(new.slope.data, RefugeName==refugeList[i])

    refugeName<-as.character(unique(sub.data$RefugeName))

    maxSize<-max(sub.data$Mean,na.rm=TRUE)
    minSize<-min(sub.data$Mean,na.rm=TRUE)

    nStations<-length(unique(sub.data$Plot_Name))
    
    coord.data<-sub.data[is.finite(sub.data$Mean),]

    #get extent
    minLon=min(sub.data$Longitude,na.rm=TRUE)
    
    maxLon=max(sub.data$Longitude,na.rm=TRUE)
    minLat=min(sub.data$Latitude,na.rm=TRUE)
    maxLat=max(sub.data$Latitude,na.rm=TRUE)

    bbox.1<-c(left=minLon-0.1, bottom=minLat-0.1,right=maxLon+0.1,top=maxLat+0.1)

    #get base map
    map.1 <- get_map(location = bbox.1,maptype="toner-lite")
    #ggmap(map.1)

    #set point colors
    mapColors<-if(length(unique(sub.data$Trend))== 2){
      c("red","royalblue1")
    }else{
      if(unique(sub.data$Trend) == "Positive"){
        c("royalblue1")
      }else{
        c("red")
      }}
    
    map1<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
      geom_point(data=sub.data,stat="identity",aes(x=Longitude, y=Latitude, color=Trend, size=abs(Mean)),alpha=0.6)+
      #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
      scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
      guides(alpha=FALSE)+
      labs(x="Longitude",y="Latitude")+
      ggtitle(paste("SET stations in ",refugeName," (n = ",nStations,")",sep=""))+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
      scale_radius(guide=guide_legend(title="Magnitude:\nMean delta height\n(mm/year)",order=2),range=c(1,4))
    #map1
    #map.out<-map1+facet_wrap(~Model)
    print(map1)


    ggsave(map1, filename=paste(refugeName,"_SET_trend_map.png",sep=""),path=paste(getwd(),"Results","Figures","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)
    #cat("\n\n\\pagebreak\n")
  }



}