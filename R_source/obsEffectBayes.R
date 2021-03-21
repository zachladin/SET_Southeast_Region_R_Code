#ObsEffect function to see how observer effects lead to changes in slope estimates.
obsEffectBayes<-function(dataIn_1, dataIn_2){

  new.slopes<-dataIn_1
  #new.slopes$Model<-"No_Observer"


  new.slopes.obs<-dataIn_2
  #new.slopes.obs$Model<-"Observer"

  #########################################################################
  #add slopes.obs mean variance estimates

  #merge slopes and slopes.obs
  slopes.obs.out.merge<-merge(new.slopes, new.slopes.obs, by="Plot_Name")

  slopes.obs.out<-slopes.obs.out.merge[,c("State.x","RefugeName.x","Site_Name.x","Plot_Name","Latitude.x","Longitude.x",
                                          "n.x","Mean.x","SD.x","Naive.SE.x","Time.series.SE.x","Model.x",
                                          "Mean.y","SD.y","Naive.SE.y","Time.series.SE.y","Model.y")]
  

  colnames(slopes.obs.out)<-c("State","RefugeName","Site_Name","Plot_Name","Latitude","Longitude","n",
                              "Mean","SD","Naive.SE","Time.series.SE","Model",
                              "Mean.obs","SD.obs","Naive.SE.obs","Time.series.SE.obs","Model.obs")

  #combine obs and noObs data
  message("Combining data and calculating differences in slopes due to Observer effects.")
  slopes.freq.1<-slopes.obs.out

  #remove na
  slopes.freq<-slopes.freq.1[is.na(slopes.freq.1$Mean)==FALSE,]

  #########################################################################
  #compute difference between slopes without Observer and with Observer effects

  #first create columns for normalized mean slopes
  #head(slopes.freq)

  slopes.freq$meanNorm<-(slopes.freq$Mean-min(slopes.freq$Mean))/(max(slopes.freq$Mean)-min(slopes.freq$Mean))
  slopes.freq$mean.obsNorm<-(slopes.freq$Mean.obs-min(slopes.freq$Mean.obs))/(max(slopes.freq$Mean.obs)-min(slopes.freq$Mean.obs))

  #compute difference between noObs and Obs normalized (0-1) slopes
  slopes.freq$DiffObs<-slopes.freq$meanNorm-slopes.freq$mean.obsNorm

  #########################################################################
  #plot SET rates to visualize observer effects

  #refugeList<-sort(unique(as.character(slopes.obs.out$RefugeName)))
  #hard code list ordered by Latitude
  refugeList<-sort(unique(as.character(slopes.obs.out$RefugeName)))


  #stack data
  slopes.freq.2<-rbind(new.slopes, new.slopes.obs)

  message("Generating and saving change in marsh elevation rate plots.")
  for(i in 1:length(refugeList)){
    sub.slope<-subset(slopes.freq.2, RefugeName==refugeList[i])

    #sort data by DiffObs for plotting
    sub.slope<-sub.slope[order(sub.slope$Mean),]

    sub.slope$Plot_Name <- factor(sub.slope$Plot_Name, levels = unique(sub.slope$Plot_Name[order(sub.slope$Mean)]))

    refugeName<-as.character(unique(sub.slope$RefugeName))

    minSET<-round(min(sub.slope$Mean, na.rm=TRUE)-10,0)
    maxSET<-round(max(sub.slope$Mean, na.rm=TRUE)+10,0)


    setPlot1<-ggplot(data=sub.slope)+
      coord_flip()+
      aes(x=Plot_Name, y=Mean, ymin=Mean-Time.series.SE, ymax=Mean+Time.series.SE, reorder(Plot_Name,Mean))+
      geom_hline(yintercept=0,lty=2,color="darkgray")+
      geom_bar(stat="identity", position=position_dodge(),aes(fill=Model),size=2,alpha=0.9)+
      geom_errorbar(width=0.1, size=0.5, position=position_dodge(width=0.95),aes(color=Model))+
      scale_fill_manual(values=c("coral3","seagreen"),guide=guide_legend(title="Model\nCovariates"))+
      scale_color_manual(values=c("coral3","seagreen"),guide=guide_legend(title="Model\nCovariates"))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=1.2, size=12, color="black"),
            axis.text.y = element_text(angle=0,size=10, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(minSET,maxSET)+
      labs(x="SET station",y="Rate of salt marsh\nelevation change (mm/year)")+
      ggtitle(paste("Comparison of modeled observer effects on\n change in salt marsh elevation (mm/year) at" ,refugeName))

    print(setPlot1)

    #create folder to save results
    dir.create(paste(getwd(), "Results","Results_Bayes","Figures","Observer_Effects", sep="/"))
    dir.create(paste(getwd(), "Results","Results_Bayes","Figures","Observer_Effects",refugeName, sep="/"))

    ggsave(setPlot1, filename=paste(refugeName,"Observer_effects_Values_Bayesian.png",sep="_"),path=paste(getwd(),"Results","Results_Bayes","Figures","Observer_Effects",refugeName,sep="/"), width=9,height=6.5, limitsize=FALSE)

    #cat("\n\n\\pagebreak\n")

  }



  #########################################################################
  #plot differences to visualize observer effects

  minDiff<-round(min(slopes.freq$DiffObs, na.rm=TRUE)-0.1,1)
  maxDiff<-round(max(slopes.freq$DiffObs, na.rm=TRUE)+0.1,1)

  message("Generating and saving plots.")
  for(i in 1:length(refugeList)){
    sub.slope<-subset(slopes.freq,RefugeName==refugeList[i])

    #sort data by DiffObs for plotting
    sub.slope<-sub.slope[order(sub.slope$DiffObs),]

    sub.slope$Plot_Name <- factor(sub.slope$Plot_Name, levels = unique(sub.slope$Plot_Name[order(sub.slope$DiffObs)]))

    refugeName<-unique(sub.slope$RefugeName)

    diffPlot1<-ggplot(data=sub.slope)+
      coord_flip()+
      aes(x=Plot_Name, y=DiffObs, reorder(Plot_Name,DiffObs))+
      geom_hline(yintercept=0,lty=2,color="darkgray")+
      geom_bar(stat="identity", aes(fill=DiffObs),size=2,alpha=0.9)+
      #scale_fill_continuous(low="#F00000",mid="#FFCB00", high="#0000FF",limits=c(-4,4), guide=guide_colorbar(title="Observer effect"))+
      scale_fill_gradient2(low="#F00000",mid="#FFCB00", high="#0000FF",midpoint=0, limits=c(minDiff,maxDiff),guide=guide_colorbar(title="Observer\neffect"))+
      #scale_fill_gradientn(colors=myColors,guide=colorbar(title="Observer effect"))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=1.2, size=12, color="black"),
          axis.text.y = element_text(angle=0,size=10, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(minDiff,maxDiff)+
      labs(x="SET station",y="Difference in normalized SET station slope")+
      ggtitle(paste("Observer effects on SET station mean delta height (mm/year) at" ,refugeName))

    print(diffPlot1)

    ggsave(diffPlot1, filename=paste(refugeName,"Observer_effects_difference_Bayesian.png",sep="_"),path=paste(getwd(),"Results","Results_Bayes","Figures","Observer_Effects",refugeName,sep="/"), width=9,height=6.5, limitsize=FALSE)
    #cat("\n\n\\pagebreak\n")

  }
}