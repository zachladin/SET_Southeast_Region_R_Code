#Format SET data function SETformat for Region 4
formatSETdataR4<-function(dataIn){

  message("Reading raw data in.")
  new.data<-dataIn

  
  #look at data
  head(new.data)
  
  
  
  # #remove any "Shallow" SET data
  # new.data.2<-subset(new.data, c(Plot_Name != "PH2-1 (Shallow)" | 
  #                                  Plot_Name != "PH2-1 (Shallow)" |
  #                                  Plot_Name != "PH2-2 (Shallow)" |
  #                                  Plot_Name != "PH2-3 (Shallow)" |
  #                                  Plot_Name != "PH2-4 (Shallow)" |
  #                                  Plot_Name != "PH2-5 (Shallow)" |
  #                                  Plot_Name != "PH2-6 (Shallow)" |
  #                                  Plot_Name != "Set 1 (Shallow)" |
  #                                  Plot_Name != "Set 2 (Shallow)" |
  #                                  Plot_Name != "Set 3 (Shallow)"))
  
  
  #Remove rows if they contain NAs or blank cells.
  data.1<-subset(new.data, Start_Date != "")
  data.1<-na.omit(data.1)

  #Remove double observers (all where Contact_Order==2) add Contact_Order as column or if there is no Contact_Orer==2, set it all to 1
  #data.1<-subset(data.1, Contact_Order==1)
  data.1$Contact_Order <- 1
  
  #change SiteName to Site_Name coliumn header
  names(data.1)[names(data.1)=="SiteName"]<-"Site_Name"

  #Add Year, Month, and Day columns to data.
  message("Adding year, month, and day columns to data.")
  date.data<-data.frame(Start_Date=data.1$Start_Date)
  date.data$Start_Date<-as.character(date.data$Start_Date)
  date.string<-read.table(text=as.character(date.data$Start_Date), sep="/",colClasses = "character")
  colnames(date.string)<-c("Month","Day","Year")
  date.string$Year<-as.factor(paste("20",date.string$Year,sep=""))

  date.string$new.date<-as.Date(as.character(paste(date.string$Year,
                                                   date.string$Month,
                                                   date.string$Day,sep="-"),
                                             format = "%Y-%b-%d"))
  #Make column for ordinal day.
  date.string$ord.Day <- as.integer(format(date.string$new.date, "%j"))

  #Compile data
  data.2<-cbind(data.1,date.string)

  #Save data here, if you want to.
  #message("Saving formatted SET data.")
  write.csv(data.2,file=paste(getwd(),"Data","Formatted_SET_data.csv" , sep="/") , row.names=FALSE)

  ########################################################################################################
  #Read in data and standardize.

  #read in formatted data
  data.1<-read.csv(paste(getwd(),"Data","Formatted_SET_data.csv" , sep="/"),header=TRUE)
  data.2<-data.1

  
  #Fix names that can be confusing when defining file paths, by replacing any "/" with "-" to not confuse with filepath definitions
  data.2$Plot_Name<-gsub("/","-",as.character(data.2$Plot_Name))
  
  #Create new columns for "station.position.year", "station.year", and "station.year.day", using paste() function.
  data.2$station.position.year<-paste(data.2$Plot_Name, data.2$Position,data.2$Year,sep=".")
  data.2$station.year<-as.factor(paste(data.2$Plot_Name,data.2$Year,sep="."))
  data.2$station.year.ord.day<-as.factor(paste(data.2$Plot_Name, data.2$Year,data.2$ord.Day,sep="."))
  #View first few rows of data.frame.
  head(data.2)

  #Create column for visits by each factor level of SET stations (Plot_Name).
  #Use 'data.table' package to add sequential visit numbers by each factor (i.e., Plot_Name)
  #Truncate data.frame
  data.1.events<-data.2[,c("Plot_Name","Year","Month","Day","ord.Day","station.year","station.year.ord.day")]

  #Convert from data.frame to data.table
  data.1.events<- as.data.table(unique(data.1.events))

  #Get the maximum number of visits to each SET per year.
  max.visits<-max(table(data.1.events$station.year, data.1.events$Year))
  max.visits

  #order
  data.1.events$Plot_Name<-as.character(data.1.events$Plot_Name)
  data.1.events<-data.1.events[with(data.1.events, order(Plot_Name, Year, ord.Day)),]
  
  #Sequentially order visits by a factor.
  data.1.events[, Visit := 1:max.visits, by = station.year]

  #Convert back to data.frame
  data.1.events<-as.data.frame(data.1.events)

  #Take a peek at first 6 rows.
  head(data.1.events)

  #Now merge visits back with data.
  data.1.events<-data.1.events[,c("station.year.ord.day","Visit")]
  data.1.merge<-merge(data.2,data.1.events, by="station.year.ord.day",all.x=TRUE)

  #Take a look.
  head(data.1.merge)

  #Redefine data as 'data.out'.
  data.out<-data.1.merge
  data.out$year.visit<-paste(data.out$Year, data.out$Visit,sep=".")
  head(data.out)

  ########################################################################################################
  #check that each station has more than 1 visit
  message("Checking data for inconsistencies.")
  check.data<-aggregate(year.visit~Plot_Name, data=data.out, FUN=function(x){length(unique(x))})
  check.data$enoughVisits<-ifelse(check.data[,2]>1,"Yes","NO")
  
  #remove any plots with only 1 year.visit
  check.data.keep<-check.data[check.data$enoughVisits!="NO",]
  
  #subset data out by list of check.data.keep
  keepPlots<-unique(as.character(check.data.keep$Plot_Name))
  
  data.out$Plot_Name<-as.character(data.out$Plot_Name)
  data.out.keep<-data.out[data.out$Plot_Name %in% keepPlots,]
  data.out.keep$Plot_Name<-as.factor(as.character(data.out.keep$Plot_Name))
  
  #check if each has all arm data
  check.arms<-aggregate(Position_Name~Plot_Name, data=data.out.keep, FUN=function(x){length(unique(x))})
  check.arms$all4<-ifelse(check.arms[,2]==4,"Yes","NO")
  
  #remove FC02 due to missing data
  data.out.keep<-subset(data.out.keep, Plot_Name!="FC02")
  data.out.keep<-data.out.keep[complete.cases(data.out.keep),]
  
  
  ########################################################################################################
  #Use package 'reshape' to reorganize data (like Pivot table in Excel) using melt().
  message("Computing elevation change (mm) for SET data.")
  #melt function .
    data.melt<-reshape2::melt(data.out.keep, id=c("Plot_Name","Last_Name",
                                 "Year","Position_Name","Visit","year.visit"),
                    measure=c("Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7",
                              "Pin8","Pin9"))
    colnames(data.melt)<-c("plot.name","observer","year","position","visit","year.visit","pin","value")

  ###################################################################
    #Generate lists 
    
    #Get stationList. (FYI, using the "<<-" sign puts object into the Global Environment).
    stationList<-unique(as.character(data.melt$plot.name))

    #Get yearList from data.
    yearList<-unique(as.character(data.melt$year))

    #Get visitList from data.
    visitList<-unique(as.character(data.melt$visit))

    #Get positionList.
    positionList<-unique(as.character(data.melt$position))

    #Get pinList from data.
    pinList<-unique(as.character(data.melt$pin))

    #Create deltaPinList.
    deltaPinList<-c("deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6","deltaPin7","deltaPin8","deltaPin9")
  
    ###################################################################
  #Compute change in marsh height between ti and t0
    station.out<-list()
    for(k in 1:length(stationList)){
      station.data<-subset(data.melt, plot.name==stationList[k])
      station.name<-unique(as.character(station.data$plot.name))
      
      position.out<-list()
      for(j in 1:length(positionList)){
        position.data<-subset(station.data, position==positionList[j])
        position.name<-unique(as.character(position.data$position))

          #now cast
          cast.data<-cast(position.data, pin~year.visit, fun.aggregate="max")
          
          #get length of total visits
          nVisits<-length(colnames(cast.data))-1
          
          #now get deltas
          all.delta.list<-list()
          for (i in 1:9){
            sub.data<-cast.data[i,-1]

            delta.list<-list()
            for(p in 1:nVisits){
              delta.list[p]<-sub.data[,p] - sub.data[,1]
            }
            
            delta.out<-as.data.frame(delta.list)
            colnames(delta.out)<-colnames(sub.data)
          
          all.delta.list<-rbind(all.delta.list, delta.out)
          }
          
          #transpose
          all.delta.trans<-as.data.frame(t(all.delta.list))
          
          #add pin column headers back in
          colnames(all.delta.trans)<-c("deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6","deltaPin7",
                                "deltaPin8","deltaPin9")
          #add year.visit back in 
          all.delta.trans$year.visit<-row.names(all.delta.trans)
          row.names(all.delta.trans)<-NULL
          
          #add position back in 
          all.delta.trans$position<-position.name
          
          #gather deltas for each postion
          position.out<-rbind(position.out, all.delta.trans)
          
        }
          
          #add station name
        position.out$plot.name<-station.name
        
        #gather station deltas
        station.out<-rbind(station.out, position.out)
    }       
###################################################################
#combine deltas with all data
station.out$Plot_Year_Visit_Position<-paste(station.out$plot.name, station.out$year.visit, station.out$position, sep=".")
    
data.out.keep$Plot_Year_Visit_Position<-paste(data.out.keep$Plot_Name, data.out.keep$year.visit, data.out.keep$Position_Name, sep=".")

new.data.out.1<-unique(merge(station.out, data.out.keep, by=c("Plot_Year_Visit_Position"),all.x=TRUE))

write.csv(new.data.out.1, file=paste(getwd(), "Data", "new.SET.data.melt.csv",sep="/"),row.names=FALSE)
###################################################################
#Read in Region_NWR_code_lookup.csv (this file has a State for each SET station)
Region.lookup<-read.csv(paste(getwd(), "Data", "Region_Refuge_code_lookup.csv", sep="/"),header=TRUE)
head(Region.lookup)

#Rename 4th column in data "Unit_Code".
colnames(Region.lookup)[2]<-"Unit_Code"

#Merge new.data.out.1 and Region.lookup.
new.data.out.2<-merge(new.data.out.1, Region.lookup, by="Unit_Code",all.x=TRUE)

  #Add Longitude and Latitude coords for making maps.
  message("Adding Lat/Long coords for SET stations to data.")

  #Read in file with SET coords.
  gps.data<-read.csv(paste(getwd(),"Data","SETcoords.csv",sep="/"),header=TRUE)
  XYdata<-gps.data[,c("State","Refuge_Name","Plot_Name","Long", "Lat")]
  XYdata$Lat<-as.numeric(as.character(XYdata$Lat))
  XYdata$Long<-as.numeric(as.character(XYdata$Long))
  XYdata<-XYdata[complete.cases(XYdata),]
  head(XYdata)
  
  #test names
  # Plot_names_list_old<-sort(unique(as.character(new.data.out.2$Plot_Name)))
  # 
  # Plot_names_list_new<-sort(unique(as.character(XYdata$Plot_Name)))
  # 
  # differentNames<-setdiff(Plot_names_list_old, Plot_names_list_new)
  

  #Merge new.data.out with XYdata.
  data.out.3<-merge(new.data.out.2, XYdata, by="Plot_Name",all.x=TRUE)
  
  #fix names of duplicate columns
  names(data.out.3)[names(data.out.3) == "year.visit.x"] <- "year.visit"
  data.out.3$year.visit.y<-NULL
  names(data.out.3)[names(data.out.3) == "State.x"] <- "State"
  data.out.3$State.y<-NULL
  data.out.3$RefugeNameOrder<-NULL


  #Save data (as a .csv file).
  message("Saving SET with delta height data to .csv file.")
  write.csv(data.out.3, file=paste(getwd(),"Data",paste("All_SET_data_formatted","csv",sep="."),sep="/"),row.names=FALSE)

  ###################################################################
  #Create and save melted data.frame.

  #Redefine data as 'set.data'
  set.data<-as.data.frame(data.out.3)
  set.delta.data<-set.data[,c("State","Refuge_Name","Unit_Code","Site_Name","Plot_Name","Year","Day","Last_Name","Position_Name","Visit",
                            "station.position.year","station.year","year.visit","Long","Lat",
                            "deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6",
                            "deltaPin7","deltaPin8","deltaPin9")]

  #Rename columns.
  colnames(set.delta.data)<-c("State","RefugeName","Unit_Code","Site_Name","Plot_Name","Year","Day","Last_Name","Position_Name","Visit",
                            "station.position.year","station.year","year.visit","Long","Lat",
                            "Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9")
  #Melt data to stack all Pins (Pin1, Pin2, . . .) in one column.
  delta.set.melt<-reshape2::melt(set.delta.data, id=c("State","RefugeName","Unit_Code","Site_Name","Plot_Name","Year","Day","Last_Name","Position_Name","Visit",
                                          "station.position.year","station.year","year.visit","Long","Lat"),
                     measure=c("Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9"))

  #Add pos.pin column using paste()
  delta.set.melt$pos.pin<-paste(delta.set.melt$Position_Name, delta.set.melt$variable,sep="_")



  #Save file.
  message("Melting data and saving 'SET.delta.melt.csv' file.")
  write.csv(delta.set.melt, file=paste(getwd(),"Data",paste("SET.delta.melt","csv",sep="."),sep="/"),row.names=FALSE)


}