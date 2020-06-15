#Format SET data function SETformat for Region 4
formatSETdataR4<-function(dataIn, dataDir){

  myDir<-as.character(dataDir)
  
  
  message("Reading raw data in.")
  new.data<-dataIn
  
  #remove PinHeight_mm = NA
  #new.data<-subset(new.data, !is.na(PinHeight_mm)
  
  #look at data
  head(new.data)
  
  #look at column header names
  names(new.data)
  ################################
  #are there double observers?  Yes, however, it appears the same observer made two measurements during a single sampling event, so these can't help us estimate observer-based measurement bias).
  
  #new.data$plot.date<-paste(new.data$Plot_Name, new.data$EventDate, sep="_")
  #doubObs<-as.data.frame(table(new.data$plot.date, new.data$ReaderFullName))
  #doubObsSub<-subset(doubObs, Freq !=0)
  
  #new.data$plot.date<-as.character(new.data$plot.date)
  
  #subset
  #new.data.sub<-subset(new.data, ReaderFullName=="Nicole Rankin")
  ################################
  
  #remove all OberservationTypeCode != R
  new.data.sub<-subset(new.data, ObservationTypeCode != "R")
  
  #Remove rows if they contain NAs or blank cells.
  data.1<-subset(new.data.sub, EventDate != "")
  data.1<-na.omit(data.1)

  #Remove double observers (all where Contact_Order==2) add Contact_Order as column or if there is no Contact_Orer==2, set it all to 1
  #data.1<-subset(data.1, Contact_Order==1)
  data.1$Contact_Order <- 1
  
  #separate columns for arm position and pin
  # position_pin_sep<-read.table(text=as.character(data.1$PinPosition),sep="-")
  # colnames(position_pin_sep)<-c("Position","Pin")
  # position_pin_sep$Pin<-paste("Pin",position_pin_sep$Pin, sep="")
  # 
  # #add columns
  # data.1<-cbind(data.1, position_pin_sep)
  
  #create plot.event.position column
  position.df<-as.data.frame(unique(data.1[, c("Plot_Name","EventDate","PipeDirectionCode")]))
  
  #get plotList
  plotList<-unique(position.df$Plot_Name)
  
  positionFactor.save<-list()
  for(i in 1:length(plotList)){
    position.df.sub<-subset(position.df, Plot_Name==plotList[i])
    
    #get unique postions
    unique.positions<-unique(position.df.sub$PipeDirectionCode)
    
    positionFactor.df<-data.frame("PipeDirectionCode" =unique.positions, "Position_Name"=c("A","B","C","D"))
    
    #merge
    position.df.merge<-merge(position.df.sub, positionFactor.df, by="PipeDirectionCode",all.x=TRUE)
    
    positionFactor.save<-rbind(positionFactor.save, position.df.merge)
    
  }
  
  #now merge back with data.1
  data.1<-merge(data.1, positionFactor.save, by=c("Plot_Name","EventDate","PipeDirectionCode"), all.x=TRUE)
  
  #change Position to character string
  data.1$PipeDirectionCode<-as.character(data.1$PipeDirectionCode)

  #change Pin to character string
  data.1$PinPosition<-as.character(data.1$PinPosition)
  
  #Add Year, Month, and Day columns to data.
  message("Adding year, month, and day columns to data.")
  date.data<-data.frame(EventDate=data.1$EventDate)
  date.data$EventDate<-as.character(date.data$EventDate)
  date.string<-read.table(text=as.character(date.data$EventDate), sep="/",colClasses = "character")
  colnames(date.string)<-c("Month","Day","Year")
  date.string$Year<-as.factor(date.string$Year)

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
  #write.csv(data.2,file=paste(myDir,"Data","New_Formatted_SET_data.csv" , sep="/") , row.names=FALSE)

  #####################################################################################################

  #Fix names that can be confusing when defining file paths, by replacing any "/" with "-" to not confuse with filepath definitions
    #data.2$Plot_Name<-gsub("/","-",as.character(data.2$Plot_Name))
  
  #Create new columns for "station.position.year", "station.year", and "station.year.day", using paste() function.
  data.2$station.position.year<-paste(data.2$Plot_Name, data.2$PipeDirectionCode, data.2$Year,sep=".")
  data.2$station.year<-as.factor(paste(data.2$Plot_Name, data.2$Year,sep="."))
  data.2$station.year.ord.day<-as.factor(paste(data.2$Plot_Name, data.2$Year, data.2$ord.Day,sep="."))
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
  data.1.events<-data.1.events[order(data.1.events$Plot_Name, data.1.events$Year, data.1.events$ord.Day)]
  
  #Sequentially order visits by a factor. NEED TO CHECK THIS
  data.1.events[, Visit := 1:.N, by = station.year]
  
  #Convert back to data.frame
  data.1.events<-as.data.frame(data.1.events)

  #Take a peek at first 6 rows.
  #head(data.1.events)

  #Now merge visits back with data.
  data.1.events<-data.1.events[,c("station.year.ord.day","Visit")]
  data.1.merge<-merge(data.2,data.1.events, by="station.year.ord.day",all.x=TRUE)

  #Take a look.
  head(data.1.merge)

  #Redefine data as 'data.out'.
  data.out<-as.data.frame(data.1.merge)
  
  #create two-digit number for zero (if nchar=1, prepend zero)
  data.out$VisitPad<-ifelse(nchar(as.character(data.out$Visit))<2, 
                              paste("0",as.character(data.out$Visit),sep=""),
                              as.character(data.out$Visit))
  
  
  
  data.out$year.visit<-paste(data.out$Year, data.out$VisitPad,sep=".")
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
  message("Checking to ensure SET arm data looks good.")
  check.arms<-aggregate(PipeDirectionCode~Plot_Name, data=data.out.keep, FUN=function(x){length(unique(x))})
  check.arms$all4<-ifelse(check.arms[,2]==4,"Yes","NO")
  
  #remove FC02 due to missing data
  #data.out.keep<-subset(data.out.keep, Plot_Name!="FC02")
  #data.out.keep<-data.out.keep[complete.cases(data.out.keep),]
  
  names(data.out.keep)
  
  unique(data.out.keep$Refuge)
  
  unique(data.out.keep$RefugeName)
  
  #fix NWR names
  #remove "National Wildlife Refuge" from RefugeName
  data.out.keep$RefugeName<-trimws(gsub("National Wildlife Refuge","",gsub("National Wildlife Refuges","",data.out.keep$Refuge)))
  
  #remove "NWR" from Site_Name
  data.out.keep$Site_Name<-trimws(gsub("NWR","",data.out.keep$Site_Name))
  
  
  ########################################################################################################
  #Use package 'reshape' to reorganize data (like Pivot table in Excel) using melt().
  message("Computing elevation change (mm) for SET data. . .")
  #melt function .
    data.melt<-reshape2::melt(data.out.keep, id=c("RefugeName","Plot_Name","ReaderFullName",
                                 "Year","PipeDirectionCode","Position_Name","PinPosition","Visit","year.visit"),
                    measure=c("PinHeight_mm"))

    # colnames(data.melt)<-c("Plot_Name","ReaderFullName","Year","PipeDirectionCode","Visit","year.visit","Pin","Value")

  ###################################################################
    #Generate lists 
      
  
    #Get stationList. (FYI, using the "<<-" sign puts object into the Global Environment).
    stationList<-unique(as.character(data.melt$Plot_Name))

    #Get yearList from data.
    yearList<-unique(as.character(data.melt$Year))

    #Get visitList from data.
    visitList<-unique(as.character(data.melt$Visit))

    #Get positionList.
    positionList<-unique(as.character(data.melt$Position_Name))

    #Get pinList from data.
    pinList<-unique(as.character(data.melt$Pin))

    #Create deltaPinList.
    deltaPinList<-c("deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6","deltaPin7","deltaPin8","deltaPin9")
  
    ###################################################################
  
    #Compute change in marsh height between ti and t0
    station.out<-list()
    for(k in 1:length(stationList)){
      #print(stationList[k])
      station.data<-subset(data.melt, Plot_Name==stationList[k])
      station.name<-unique(as.character(station.data$Plot_Name))
      
      position.out<-list()
      for(j in 1:length(positionList)){
        position.data<-NULL
        position.data<-subset(station.data, Position_Name==positionList[j])
        position.name<-unique(as.character(position.data$Position_Name))
        #position.data$year.visit<-as.numeric(position.data$year.visit)
        
          #now cast
          cast.data<-cast(position.data, PinPosition~year.visit+variable, fun.aggregate="max")
          
          #get length of total visits
          nVisits<-length(colnames(cast.data))-1
          
          #now get deltas
          all.delta.list<-list()
          for (i in 1:9){
            #print(i)
            sub.data<-cast.data[i,-1]

            delta.list<-list()
            for(p in 1:nVisits){
              #print(p)
              delta.list[p]<-sub.data[,p] - sub.data[,1]
            }
            
            delta.out<-as.data.frame(delta.list)
            colnames(delta.out)<-colnames(sub.data)
          
          all.delta.list<-rbind(all.delta.list, delta.out)
          }
          
          #transpose raw values
          cast.data.trans<-as.data.frame(as.matrix(t(cast.data)))
          
          #transpose deltas
          all.delta.trans<-as.data.frame(t(all.delta.list))
          
          #add pin column headers back in
          colnames(all.delta.trans)<-c("deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6","deltaPin7", "deltaPin8","deltaPin9")
          
          pin.df<-cbind(cast.data.trans, all.delta.trans)
          
          #add year.visit back in 
          pin.df$year.visit<-gsub("_PinHeight_mm","",row.names(pin.df))
          row.names(pin.df)<-NULL
          
          #add position back in 
          pin.df$Position_Name<-position.name
          
          #gather deltas for each postion
          position.out<-rbind(position.out, pin.df)
          
        }
          
          #add station name
        position.out$Plot_Name<-station.name
        
        #gather station deltas
        station.out<-rbind(station.out, position.out)
        
    }       
###################################################################
#combine deltas with all data
station.out$Plot_Year_Visit_Position<-paste(station.out$Plot_Name, station.out$year.visit, station.out$Position_Name, sep=".")
#remove columns
station.out$year.visit<-NULL
station.out$Plot_Name<-NULL
station.out$Position_Name<-NULL

#trim data.out.keep to include only needed columns
names(data.out.keep)

#need to merge with event data 
data.out.keep$Plot_Year_Visit_Position<-paste(data.out.keep$Plot_Name, data.out.keep$year.visit, data.out.keep$Position_Name, sep=".")

data.out.keep.1<-unique(data.out.keep[,c("RefugeName","Site_Name","Plot_Name","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionCode","Position_Name","Plot_Year_Visit_Position")])

new.data.out.1<-unique(merge(station.out, data.out.keep.1, by=c("Plot_Year_Visit_Position"),all.x=TRUE))

#write.csv(new.data.out.1, file=paste(myDir, "Data", "new.SET.data.melt.csv",sep="/"),row.names=FALSE)
###################################################################
#Read in R4_SET to get location info

#add Lat/Long if needed to rawData
#SETcoords.1<-read.csv(file=paste(myDir,"SET_Station_coords_all.csv",sep="/"), header=TRUE)
SETcoords.1<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/SET_USFWS_Region_4/Reports/RMarkdown_html/Data/SET_Station_coords_all.csv", header=TRUE)

SETstates<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/SET_USFWS_Region_4/Reports/RMarkdown_html/Data/R4_SET.csv",header=TRUE)

SETstates.sub<-unique(SETstates[,c("State","Refuge")])

#merge with SETcoords
SETcoords<-merge(SETcoords.1, SETstates.sub,by="Refuge",all.x=TRUE)

R4_coords<-subset(SETcoords, RegionNumber==4)
R4_coords.sub<-unique(R4_coords[,c("State","StationName","StationLatitude","StationLongitude")])
colnames(R4_coords.sub)<-c("State","Plot_Name","Latitude","Longitude")

#Merge new.data.out.1 and Region.lookup.
#new.data.out.2<-merge(new.data.out.1, Region.lookup, by="Unit_Code",all.x=TRUE)
new.data.out.2<-new.data.out.1

  #Add Longitude and Latitude coords for making maps.
  message("Adding State and Lat/Long coords for SET stations to data.")

  XYdata<-R4_coords.sub

  #Merge new.data.out with XYdata.
  data.out.3<-merge(new.data.out.2, XYdata, by="Plot_Name",all.x=TRUE)
  
  #Save data (as a .csv file).
  message("Saving SET with delta height data to .csv file.")
  write.csv(data.out.3, file=paste(myDir,"Data",paste(paste("R4_SET_data_formatted",sep=""),"csv",sep="."),sep="/"),row.names=FALSE)

  ####################################################################
  #Create and save melted data.frame.

  #Redefine data as 'set.data'
  set.data<-as.data.frame(data.out.3)
  
  #create Date column
  set.data$Date<-as.Date(set.data$EventDate, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"))
  
  set.delta.data<-set.data[,c("State","RefugeName","Site_Name","Plot_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionCode","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude","deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6", "deltaPin7","deltaPin8","deltaPin9")]

  #Rename columns.
  colnames(set.delta.data)<-c("State","RefugeName","Site_Name","Plot_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionCode","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude","Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9")
  
  #Melt data to stack all Pins (Pin1, Pin2, . . .) in one column.
  delta.set.melt<-reshape2::melt(set.delta.data, id=c("State","RefugeName","Site_Name","Plot_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionCode","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude"),
                     measure=c("Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9"))

  #Add pos.pin column using paste()
  delta.set.melt$pos.pin<-paste(delta.set.melt$Position_Name, delta.set.melt$variable,sep="_")

#convert -Inf and Inf to NAs
  delta.set.melt$value<-ifelse(delta.set.melt$value==-Inf,NA,
                               ifelse(delta.set.melt$value==Inf,NA,
                                      delta.set.melt$value))

  
  #Save file.
  message("Finalizing data and saving file.")
  write.csv(delta.set.melt, file=paste(myDir,"Data",paste(paste("R4_SET_formatted_data.melt",sep="_"),"csv",sep="."),sep="/"),row.names=FALSE)

  return(delta.set.melt)

}