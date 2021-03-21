#Format SET data function SETformat for Region 4
formatSETdataR4<-function(dataIn, dataDir,IncludeUncorrected=TRUE){

  myDir<-as.character(dataDir)
  
  message("Reading raw data in.")
  new.data<-dataIn
  
  #remove (or retain) records with NAs for pin heights
  ifelse(isTRUE(IncludeUncorrected),
                   new.data<-new.data,
                   new.data<-subset(new.data, c(! is.na(PinHeight_mm) & ! is.na(PinHeight_mm_Uncorrected)))
                   )
  
  
  #change Station_Name to Station_Name
  names(new.data)[names(new.data)=="Plot_Name"]<-"Station_Name"
  
  #look at data
  #head(new.data)
  
  #look at column header names
  #names(new.data)
  
  #unique(new.data$DataProcessingLevelCode)
  ################################
  #are there double observers?  Yes, however, it appears the same observer made two measurements during a single sampling event, so these can't help us estimate observer-based measurement bias).
  
  #new.data$plot.date<-paste(new.data$Station_Name, new.data$EventDate, sep="_")
  doubObs<-aggregate(ReaderFullName~Station_Name+EventDate, data=new.data, FUN=length)
  #check if there are cases with > 36 observations (i.e., 4 arms X 9 pins = 36), if so indicates two observers, typically.
  doubObsSub<-subset(doubObs, ReaderFullName >36)
  
  ################################
  #Issue found on 11-24-2020 where replicate rows have different DataLevelProcessingCode factor levels, so they are being included within the data as independent records.  Fixing this now in the formatSETdataR4 funciton.
  
  #get list of replicits
  replicateList<-paste(doubObsSub$Station_Name, doubObsSub$EventDate,sep="_")

  #get these rows that have apparent replicated rows with different DataLevelProcessingCodes
  new.data$Station_Event<-paste(new.data$Station_Name, new.data$EventDate,sep="_")
  replicate.data<-subset(new.data, Station_Event %in% replicateList)

  replicate.data.sub<-subset(replicate.data, ObservationTypeCode != "R")

  ################################
  
  #remove all Raw "R" and "P" data from 
  #replicate.data.sub.accepted<-subset(replicate.data.sub, DataProcessingLevelCode == "A")
  
  #or use lax version including all dataProcessing levels
  ifelse(isTRUE(IncludeUncorrected),
         replicate.data.sub.accepted<-replicate.data.sub,
         replicate.data.sub.accepted<-subset(replicate.data.sub, DataProcessingLevelCode == "A"))

  #remove any replicate rows
  replicate.data.sub.accepted.2<-unique(replicate.data.sub.accepted)
  
  #prepare table for export as .csv for including in Appendix (all records with replicates)
  replicate.data.out<-unique(replicate.data.sub.accepted.2[,c("Refuge","Site_Name","Station_Name","EventDate","ReaderFullName")])
  
  #save as .csv
  write.csv(replicate.data.out, file=paste(paste(myDir,"Data","Tables",sep="/"),"Table_records_with_replicates.csv",sep="/"),row.names=FALSE)
  
  #################################################################################################
  #create and save table of data omitted from Analysis: Provisional and Raw data types
  
  #remove all "R"eplicate observations
  new.data.1<-subset(new.data, ObservationTypeCode !="R")
  
  #subset all "P"rovisional and "R"aw data types 
  #data.omitted<-subset(new.data.1, c(DataProcessingLevelCode != "A"))
  
  #or use lax version including all dataProcessing levels
  ifelse(isTRUE(IncludeUncorrected),
         {
         data.omitted<-data.frame(matrix(ncol=6))
         colnames(data.omitted)<-c("Refuge","Site_Name","Station_Name","EventDate","ReaderFullName","DataProcessingLevelCode")
         },
         data.omitted<-subset(new.data.1, c(DataProcessingLevelCode != "A")))
  
  
  #simplify
  data.omitted.out<-unique(data.omitted[,c("Refuge","Site_Name","Station_Name","EventDate","ReaderFullName","DataProcessingLevelCode")])
  
  #save as .csv
  write.csv(data.omitted.out, file=paste(paste(myDir,"Data","Tables",sep="/"),"Table_records_omitted_from_anlaysis.csv",sep="/"),row.names=FALSE)
  
  ############################################################################################
  #create table of all stations with info and Pipe Azimuth directions
  
  #get all "A"ccepted data to use in analysis
  all.stations.pipe.dirs<-unique(new.data.1[,c("Refuge","Site_Name","Station_Name", "PipeDirectionAzimuth")])
  
  #add factor for pipe direction for each SET
  all.stations.pipe.dirs$Station_Name<-as.factor(all.stations.pipe.dirs$Station_Name)

  #convert to data.table
  all.stations.pipe.dirs.dt<-all.stations.pipe.dirs
  setDT(all.stations.pipe.dirs.dt)   ## change format
  
  all.stations.pipe.dirs.dt[, PipeDirectionCode:=1:.N, by=Station_Name]
  
  #make PipeDirectionCode a factor
  all.stations.pipe.dirs.dt$PipeDirectionCode<-as.factor(all.stations.pipe.dirs.dt$PipeDirectionCode)
  
  #melt and cast data to create column with list of PipeAzimuth directions
  all.stations.pipe.melt<-data.table::melt(all.stations.pipe.dirs.dt, id.vars=c("Refuge","Site_Name","Station_Name","PipeDirectionCode"), measure.vars=c("PipeDirectionAzimuth"))
  
  all.stations.pipe.cast<-as.data.frame(data.table::dcast(Refuge+Site_Name+Station_Name~PipeDirectionCode, data=all.stations.pipe.melt, value.var="value"))
  

  #Alligator River (ALL030C) has two sets of PipeDirectionAzimuths: 
  #167, 252, 351, 75, / 170, 260, 350, 80
  
  #make columns of pipe directions
  all.stations.pipe.cast$PipeDirectionAzimuths<-paste(all.stations.pipe.cast$'1', all.stations.pipe.cast$'2', all.stations.pipe.cast$'3', all.stations.pipe.cast$'4',sep=", ")

  #simplify
  all.stations.pipe.out<-all.stations.pipe.cast[,c("Refuge","Site_Name","Station_Name", "PipeDirectionAzimuths")]
  
  #This won't effect lax data inclusion 
  # ifelse(isTRUE(IncludeUncorrected),
  #        
  #        {
  #          #make columns of pipe directions
  #          all.stations.pipe.cast$PipeDirectionAzimuths_1<-paste(all.stations.pipe.cast$'1', all.stations.pipe.cast$'2', all.stations.pipe.cast$'3', all.stations.pipe.cast$'4',sep=", ")
  #          
  #          all.stations.pipe.cast$PipeDirectionAzimuths_2<-ifelse(! is.finite(all.stations.pipe.cast$'5'), NA,paste(all.stations.pipe.cast$'5', all.stations.pipe.cast$'6', all.stations.pipe.cast$'7', all.stations.pipe.cast$'8',sep=", "))
  #          
  #          #simplify
  #          all.stations.pipe.out<-all.stations.pipe.cast[,c("Refuge","Site_Name","Station_Name", "PipeDirectionAzimuths_1","PipeDirectionAzimuths_2")]
  #        },
  #        {
  #        #make columns of pipe directions
  #        all.stations.pipe.cast$PipeDirectionAzimuths<-paste(all.stations.pipe.cast$'1', all.stations.pipe.cast$'2', all.stations.pipe.cast$'3', all.stations.pipe.cast$'4',sep=", ")
  #        
  #        #simplify
  #        all.stations.pipe.out<-all.stations.pipe.cast[,c("Refuge","Site_Name","Station_Name", "PipeDirectionAzimuths")]
  #        }
  #       )
  
  #save csv file
  write.csv(all.stations.pipe.out, file=paste(paste(myDir,"Data","Tables",sep="/"),"Table_all_SET_pipe_directions.csv",sep="/"),row.names=FALSE)
  
  ############################################################################################
  #create a table of all records with out Uncorrected or Corrected Pin Heights
  
  #get a list of data with no Corrected pin heights entered.
  uncorrected.df<-subset(new.data, is.na(PinHeight_mm))
  
  missing.pin.df<-subset(new.data, c(is.na(PinHeight_mm) | is.na(PinHeight_mm_Uncorrected)))
  
  #simplify
  missing.pin.df.sub<-unique(missing.pin.df[,c("Refuge","Site_Name","Station_Name","EventDate","PipeDirectionAzimuth","PinPosition","PinLength_mm","PinLength_mm_Baseline",
                                               "PinHeight_mm_Uncorrected", "PinHeight_mm")])
  
  #save csv file
  write.csv(missing.pin.df.sub, file=paste(paste(myDir,"Data","Tables",sep="/"),"Table_all_SET_missing_height_data.csv",sep="/"),row.names=FALSE)
  
  
  ############################################################################################
  #get all "A"ccepted data to use in analysis
  #data.use<-subset(new.data.1, c(DataProcessingLevelCode == "A"))
  
  #or use lax version including all dataProcessing levels
  ifelse(isTRUE(IncludeUncorrected),
         data.use<-new.data.1,
         data.use<-subset(new.data.1, c(DataProcessingLevelCode == "A")))
  
  
   #create plot.event.position column
  position.df<-as.data.frame(unique(data.use[, c("Station_Name","EventDate","PipeDirectionAzimuth")]))
  
  
  ifelse(position.df$Station_Name=="ALL030C",
         {
         position.df$PipeDirectionAzimuth[position.df$PipeDirectionAzimuth == 170] <- 167
         position.df$PipeDirectionAzimuth[position.df$PipeDirectionAzimuth == 80] <-  75
         position.df$PipeDirectionAzimuth[position.df$PipeDirectionAzimuth == 260] <- 252
         position.df$PipeDirectionAzimuth[position.df$PipeDirectionAzimuth == 350] <- 351
         },
        ifelse(position.df$Station_Name=="POC016B",
               position.df$PipeDirectionAzimuth[position.df$PipeDirectionAzimuth == 252] <- 250,
               position.df$PipeDirectionAzimuth<-position.df$PipeDirectionAzimuth)
               

  )
  
  #find NAs in data.use$PipeDirectionAzimuth
  pipe.dir.NA<-subset(position.df, is.na(PipeDirectionAzimuth))
  
  ifelse(data.use$Station_Name=="ALL030C",
         {
           data.use$PipeDirectionAzimuth[data.use$PipeDirectionAzimuth == 170] <- 167
           data.use$PipeDirectionAzimuth[data.use$PipeDirectionAzimuth == 80] <-  75
           data.use$PipeDirectionAzimuth[data.use$PipeDirectionAzimuth == 260] <- 252
           data.use$PipeDirectionAzimuth[data.use$PipeDirectionAzimuth == 350] <- 351
         },
         ifelse(data.use$Station_Name=="POC016B",
                data.use$PipeDirectionAzimuth[data.use$PipeDirectionAzimuth == 252] <- 250,
                data.use$PipeDirectionAzimuth<-data.use$PipeDirectionAzimuth)
         
         
  )
  
  
  
  #get stationList
  stationList<-unique(position.df$Station_Name)
  
  positionFactor.save<-list()
  for(i in 1:length(stationList)){
    
    #print(i)
    
    position.df.sub<-subset(position.df, Station_Name==stationList[i])
    
    #get unique postions
    unique.positions<-unique(position.df.sub$PipeDirectionAzimuth)
    
    #get length of unique positions
    count.positions<-length(unique.positions)
    
    if(count.positions != 4){
           position.df.sub<-subset(position.df.sub, PipeDirectionAzimuth %in% c(unique.positions[1:4]))
}
    #check unique postions
    #unique.positions.chk<-unique(position.df.sub$PipeDirectionAzimuth)

    positionFactor.df<-data.frame("PipeDirectionAzimuth" =unique.positions, "Position_Name"=c("A","B","C","D"))
    
    #merge
    position.df.merge<-merge(position.df.sub, positionFactor.df, by="PipeDirectionAzimuth",all.x=TRUE)
    
    positionFactor.save<-rbind(positionFactor.save, position.df.merge)
    
  }
  
  #now merge back with data.1
  data.1<-merge(data.use, positionFactor.save, by=c("Station_Name","EventDate","PipeDirectionAzimuth"), all.x=TRUE)
  
  unique(positionFactor.save$Position_Name)
  
  #change Position_Name to character string
  data.1$Position_Name<-as.character(data.1$Position_Name)

  #change PinPosition to character string
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
  
  #####################################################################################################

  #Fix names that can be confusing when defining file paths, by replacing any "/" with "-" to not confuse with filepath definitions
    #data.2$Station_Name<-gsub("/","-",as.character(data.2$Station_Name))
  
  #Create new columns for "station.position.year", "station.year", and "station.year.day", using paste() function.
  data.2$station.position.year<-paste(data.2$Station_Name, data.2$Position_Name, data.2$Year,sep=".")
  data.2$station.year<-as.factor(paste(data.2$Station_Name, data.2$Year,sep="."))
  data.2$station.year.ord.day<-as.factor(paste(data.2$Station_Name, data.2$Year, data.2$ord.Day,sep="."))
  #View first few rows of data.frame.
  head(data.2)

  #Create column for visits by each factor level of SET stations (Station_Name).
  #Use 'data.table' package to add sequential visit numbers by each factor (i.e., Station_Name)
  #Truncate data.frame
  data.1.events<-data.2[,c("Station_Name","Year","Month","Day","ord.Day","station.year","station.year.ord.day")]

  #Convert from data.frame to data.table
  data.1.events<- as.data.table(unique(data.1.events))

  #Get the maximum number of visits to each SET per year.
  max.visits<-max(table(data.1.events$Station_Name, data.1.events$Year))
  max.visits

  #order
  data.1.events$Station_Name<-as.character(data.1.events$Station_Name)
  data.1.events<-data.1.events[order(data.1.events$Station_Name, data.1.events$Year, data.1.events$ord.Day)]
  
  #Sequentially order visits by a factor. NEED TO CHECK THIS
  data.1.events[, Visit := 1:.N, by = station.year]
  
  #Convert back to data.frame
  data.1.events<-as.data.frame(data.1.events)

  #Take a peek at first 6 rows.
  #head(data.1.events)

  #Now merge visits back with data.
  data.1.events<-data.1.events[,c("Station_Name","Year","Month","Day","ord.Day","Visit")]
  data.1.merge<-merge(data.2,data.1.events, by=c("Station_Name","Year","Month","Day","ord.Day"),all.x=TRUE)

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
  #create table with # of visits per SET per year
  
  data.out.sub<-unique(data.out[,c("Refuge","Site_Name","Station_Name","Year","Month","Day","EventDate","Visit","year.visit","station.year","station.year.ord.day")])
  
  num.visits.agg<-aggregate(EventDate~Refuge+Site_Name+Station_Name+Year, FUN="length", data=data.out.sub)
  
  #melt and cast
  
  ###################################
  #convert to data.table
  num.visits.agg.dt<-num.visits.agg
  setDT(num.visits.agg.dt)   ## change format
  
  #melt
  num.visits.melt<-data.table::melt(num.visits.agg.dt,id.vars=c("Refuge","Site_Name","Station_Name","Year"),measure.vars=c("EventDate")
  )
  
  #cast
  num.visits.cast<-as.data.frame(data.table::dcast(Refuge+Site_Name+Station_Name~Year, data=num.visits.melt, value.var="value"))
  
  #save csv file
  write.csv(num.visits.cast, file=paste(paste(myDir,"Data","Tables",sep="/"),"Table_all_SET_num_visits_per_year.csv",sep="/"),row.names=FALSE)
  
  ################################################################################
  #check that each station has more than 1 visit
  message("Checking data for inconsistencies.")
  check.data<-aggregate(year.visit~Station_Name, data=data.out, FUN=function(x){length(unique(x))})
  check.data$enoughVisits<-ifelse(check.data[,2]>1,"Yes","NO")
  
  #remove any plots with only 1 year.visit
  check.data.keep<-check.data[check.data$enoughVisits!="NO",]
  
  #subset data out by list of check.data.keep
  keepPlots<-unique(as.character(check.data.keep$Station_Name))
  
  data.out$Station_Name<-as.character(data.out$Station_Name)
  data.out.keep<-data.out[data.out$Station_Name %in% keepPlots,]
  data.out.keep$Station_Name<-as.factor(as.character(data.out.keep$Station_Name))
  
  #check if each has all arm data
  message("Checking to ensure SET arm data looks good.")
  check.arms<-aggregate(Position_Name~Station_Name, data=data.out.keep, FUN=function(x){length(unique(x))})
  check.arms$all4<-ifelse(check.arms[,2]==4,"Yes","NO")
  
  #fix NWR names
  #remove "National Wildlife Refuge" from Refuge
  data.out.keep$Refuge<-trimws(gsub("National Wildlife Refuge","",gsub("National Wildlife Refuges","",data.out.keep$Refuge)))
  
  #remove "NWR" from Site_Name
  data.out.keep$Site_Name<-trimws(gsub("NWR","",data.out.keep$Site_Name))
  
  ################################################################################
  #now move pin height data from 'PinHeight_mm_Uncorrected" to 'PinHeight_mm' columns
  
  
  #or use lax version use all Pin height data (uncorrected and corrected)
  ifelse(isTRUE(IncludeUncorrected),
           data.out.keep$PinHeight_mm<-ifelse(is.na(data.out.keep$PinHeight_mm),
                                              data.out.keep$PinHeight_mm_Uncorrected,
                                              data.out.keep$PinHeight_mm),
          data.out.keep<-data.out.keep)
  
  
  
  
  ################################################################################
  #Use package 'data.table' (reshape is depreciated) to reorganize data (like Pivot table in Excel) using melt().
  message("Computing elevation change (mm) for SET data. . .")
  #melt function .
  
  #convert to data.table
  data.out.keep.dt<-data.out.keep
  setDT(data.out.keep.dt)
  
  unique(data.out.keep.dt$Position_Name)
  
  #find data.out.keep.dt NAs position name
  postionName.NA<-subset(data.out.keep.dt, is.na(Position_Name))
  
    data.melt<-data.table::melt(data.out.keep.dt, id=c("Refuge","Station_Name","ReaderFullName","Year","Position_Name","PipeDirectionAzimuth","PinPosition","Visit","year.visit"),measure=c("PinHeight_mm"))
  ###################################################################
    #Generate lists 
      
  
    #Get stationList. (FYI, using the "<<-" sign puts object into the Global Environment).
    stationList<-unique(as.character(data.melt$Station_Name))

    #Get yearList from data.
    yearList<-unique(as.character(data.melt$Year))

    #Get visitList from data.
    visitList<-unique(as.character(data.melt$Visit))

    #Get positionList.
    positionList<-unique(as.character(data.melt$Position_Name))
    
    #check how many NAs
    positionNAlist<-subset(data.melt, is.na(Position_Name))

    #Get pinList from data.
    pinList<-unique(as.character(data.melt$PinPosition))

    #Create deltaPinList.
    deltaPinList<-c("deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6","deltaPin7","deltaPin8","deltaPin9")
  
    ###################################################################
  
    #Compute change in marsh height between ti and t0
    station.out<-list()
    for(k in 1:length(stationList)){
      #print(stationList[k])
      station.data<-subset(data.melt, Station_Name==stationList[k])
      station.name<-unique(as.character(station.data$Station_Name))
      
      position.out<-list()
      for(j in 1:length(positionList)){
        position.data<-NULL
        position.data<-subset(station.data, Position_Name==positionList[j])
        position.name<-unique(as.character(position.data$Position_Name))
        #position.data$year.visit<-as.numeric(position.data$year.visit)
        
        #convert to data.table 
        position.data.dt<-position.data
        setDT(position.data.dt)
        
          #now cast
          cast.data<-as.data.frame(data.table::dcast(position.data.dt, PinPosition~year.visit+variable, value.var="value"))
          
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
          cast.data.trans<-as.data.frame(as.matrix(t(cast.data)))[-1,]
          
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
        position.out$Station_Name<-station.name
        
        #gather station deltas
        station.out<-rbind(station.out, position.out)
        
    }       
###################################################################
#combine deltas with all data
station.out$Plot_Year_Visit_Position<-paste(station.out$Station_Name, station.out$year.visit, station.out$Position_Name, sep=".")
#remove columns
station.out$year.visit<-NULL
station.out$Station_Name<-NULL
station.out$Position_Name<-NULL

#trim data.out.keep to include only needed columns
names(data.out.keep)

#need to merge with event data 
data.out.keep$Plot_Year_Visit_Position<-paste(data.out.keep$Station_Name, data.out.keep$year.visit, data.out.keep$Position_Name, sep=".")

data.out.keep.1<-unique(data.out.keep[,c("Refuge","Site_Name","Station_Name","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionAzimuth","Position_Name","Plot_Year_Visit_Position")])

new.data.out.1<-unique(merge(station.out, data.out.keep.1, by=c("Plot_Year_Visit_Position"),all.x=TRUE))

#write.csv(new.data.out.1, file=paste(myDir, "Data", "new.SET.data.melt.csv",sep="/"),row.names=FALSE)
###################################################################
#Read in R4_SET to get location info

#add Lat/Long if needed to rawData
#SETcoords.1<-read.csv(file=paste(myDir,"SET_Station_coords_all.csv",sep="/"), header=TRUE)
SETcoords.1<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/SET_USFWS_Region_4/Reports/RMarkdown_pdf/Data/SET_Station_coords_all.csv", header=TRUE)

#Bring in State field (add To do list)
SETstates<-read.csv(file="/Users/zach/Dropbox (ZachTeam)/Projects/SET_USFWS_Region_4/Reports/RMarkdown_pdf/Data/R4_SET.csv",header=TRUE)

SETstates.sub<-unique(SETstates[,c("State","Refuge")])

#merge with SETcoords
SETcoords<-merge(SETcoords.1, SETstates.sub,by="Refuge",all.x=TRUE)

R4_coords<-subset(SETcoords, RegionNumber==4)
R4_coords.sub<-unique(R4_coords[,c("State","StationName","StationLatitude","StationLongitude")])
colnames(R4_coords.sub)<-c("State","Station_Name","Latitude","Longitude")

#Merge new.data.out.1 and Region.lookup.
#new.data.out.2<-merge(new.data.out.1, Region.lookup, by="Unit_Code",all.x=TRUE)
new.data.out.2<-new.data.out.1

  #Add Longitude and Latitude coords for making maps.
  message("Adding State and Lat/Long coords for SET stations to data.")

  XYdata<-R4_coords.sub

  #Merge new.data.out with XYdata.
  data.out.3<-merge(new.data.out.2, XYdata, by="Station_Name",all.x=TRUE)
  
  #Save data (as a .csv file).
  message("Saving SET with delta height data to .csv file.")
  write.csv(data.out.3, file=paste(myDir,"Data",paste(paste("All_R4_SET_data_formatted_",Sys.Date(),sep=""),"csv",sep="."),sep="/"),row.names=FALSE)

  ####################################################################
  #Create and save melted data.frame.

  #Redefine data as 'set.data'
  set.data<-as.data.frame(data.out.3)
  
  #create Date column
  set.data$Date<-as.Date(set.data$EventDate, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"))
  
  set.delta.data<-set.data[,c("State","Refuge","Site_Name","Station_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionAzimuth","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude","deltaPin1","deltaPin2","deltaPin3","deltaPin4","deltaPin5","deltaPin6", "deltaPin7","deltaPin8","deltaPin9")]

  #Rename columns.
  colnames(set.delta.data)<-c("State","Refuge","Site_Name","Station_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionAzimuth","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude","Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9")
  
  #convert to data.table
  set.delta.data.dt<-set.delta.data
  setDT(set.delta.data.dt)
  
  #Melt data to stack all Pins (Pin1, Pin2, . . .) in one column.
  delta.set.melt.dt<-data.table::melt(set.delta.data.dt, id.vars=c("State","Refuge","Site_Name","Station_Name","Date","EventDate","Year","Month","Day","ord.Day", "Visit","year.visit","ReaderFullName","ReaderID","PipeDirectionAzimuth","Position_Name","Plot_Year_Visit_Position","Longitude","Latitude"),
                                 measure.vars=c("Pin1","Pin2","Pin3","Pin4","Pin5","Pin6","Pin7","Pin8","Pin9"))
  
  #convert back to data.frame
  delta.set.melt<-as.data.frame(delta.set.melt.dt)
  
  #Add pos.pin column using paste()
  delta.set.melt$pos.pin<-paste(delta.set.melt$Position_Name, delta.set.melt$variable,sep="_")

#convert -Inf and Inf to NAs
  delta.set.melt$value<-ifelse(delta.set.melt$value==-Inf,NA,
                               ifelse(delta.set.melt$value==Inf,NA,
                                      delta.set.melt$value))

  
  #Save file.
  message("Finalizing data and saving file.")
  write.csv(delta.set.melt, file=paste(myDir,"Data",paste(paste("R4_SET_formatted_data.melt", Sys.Date(),sep="_"),"csv",sep="."),sep="/"),row.names=FALSE)

  return(delta.set.melt)

}