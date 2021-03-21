#function to get summary stats
summaryDelta<-function(new.dataIn){
  
  #remove Inf and -Inf values
  new.data<-new.dataIn[is.finite(new.dataIn$value),]
  
  delta.summary<-summaryFunction(dataIn=new.data, response="value",factor="year.visit")
  delta.summary$Year<-as.integer(substr(delta.summary$year.visit,1,4))
  delta.summary$year.visit<-as.factor(delta.summary$year.visit)
  summary.out<-delta.summary[,c("Year","year.visit","n","mean","var","SD","SE","CV","lwr","upr")]

  return(summary.out)
}
