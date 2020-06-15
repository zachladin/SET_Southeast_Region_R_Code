#summary function creates table of n, mean, var, SD, and SE
summaryFunction <- function(dataIn, factor, response){
  summaryOut <- ddply(dataIn, factor, .fun = function(xx){
    c(n = length(xx[,response]),
      mean = mean(xx[,response],na.rm=TRUE),
      var = var(xx[,response],na.rm=TRUE),
      SD = sd(xx[,response],na.rm=TRUE),
      SE = sqrt(var(xx[,response],na.rm=TRUE)/length(xx[,response])),
      CV = sd(xx[,response],na.rm=TRUE)/mean(xx[,response],na.rm=TRUE),
      lwr = mean(xx[,response],na.rm=TRUE)-sqrt(var(xx[,response],na.rm=TRUE)/length(xx[,response]))*1.96,
      upr = mean(xx[,response],na.rm=TRUE)+sqrt(var(xx[,response],na.rm=TRUE)/length(xx[,response]))*1.96)
  })
  return(summaryOut)
  dev.off()
}
