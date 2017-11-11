pollutantmean<-function(directory,pollutant,id){
  
  pollutantdata<-data.frame()
  for(i in id){
    if(i<10){
      i<-paste("00",i,sep="")
    }
    else if(i<100){
      i<-paste("0",i,sep="")
    }
    else if(i<1000){
      i<-paste("",i,sep="")
    }
   
    filename<-paste(directory,"\\",i,".csv",sep="")
    #print(filename)
    temppollutant<-read.csv(filename,header = TRUE)
    pollutantdata<-rbind(pollutantdata,temppollutant)
  }
  
  value<-pollutantdata[,pollutant]
  
  meanvalue<-mean(value,na.rm = TRUE)
  print(meanvalue)
}


complete<-function(directory,filelist){
  setwd(directory)
  #filelist<-list.files(directory,full.names = FALSE)
  pollutantdata<-data.frame()
  
  for(i in filelist){
 
      if(i<10){
        i<-paste("00",i,sep="")
      }
      else if(i<100){
        i<-paste("0",i,sep="")
      }
      else if(i<1000){
        i<-paste("",i,sep="")
      }
    
    #print(i)
    filename<-paste(directory,"\\",i,".csv",sep="")
    #if(ext[2]=="csv"){
      temppollutant<-read.csv(filename,header = TRUE)
      completecases<-complete.cases(temppollutant[,1],temppollutant[,2],temppollutant[,3],temppollutant[,4])
      sumcompletecases<-sum(completecases)
      temppollutantdata<-data.frame(monitornames=i,totalcase=sumcompletecases)
      pollutantdata<-rbind(pollutantdata,temppollutantdata)
    #}
  }
  print(pollutantdata)
}


completecorr<-function(directory,threshold){
  setwd(directory)
  filelist<-list.files(directory,full.names = FALSE)
  pollutantdata<-data.frame()
  corrdata<-vector()
  
  for(i in filelist){
    #print(i)
    ext<-strsplit(i,split = ".")
    #if(ext[2]=="csv"){
    temppollutant<-read.csv(i,header = TRUE)
    completecases<-complete.cases(temppollutant[,1],temppollutant[,2],temppollutant[,3],temppollutant[,4])
    sumcompletecases<-sum(completecases)
    temppollutantdata<-data.frame(monitornames=i,totalcase=sumcompletecases)
    pollutantdata<-rbind(pollutantdata,temppollutantdata)
    #}
   
    
  }
  monitornames<-pollutantdata$monitornames[pollutantdata$totalcase>threshold]
  for(j in monitornames){
    tempcorrelation<-read.csv(j,header = TRUE)
    correlation<-cor(tempcorrelation[,2],tempcorrelation[,3],use="na.or.complete",method = "pearson")
    corrdata<-append(corrdata,correlation)
  }
  corrdata
}