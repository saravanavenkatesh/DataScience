setwd("C:\\D\\Repos\\DataScience\\R\\FProgAssignment3-data")
plotmortalilty<-function(){
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[,11]<-as.numeric(outcome[,11])
  hist(x = outcome[,11],main="heart attack mortalit rate")
}

best<-function(state,outcome_val){
  outcome<-read.csv("outcome-of-care-measures.csv")
  outcomepossibilties<-c("heart attack", "heart failure", "pneumonia")
  if(is.element(state,outcome$State))
  {
    if(is.element(outcome_val,outcomepossibilties))
    {
      access_val=numeric(1)
      outcome_temp<-split(outcome,outcome$State)
      outcome_state<-outcome_temp[[state]]
      
      if(outcome_val=="heart attack"){access_val<-11}
      else if(outcome_val=="heart failure"){access_val<-17}
      else {access_val<-23}
        
      outcome_state[,access_val]<-as.numeric(as.character(outcome_state[,access_val]))
      min_value<-min(outcome_state[,access_val],na.rm = TRUE)
      outcome_state_temp<-outcome_state$Hospital.Name[outcome_state[,access_val] == min_value]
      outcome_state_temp[!is.na(outcome_state_temp)]
    }
    else
    {
      stop("[Invalid outcome name")
      
    }
    
    
  }
  else
  {
    stop("Invalid state name")
  }
  
  
}

rankhospital<-function(state,outcome_val,rank_temp){
  
  
  outcome<-read.csv("outcome-of-care-measures.csv")
  outcomepossibilties<-c("heart attack", "heart failure", "pneumonia")
  if(is.element(state,outcome$State))
  {
    if(is.element(outcome_val,outcomepossibilties))
    {
      access_val=numeric(1)
      outcome_temp<-split(outcome,outcome$State)
      outcome_state<-outcome_temp[[state]]
      
      if(outcome_val=="heart attack"){access_val<-11}
      else if(outcome_val=="heart failure"){access_val<-17}
      else {access_val<-23}
      
      outcome_state[,access_val]<-as.numeric(as.character(outcome_state[,access_val]))
      #min_value<-min(outcome_state[,access_val],na.rm = TRUE)
      #outcome_state_temp<-outcome_state$Hospital.Name[outcome_state[,access_val] == min_value]
      #outcome_state_temp[!is.na(outcome_state_temp)]
      newdata<-outcome_state[order(outcome_state[,access_val],outcome_state$Hospital.Name,na.last=TRUE),]
      newdata_rate<-newdata[,access_val]
      rank<-numeric()
      prev_rank<-min(newdata_rate,na.rm = TRUE)
      rankingid<-0
     
      for(i in newdata_rate)
      {
     
        if(!is.na(i))
        {
        if(i > prev_rank || i== prev_rank )
        {
          prev_rank<-i
          rankingid<-rankingid+1
          rank<-rbind(rank,rankingid)
        }
        #else if (i== prev_rank)
        #{
        #  prev_rank<-i
        #  rankingid<-rankingid+1
        #  rank<-rbind(rank,rankingid)
          
        #}
        }
        else
        {
          rank<-rbind(rank,0)
        }
        
        
        
        
      }
      if(rank_temp=="best"){rank_temp<-1}
      else if(rank_temp=="worst"){rank_temp<-rankingid}
      else if(rank_temp>length(newdata_rate)){stop("Invalid rank")}
      newdata<-cbind(newdata,rankid=rank)
      outcome_state_temp<-newdata$Hospital.Name[newdata$rankid == rank_temp]
      unsorted<-outcome_state_temp[order(outcome_state_temp[!is.na(outcome_state_temp)])]
      unsorted[1]
    }
    else
    {
      stop("[Invalid outcome name")
      
    }
    
    
  }
  else
  {
    stop("Invalid state name")
  }
  
  
}

rankall <- function(outcome_val, num = "best") {
  outcome<-read.csv("outcome-of-care-measures.csv")
  outcomepossibilties<-c("heart attack", "heart failure", "pneumonia")
  result<-data.frame()
  if(is.element(outcome_val,outcomepossibilties))
  {
    if(outcome_val=="heart attack"){access_val<-11}
    else if(outcome_val=="heart failure"){access_val<-17}
    else {access_val<-23}
    outcome[,access_val]<-as.numeric(as.character(outcome[,access_val]))
    
    statesplit<-split(outcome,outcome$State)
    
    for(i in 1:length(statesplit))
    {
      outcome_state<-statesplit[[i]]
      outcome_state[,access_val]<-as.numeric(as.character(outcome_state[,access_val]))
      outcome_state<-outcome_state[order(outcome_state[,access_val],outcome_state$Hospital.Name,na.last=TRUE),]
      statesplit[[i]]<-outcome_state
    }
    
   
    
    for(j in 1:length(statesplit))
    {
      outcome_state<-statesplit[[j]]
      if(num=="best"){num_temp<-1}
      else if(num=="worst"){num_temp<-length(outcome_state$State)}
      else{num_temp<-num}
      
      outcome_state_Hospital<-outcome_state[num_temp,]['Hospital.Name']
      outcome_state_State<-as.character(names(statesplit[j]))
      row_temp<-data.frame(Hospital=outcome_state_Hospital,State=outcome_state_State)
      
      result<-rbind(result,row_temp)
      
    }
  }
  
 names(result) <-c("Hospital","State")
 result  
   
}
