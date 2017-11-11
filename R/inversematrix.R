makeCacheMatrix<-function(x=matrix())
{
  transpose<-matrix(rep(NA,4),nrow = 2,ncol = 2)
  set<-function(y){
    y<<-x
    transpose<<-matrix(rep(NA,4),nrow = 2,ncol = 2)
  }
  get<-function(){x}
  settranspose<-function(trans=matrix()){transpose<<-trans}
  gettranspose<-function(){transpose}
  return(list(set=set,get=get,settranspose=settranspose,gettranspose=gettranspose))
}

cacheSolve<-function(func,...){
  mattrans<-func$gettranspose()
  if(!all(is.na(mattrans))){
    print("Value obtained from the cache")
    return(mattrans)
  }
  else{
    matns<-solve(func$get())
    func$settranspose(matns)
    return(func$gettranspose())
    
  }
  
}