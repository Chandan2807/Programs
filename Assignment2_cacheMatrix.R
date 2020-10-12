makeCacheMatrix<-function(mtrx=matrix())
{
 invrs<-NULL

 set<-function(matrix)
 {
   mtrx<<-matrix
   invrs<<-NULL
 }
 
 get<-function()
 {
   mtrx
 }

 sInvrs<-function(inverse)
 {
   invrs<<-inverse
 }

 gInvrs<-function()
 {
   invrs
 }
  
 list(set=set,get=get,sInvrs=sInvrs,gInvrs=gInvrs)
}

cacheSolve<-function(x,...)
{
 mtrx<-x$gInvrs()
 if(!is.null(mtrx)){
          message("Getting Inverse")
          return(mtrx)
 }
 
 data<-x$get()
 mtrx<-solve(data)
 x$sInvrs(mtrx)
 mtrx
}