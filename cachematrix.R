#an R function is able to cache potentially time-consuming computations
# the function helps in the creation of a cache matrix
makeCacheMatrix <- function(x = matrix()) {
        inv=NULL
  set<-function(y){
    x<<-y
    inv<-NULL
  }
  get<- function() {x}
  sinv<- function(inverse)(inv<<-inverse)
  ginv<-function(){inv}
  list(set=set,get=get,sinv=sinv,ginv=ginv)

}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       inv<-x$ginv()
  if(!is.null(inv)){
    message("cashe data is being received")
    return(inv)
    
  }
  mat<-x$get()
  inv<- solve(mat,...)
  x$sinv(inv)
  inv
}
