## used to create a cache matrix


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


## to cache the inverse of a matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
