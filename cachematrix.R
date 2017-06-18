## Matrix inverse could become very costly operation if its dimensions are very  large.
## Below functions caches the inverse of matrix, which reduces the compuation of the inverse again and again
## if input value is same. As R, uses the memory to store the data sets, caching could be very helpful if resources are limited.

## This method creates a  "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <-function() x
  setInverse <- function(inverse) inv<<-inverse
  getInverse <-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}



## This function computes the inverse of matrix created by above function
## If the value exists in cache, then this method returns the value else it calculates the inverse of matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <-solve(mat, ...)
  x$setInverse(inv)
  inv
}
