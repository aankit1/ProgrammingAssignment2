## The following functions are to cache the inverse of a matrix

## This function calculate the inverse of a matrix and save its value to chache.

makeCacheMatrix <- function(x = matrix()) {
  inverseY<-NULL
  set<-function(y)
  {
    x<<-y
    inverseY<<-NULL
  }
  get <-function() x
  setinverse<-function()
  {
    inverseY<<-solve(x)
  }
  getinverse<-function() inverseY
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function is to compute the inverse of a matrix if it is stored in cache or else calculate it.

cacheSolve <- function(x, ...) {
        inverseY<-x$getinverse()
        if(!is.null(inverseY)){
          message("getting cached data")
        return(inverseY)
        }
        else
        {
        number <-x$get()
        inverseY<<-solve(number)
        return(inverseY)
        }
}
