## The makeCacheMatrix is used to store value that take a long time to compute,
## by storing the value into a cache, the computer need not compute the value
## again and thus save time and computing power.

##   ##return: a list containing functions to
  ##  1.set the matrix
  ##  2.get the matrix
  ##  3.set the inverse
  ##  4.get the inverse
  ##  this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL ##initializes the inv
  set = function(y){ ## assigning x,inv into a different environment
        x<<-y
    inv <<- NULL
  }
  get = function()x
  setinv= function(inverse)inv<<-inverse
  getinv= function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()

  #check if the inv is null, if not the calculated value of inv stored will be
  #returned.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #if inv is null, calculate the inv
  z=x$get()
  inv=solve(z,...) #computes the inv
  x$setinv(inv) #sets the value of inv in the cache via setinv function
  return(inv)
}
