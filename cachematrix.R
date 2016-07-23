
## makeCacheMatrix will take a matrix object and new object by default and will
## return a list of 4 methods i.e setmatrix,setmatrix,getinv,setinv and all these
## methods are nested methods with lexical scope variable i.e invert 
## which will hold the inverted matrix for matrix say x.

makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(y) {
    x <<- solve(y)
    inversion <<- NULL
  }
  get <- function() x
  setinversion <- function(invert) inversion <<- invert
  getinversion <- function() inversion
  list(setmatrix = set, getmatrix = get,
       setinv = setinversion,
       getinv = getinversion)
}


## cacheSolve method will take function makeCacheMatrix as argument where the list 
## of methods will be called for inverting the (makeCacheMatrix(x)), 
## x matrix variable, and if the value is not cached i.e (is.null(inv))==TRUE
## we will set the inverted matrix and cache it in the setinv nested function
## body and we return its body
 
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inversion data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
