## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## it creates a list that contains 4 member functions: set, get, setInv
## and getInv.
makeCacheMatrix <- function(x = matrix()) {
	  xinv <- NULL ##  result of inversion stored
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL ## initialises xinv to null
      }

      get <- function() x ## return the input matrix x
      setInv <- function(inv) xinv <<- inv ## set the inversed matrix
      getInv <- function() xinv ## return the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInv() ## get the inversed matrix from object x
      ## If not yet calculated will be Null from the intialize of the makecache
      if(!is.null(m)) { ## if the inversion result not Null
	  message("getting cached data")
	  return(m) ## return the calculated inversion from cache 
      }
      matobj <- x$get() ## if not, we do x$get to get the matrix object
      m <- solve(matobj) ## solve will inverse it
      x$setInv(m) ## set to the object
      m ## return the solved result 
}
