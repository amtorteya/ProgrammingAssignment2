## This set of functions cache the inverse of a matrix

## This function creates an object (list) with a set of 4 functions to get and
## set the matrix, and get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  out <- NULL
  set <- function(y) {
    x <<- y
    out <<- NULL
  }
  get <- function() x
  setInv <- function(inv) out <<- inv
  getInv <- function() out
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function cheks if there is a solution in the cache. If not, it computes
## it and caches it in the makeCacheMatrix object for future use

cacheSolve <- function(x, ...) {
  out <- x$getInv()
  if(!is.null(out)) {
    message("getting cached data")
    return(out)
  }
  data <- x$get()
  out <- solve(data, ...)
  x$setInv(out)
  out
}
