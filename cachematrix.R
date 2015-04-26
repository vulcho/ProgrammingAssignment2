## These functions cache the inverse of a matrix.
## They recalc the code once rather than doing it every time in a loop.
## This helps simplify the matrix inversion which can be a compuatationaly intinsive task.

## The first function creates a matrix the input of which is a variable.
## This will help in caching. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve computes inverse and caches the result.
## This also uses the already computed results of the same special matrix,
## if the function is rerun. This avoids the recomputation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
