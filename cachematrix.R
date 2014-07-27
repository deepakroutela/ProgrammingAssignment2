  ## This pair of functions fetches/computes inverse of a matrix. This set works much
  ## faster in case repeatitive calling of identical matrix inverse is required.
  ## This will happen because these pair of function do inverse computation only
  ## for the first time. At that time, it gets cached. Therefore next time its
  ## fetched from cache rather than computing again
  
  ## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
            x <<- y
          inv <<- NULL
    }
    get <- function() x
    setinverse <- function(matinv) inv<<-matinv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
         )
  }
  
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)) {
                  message("getting cached inverse data")
                  return(inv)
          }       
          data <- x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
  }
