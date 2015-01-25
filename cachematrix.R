## This function will create a special matrix object, that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get, setinverse = setInverse, getinverse = getInverse)
}


## This function will check if this matrix's inverse has already been calculated nad if not then it will
## calculate this matrix's inverse and cache its value

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        	message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#trying, if functions work
a <- matrix(c(3,7,4,2,8,4,6,7,5),3,3)
d <- makeCacheMatrix(a) 
cacheSolve(d) 
