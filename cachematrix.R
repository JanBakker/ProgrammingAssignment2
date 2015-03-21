  ## Jan Bakker, 21 maart 2015
  ## makeCacheMatrix is a function to create space en structure
  ## in the memory of your computer (cache) to store an matrix.
  ## The memoryposition is defined in the set-funtion
  ## The structure is a list of 4 vectors (set, get, setmatrix, getmatrix)
  ## In makeCacheMatrix we do not calculate the inverse matrix.
  ## Calculating of the inverse matrix is done in cacheSolve 

makeCacheMatrix <- function(x) {
        m <- NULL
        ## by using <<- we make a pointer to the memoryposition
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        ## show memoryposition of the cache
	  list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
	  ## if we've already inverted the matrix 
	  ## then lets get the inverted matrix from cache
        if(!is.null(m)) {
                message("getting inverted matrix from cache")
                return(m)
        }
        data <- x$get()
        ## invert the matrix and cache in x$setmatrix(m)
	  m <- solve(data, ...)
        x$setmatrix(m)
        m
}
