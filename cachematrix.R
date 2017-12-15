## These two functions work in conjunction to compute the inverse of a matrix, and also check if this inverse has already been computed
## this saves time and resources.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m<<-solve(x)
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
        message("getting caches data")
        return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
  
}

## Here is a test of the function::

 funs <- makeCacheMatrix() ## create the object to be analyzed for inversion (including the original matrix)
 funs$set(matrix(6:9, 2)) ## this is the matrix to be inverted.
 funs$setinverse()
 funs$getinverse() ## here we have the inversion
 ls(environment(funs$set)) ## here we can see all the objects inside the funs object.(we have set, get, setinverse, and getinverse, m, and x
