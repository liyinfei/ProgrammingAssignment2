## makeCacheMatrix() creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #inv is the inverse of matrix, NULL as default in makeCacheMatrix()
  inv<-NULL
  #set() can acquire new matrix through "y" and change the output of get(). inv is set to NULL 
  #in the global environment
  set <- function(y) {
    #when new input from set(y), x or get() is updated to y in the global environment
    x <<- y
    #inv is set to NULL in the global environment
    inv <<- NULL
  }
  #get() requires matrix from makeCacheMatrix(x)
  get <- function() x
  #setinverse() stores input "inverse" as the matrix inverse in the global environment
  setinverse <- function(inverse) inv <<- inverse
  #getinverse() outputs the result of setinverse(inverse) or the result of inv from cacheSolve()
  getinverse <- function() inv
  #store set(),get(),setinverse(),getinverse() in the makeCacheMatrix() and name them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() caches the inverse matrix.
cacheSolve <- function(x, ...) {
  #assign getinverse() from makeCacheMatrix() to inv(inverse matrix)
  inv <- x$getinverse()
  #determin if a inverse matrix exist.
  if(!is.null(inv)){
    #if so, the message and value inv will be returned.
    message("getting cached data")
    return(inv)
  }
    #if not, the m(matrix) will be obtained from get() 
  m <- x$get()
    # and inv(inverted)
  inv <- solve(m, ...)
    #the inverted results will be stored in setinverse(inv)
  x$setinverse(inv)
    # and show the inverted result
  inv
}
