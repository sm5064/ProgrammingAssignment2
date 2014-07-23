
#`makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}

  
#`cacheSolve`: This function computes the inverse of the special
#  "matrix" returned by `makeCacheMatrix` above. If the inverse has
#  already been calculated (and the matrix has not changed), then
#  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## Examples of using above functions
#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()         # Returns original matrix
#cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
#amatrix$getinv()  # Returns matrix inverse
#cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

#amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#amatrix$get()         # Returns matrix
#amatrix$getinv()  # Returns matrix inverse
