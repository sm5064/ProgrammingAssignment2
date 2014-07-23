#The first function, `makeVector` creates a special "vector", which is
#really a list containing a function to

#1.  set the value of the vector
#2.  get the value of the vector
#3.  set the value of the mean
#4.  get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#The following function calculates the mean of the special "vector"
#created with the above function. However, it first checks to see if the
#mean has already been calculated. If so, it `get`s the mean from the
#cache and skips the computation. Otherwise, it calculates the mean of
#the data and sets the value of the mean in the cache via the `setmean`
#function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

 # Let's create an instance of makeVector:
#  > supervector <- makeVector(c(2, 4, 6, 8))
 # (In other words, supervector is the name of a makeVector
# that we've just created.)
 # Let's calculate the mean of 2, 4, 6, 8:
#  > mean(c(2, 4, 6, 8))
#[1] 5
 # So cachemean should return something similar:
#  > cachemean(supervector)
#[1] 5
 # So how is the makeVector-cachemean combo better than
 # the vanilla flavoured vector-mean combo? 
 # Answer: the next time we call cachemean on supervector,
 # R doesn't have to actually calculate the mean, R can
 # just read it from where it was stored (in supervector)
 # when it was first calculated:
# cachemean(supervector)
#getting cached data
#[1] 5


#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()         # Returns original matrix
#cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
#amatrix$getinverse()  # Returns matrix inverse
#cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

#amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#amatrix$get()         # Returns matrix
#amatrix$getinverse()  # Returns matrix inverse
