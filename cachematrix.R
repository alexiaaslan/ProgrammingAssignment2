# Course R Programming
# Week 3 - Peer Assignment

## Overall Comments
# The chalenge of this assignemnt was to understand how caching and cache-effective algorithms worked 
# The solutions ended up being an adaptation of the examples in the exercise instructions to the context of matrix inversion

## Function makeCacheMatrix
# This function is very similar to the example provided in the instructions given that the idea was to do the same thing but instead of the mean it should be the inverse and instead of being a vector it is a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Function cacheSolve
# Similar to the first function, this one was also based on the exercise instructions but instead of using the mean() function, I used the solve()
# Also changed the names to match the previous function and to make more sense in the context of the exercise

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}

