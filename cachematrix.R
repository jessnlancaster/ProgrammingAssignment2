## These are functions that will cache the inverse of a matrix

## makeCacheMatrix is a function that will create an object
## to store a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# initialize the inverse solution matrix
inv_matrix <- matrix(nrow = 0, ncol = 0)

# function to set the value of the matrix
set <- function(y) {
  x <<- y
  inv_matrix <<- matrix(nrow = 0, ncol = 0) # since we changed the
                                            # matrix value, we
                                            # restore the inverse
                                            # solution to zero
}

# function to get the value of the matrix
get <- function() x

# function to set the value of the inverse
setinverse <- function(inverse) inv_matrix <<- inverse

# function to get the value of the inverse
getinverse <- function() inv_matrix

# assign makeCacheMatrix to an object
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve is a function that will check to see if the inverse
## has been calculated. If so, it will get the mean from the cache
## and skip the computation.
## Otherwise, it will calculate the inverse of the matrix
## and set the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# check to see if inverse has been already calculated
inv_matrix <- x$getinverse()
if(!identical(inv_matrix,matrix(nrow = 0, ncol = 0))) {
# if already calculated: get the inverse from the cache
  message("getting cached data")
  return(inv_matrix)
}
# if not calculated: calculate inverse
newmatrix <- x$get()
inv_matrix <- solve(newmatrix)

# set the value of the inverse in the cache
x$setinverse(inv_matrix)
inv_matrix 
}
