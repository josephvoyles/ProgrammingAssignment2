## Store a matrix and its inverse, check for stored values, and calculate values if not present

# This function takes a matrix as an input and creates a class that stores it and its inverse in memory
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # sets the matrix and its inverse as class attributes
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # returns the current matrix stored in memory
  get <- function() x
  # sets the inverse attribute by updating the inv variable instantiated within the parent function
  setInverse <- function(inverse) inv <<- inverse
  # returns the inverse attribute
  getInverse <- function() inv
  # returns the list of available functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function takes a matrixCache object as an input
# The object is evaluated for an existing matrix inverse
# An inverse matrix is calculated if one does not exist and returns
# the inverse that is in memory if it does
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the inverse from the matrixCache class
        inv <- x$getInverse()
        # check that the inverse is not null
        if(!is.null(inv)) {
          # if inverse is not null, return the inverse stored in memory
          message("getting cached data")
          return(inv)
        }
        # if the inverse is null, calculate the inverse of the matrix and store in the object
        matrix <- x$get()
        inv <- solve(matrix)
        # set and store and store the matrix invers
        x$setInverse(inv)
        # return the inverse
        inv
}
