### Sample Input/Output

# > m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
#  > m
# [,1] [,2]
# [1,]    0    2
# [2,]    1    0
# > n<-makeCacheMatrix(m)
# > n$get_matrix()
# [,1] [,2]
# [1,]    0    2
# [2,]    1    0
# > cacheSolve(n)
# Checking cache...
# Get matrix...
# Calculating inverse...
# Calculated result returned and cached...
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0
# > cacheSolve(n)
# Checking cache...
# Cached result returned...
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0

### Special matrix object that can cache its inverse
### As with the sample function, we need to
# a. set the matrix
# b. get the matrix
# c. set the inverse
# d. get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  ## The special operator '<<-' assigns a value in a different environment
  # a. set the matrix
  set_matrix = function(y) {
    x       <<- y
    inverse <<- NULL
  }

  # b. get the matrix
  get_matrix = function() {
    x
  }

  # c. set the inverse
  set_inv_matrix = function(inverse_matrix) {
    inverse <<- inverse_matrix
  }

  # d. get the inverse
  get_inv_matrix = function()  {
      inverse
  }

  list(set_matrix=set_matrix, get_matrix=get_matrix, set_inv_matrix=set_inv_matrix, get_inv_matrix=get_inv_matrix)
}

### Computes the inverse of the special matrix returned by above
### If matrix has not changed, retrieve its inverse from cache
# It takes as its input, the output of the above function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  message ("Checking cache...")
  inverse = x$get_inv_matrix()

  ## Check cache to see if its inverse has already been calculated
  if (!is.null(inverse)) {
    message ("Cached result returned...")
    return (inverse)
  }

  ##  Calculate the inverse if not cached
  message ("Get matrix...")
  mymatrix = x$get_matrix()
  message ("Calculating inverse...")
  inverse  = solve(mymatrix, ...)

  ## Save to cache
  x$set_inv_matrix(inverse)

  ## Return the calculated value
  message ("Calculated result returned and cached...")
  return (inverse)
}