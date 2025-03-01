## Cole Yorston

## Two functions created here, the first creates the 'special'
# matrix object that can cache its inverse

## The second computes the inverse of the special matrix 
# returned already, using the solve function as well

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse<- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}

# Now run the functions to obtain the result 

mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$get()
cacheSolve(mat)
mat$getInverse()
mat$set(matrix(c(6, 2, 3, 4), 2, 2))
mat$get()
mat$getInverse()
cacheSolve(mat)
mat$getInverse()

