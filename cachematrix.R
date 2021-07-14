# deliberate an element for the matrix
# utilize gb as the name of the matrix
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  gb <- NULL
  set <- function(y) {
    x <<- y
    gb <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) gb <<- solve
  getsolve <- function() gb
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## procuring the opposite value
cacheSolve <- function(x, ...) {
  gb <- x$getsolve()
  if(!is.null(gb)) {
    message("getting inversed matrix")
    return(gb)
  }
  data <- x$get()
  gb <- solve(data, ...)
  x$setsolve(gb)
  gb
## go back to matrix gb, which is the antithetical of gb
}
