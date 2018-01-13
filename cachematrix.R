##  makeCacheMatrix is a convenience function that implicitly creates
## an environment to store a matrix and its inverse. The objects
## inside the environment can be accessed by the four functions (get,
## set, getinv, setinv), returned by a call to makeCacheMatrix. The
## only input parameter to makeCacheMatrix is a matrix (the default value
## is matrix()).
##  cacheSolve takes the list of four functions from makeCacheMatrix
## as an input and returns the matrix' inverse. The invese matrix will
## be cached inside makeCacheMatrix' environment such that subsequent
## calls to cacheSolve do not need to recompute the inverse.


#' implicitly creates an environment that can store a matrix and
#' its inverse. the matrix and its inverse can be accessed/modified
#' through the list of functions returned by makeCacheMatrix:
#'  - get() return the matrix
#'  - set(m) set the matrix to m and its inverse to NULL
#'  - getinv() return the inverse of the matrix if cacheSolve has 
#'             been called beforehand, otherwise NULL
#'  - setinv(m) set the inverse matrix to m
#' 
#' @param x matrix; default: matrix()
#' @return list of 4 functions; allows to access/modify
#'         the matrix (get, set) and its inverse (getinv, setinv)
#' @examples
#' \dontrun{
#' MyMatrix <- makeCacheMatrix(matrix(rnorm(144, 0, 1), ncol = 12))
#' InverseMatrix <- cacheSolve(MyMatrix)
#' round(MyMatrix$get() %*% InverseMatrix, 1)
#' round(MyMatrix$get() %*% MyMatrix$getinv(), 1)
#' }
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_) inv <<- inv_
  getinv <- function() inv
  list(set    = set, 
       get    = get,
       setinv = setinv,
       getinv = getinv)
}



#' computes the inverse of a matrix or returns the
#' cached inverse matrix, if previously calculated.
#' 
#' @param x return value from makeCacheMatrix
#' @param ... further arguments passed to solve(x, ...)
#' @return inverse of the matrix in x
#' @examples
#' \dontrun{
#' MyMatrix <- makeCacheMatrix(matrix(rnorm(144, 0, 1), ncol = 12))
#' InverseMatrix <- cacheSolve(MyMatrix)
#' round(MyMatrix$get() %*% InverseMatrix, 1)
#' round(MyMatrix$get() %*% MyMatrix$getinv(), 1)
#' }
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv_ <- solve(data, ...)
  x$setinv(inv_)
  inv_
}

