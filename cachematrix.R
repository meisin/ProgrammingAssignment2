## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  ### sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  set <- function(y) {    ### set the value of the matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve - computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {    ### check to see if cacheSolve has been run before
    message("getting cached data")
    return(m)
  }
  
  ### this section is run if cacheSolve has not been run before
  matrix<- x$get()  ### run get function to get the value of the input matrix
  m <- solve(matrix, ...) ### compute the value of the inverse of the input matrix
  x$setinverse(m)  ### compute the value of the inverse of the input matrix
  m
}



###mat <- matrix(1:4, 2, 2)
###mat2 <- makeCacheMatrix(mat)
###cacheSolve(mat2)


###result:
###      [,1] [,2]
###[1,]   -2  1.5
###[2,]    1 -0.5

