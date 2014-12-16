## cachematrix.R written by cgil for coursera rprog-016 2014-12-16

## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly that's the puprose of those 2 functions

## sample use case :
#   > x = rbind(c(4, 3), c(3,2))    # let's create a square matrix and store it in x
#   > m<-makeCacheMatrix(x)         # now let's make the special prepared matrix m 
#   > cacheSolve(m3)                #now firt call to cache Solve calculates the inverse
#     [,1] [,2]
#     [1,]   -2    3
#     [2,]    3   -4
#  >  cacheSolve(m3)                #but any new call with the same matrix will use the cached result
#     getting cached data
#     [,1] [,2]
#     [1,]   -2    3
#     [2,]    3   -4


##
# makeCacheMatrix(your_square_matrix) This function creates a special "matrix" object
# that can cache its inverse. this special matrix is for use with cacheSolve()  
# basicaly its a list containing functions :
#  set : to set the value of the matrix
#  get : to get the value of the matrix
#  setinverse : the value of inverse of the matrix
#  getinverse : to get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
}


## cacheSolve(special_square_matrix)) This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache
## this functions expects parameter one to be a special matrix as
## prepared by function makeCacheMatrix(yourmatrix).
## the function assume that the matrix supplied is always invertible. 
## NO CHECK IS DONE ABOUT THAT INSIDE THE FUNCTION

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("Getting cached result of the inverse of this matrix")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
