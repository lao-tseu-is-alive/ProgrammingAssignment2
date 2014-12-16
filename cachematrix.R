##    cachematrix.R 
##    written by cgil for coursera rprog-016 2014-12-16
##    https://github.com/lao-tseu-is-alive/ProgrammingAssignment2/

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
## makeCacheMatrix(your_square_matrix) This function creates a special "matrix" object
## that can cache its inverse. this special matrix is for use with cacheSolve()  
## basicaly its a list containing functions :
##  set : to set the value of the matrix
##  get : to get the value of the matrix
##  setinverse : the value of inverse of the matrix
##  getinverse : to get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL               # initialise inverse to null
      set <- function(y) {          # in case we receive new matrix data
            x <<- y                 # let's store this new matrix
            inverse <<- NULL        # and reinit the inverse (new matrix => new inverse)
      }
      get <- function() x           # let's give back the matrix
      setinverse <- function(inv) inverse <<- inv # allows to store the inverse
      getinverse <- function() inverse    # and this one is to get the inverse
      list(set = set, get = get,          # and let's build and return the whole list of functions
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
      inverse <- x$getinverse() # let's store any previous results localy
      if(!is.null(inverse)) {
            message("Getting cached result of the inverse of this matrix")
            return(inverse)
      }
      data <- x$get()         # that's the new matrix data
      inverse <- solve(data)  # let's calculate the inverse
      x$setinverse(inverse)   # and store it for later use
      inverse                 # don't forget to return the result...
}
