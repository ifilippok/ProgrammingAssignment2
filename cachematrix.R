## Calculates inverse matrix and putting it value to cache to avoid 
##                                                   double computation


## This function return list of functions:
## set - setting matrix for defind value
## get - getting valie of matrix
## setInverseMatr - set value for inverse matrix
## getInverseMatr - get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatr <- NULL
  set1 <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  get1 <- function() x
  setInverseMatr1 <- function(invMatrix) invMatr <<- invMatrix
  getInverseMatr1 <- function() invMatr
  
  #returning functions list
  # set1, get1, ... is used to separate returing values from internal values
  list(set = set1, get = get1,
       setInverseMatr = setInverseMatr1,
       getInverseMatr = getInverseMatr1)
}

## This function calculates inverse matrix and put value to cash. If inversed 
## value has been already calculated it will received from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatr <- x$getInverseMatr()
  if(!is.null(invMatr)) {
    message("getting cached data")
    return(invMatr)
  }
  matr <- x$get()
  #inverting matrix
  invMatr <- solve(matr)
  x$setInverseMatr(invMatr)
  #returning iversed matrix
  invMatr
}

#checking results: myMatr %*% myInv
