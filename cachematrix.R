## Below is a function that creates a matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## The variable that stores the inverse cache.
  Inverse <- NULL
  
  ## Set new matrix and invalidates the cache.
  set <- function(newMatrix) {
    x <<- newMatrix
    Inverse <<- NULL
    }


  ## Return inverse of 'x'
  cacheSolve <- function(x, ...) 
  {
        x
  }

  ## Set cached .
  setinverse <- function(inv)
  {
    Inverse <<- inv
  }
  
  ## Return cached inverse.
  getinverse <- function() 
  {
    Inverse
  }
  
  ## Return the list of functions to the special matrix.
  list(set = set, get = get, setinverse = setinverse,  getinverse = getinverse)
}







## The below function returns an inverse function but it first checks if
#threre is already a cached inverse before it goes ahead and calculates the inverse
cacheSolve <- function(x, ...) {
  
  ## try to retive the inverse matrix
  Inverse <- x$getinverse()
  # check if the retived matrix is valied i.e if it was already calculated 
  if (!is.null(Inverse)) {    
    return(Inverse) #return if the cached is valied
  }
  
  ## if cached is not valied 
  mtx <- x$get()#get the matrix
  Inverse <- solve(mtx, ...)#calculate the inverse 
  x$setinverse(Inverse)# cache the calculated inverse for future use
  
  ## Return the inverse.
  Inverse
}