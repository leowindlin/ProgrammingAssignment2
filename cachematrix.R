##First step: create makeCache function that
##returns a "special matrix" that is a list
##with functions to 1)set the value of a matrix,
##2)get the value of a matrix, 3)set the value of 
##the inverse and 4)get the value of the inverse
makeCacheMatrix<- function(x = matrix()) 
{
  ##I will use m as the cache variable
  m <- NULL
  
  ##I create the set function: it will set the
  ##value of the matrix
  set <- function(y) 
  {
    ##Set the y matrix to the x variable 
    ##in the above enviroment
    x <<- y 
  
    ##The cache must go back to NULL in 
    ##the above enviroment
    m <<- NULL
  }
  
  ##I create the get function: it will return
  ##the value of the matrix
  get <- function() x
  
  ##We do the same for the inverse function
  
  ##Here I set the cache m as the inverse
  ##of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  ##Here I return the cache
  getInverse <- function() m
  
  #Return the results as a list
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Second step: I create the function that
##returns the inverse of a given matri.
##The function checks if the inverse is
##already calculated in the cache and
##return it if so; otherwise, it
##calculates the inverse
cacheSolve <- function(x,...) {
  
  m <- x$getInverse()
  if(!is.null(m))
  {
    print("Returning cache")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
