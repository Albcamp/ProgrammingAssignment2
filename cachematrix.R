## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<- function(x= matrix()){  # Defining the function using matrix as the default arg. value.
  inv<- NULL                              # to hold value of matrix inverse.
  set<- function(y){                      # Define set function
    x<<- y                                # set new value of matrix in parent environment
    inv<<- NULL                           # if there is a new matrix, reset inv to NULL
  }
  get<- function() {x}                              # returns value of the matrix argument.
  setInverse<- function(inverse) {inv<<- inverse}   # assings value of inv in parent environment. 
  getInverse<- function(){inv}                      # gets value of inv where it was called.
  list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)   # necesary for refering the functions.
}                                                                               # with the $ operator


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed),
##             then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...){             # gets cache data
  inv<- x$getInverse()                     # returns a matrix that is the inverse of x
  if(!is.null(inv)){                       # checking if inverse is NULL
    message("getting cached data")         # if inverse retrieved from the cache, share message
    return(inv)
    
  }
mat <- x$get()
inv<- solve(mat, ...)                     # to compute the inverse of a function
x$setInverse(inv)                         # set value of the inverse in the cache.
inv                                       # Returns a matrix that is the inverse of x.
}
