#Assignmnet 2- Lexical Scoping
#Caching the Inverse of a Matrix:


#makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  inv_m <- NULL  # initially nothing is cached so set it to NULL
  #stores a matrix
  setm <- function(m1) {
    m <- m1
    inv_m <<- NULL
  }
  #returns the stored matrix
  getm <- function() {
    m
  }
  #cache the given argumet
  setInverse <- function(inverse) 
    {
    inv_m <<- inverse
  }
  #get the cached value
  getInverse <- function() {
    inv_m
  }
  #returns a list
  list(set = setm,
       get = getm,
       setInverse = setInverse,
       getInverse = getInverse)
}


#CacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_x <- x$getInverse() #get the cahed value
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x) #return the cached value if it exists
  }
   #otherwise get the matrix, caclulate the inverse and store it 
  m <- x$get()
  inv_x <- solve(m, ...)
  x$setInverse(inv_x)
  inv_x #return the inverse
}

#verification
m<-matrix(c(2,3,4,5),2,2)
mymatrix <- makeCacheMatrix(m)
mymatrix$get()
mymatrix$getInverse()
cacheSolve(mymatrix)
mymatrix$getInverse()




