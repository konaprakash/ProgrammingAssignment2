# Creating the matrix
makeCacheMatrix <- function(x = matrix()){
  inv <- null
  set <- function(y){
    x <<- y
    m <<- null
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#If the matrix inverse is already availble in Cashe pull from Cache. #Otherwise Create inverse
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data,...)
  x$setinverse(inv)
  inv
}