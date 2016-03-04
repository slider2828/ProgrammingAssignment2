## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inverse) inversematrix <<- inverse
  getinversematrix <- function() inversematrix
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}




cacheSolve <- function(x, ...) {
  inversematrix <- x$getinversematrix()
  
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  
  mat.data <- x$get()
  inversematrix <- solve(mata.data, ...)
  
  x$setinversematrix(inversematrix)
  
  return(inversematrix)
}


x <- c(1, 2)
y <- c(2,1)
z <- rbind(x, y)


solve(z) %*% z

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}