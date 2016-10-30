# The function responsible for making cache matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  get <- function() x

  set <- function(y) {

    x <<- y

    inv <<- NULL

  }

  


  getinv <- function() inv

  setinv <- function(inverse) inv <<- inverse

  

  list(get=get, set=set, getinv=getinv, setinv=setinv)

}




cacheSolve <- function(x, ...) {

  inv <- x$getinv()


#qwe

  if (!is.null(inv)) {

    message("inverse is cached")

    return(inv)

  }


  m <- x$get()

  inv <- solve(m, ...)


  x$setinv(inv)


  return(inv)

}

