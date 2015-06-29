makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      #m is ultimately where the inverse will be cached
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #here, it's setting the values of x into the function 
      get <- function() x
      #get stores the values for now
      setinv <- function(solve) m <<- solve
      #setinv is made to find the inv, and m is then stored into the function solved
      getinv <- function() m
      #getinv is a function made to pull m, and set it as x
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
      #don't know why this list is made
}

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      #here, m is set as x and the getinv function
      if(!is.null(m)){
            message("retrieving cache")
            return(m)
      }
      #if m is NULL, this informs that it is null
      invent <- x$get()
      #invent variable is made to store the data in get
      m <- solve(invent, ...)
      #m is now set as the solved invent , or the inverse
      x$setinv(m)
      #m is finally set as the inverse and printed
      m
}

