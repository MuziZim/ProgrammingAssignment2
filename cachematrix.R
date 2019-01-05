##makeCacheMatrix creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {       #initiate the function by setting x (input) to an empty matrix
  mTrix <- NULL                                   #mTrix will be the inverted matrix that gets returned
  calc <- function(y) {                           #initiate the actual calculation
    x <<- y                                       #store the output in different environment
    mTrix <<- NULL                                #mTrix stored in different environment (still Null)
  }
  ret <- function() x                             
  set <- function() mTrix <<- solve(x)            #calculate the inverse and store in separate environment
  obt <- function() mTrix                         #obtain the value of the inverse matrix
  list(calc = calc, ret = ret, set = set, get = get)
  
}

##casheSolve solves function above - if already solved AND MATRIX HASN'T CHANGED, retrieve from cache

cacheSolve <- function(x, ...) {                  #solve inverse function - input is matrix x
  
  mTrix <- x$obt()                                #return the value of the inverse matrix (makeCacheMatrix function)
  if (!is.null(mTrix)) {                          #if the cached matrix is not null, then obtain that value
    message("getting cached data")
    return(mTrix)
  }
  mat <- x$ret()
  mTrix <- solve(mat, ...)
  x$set(mTrix)
  mTrix
}

