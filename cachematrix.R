## function that creates a special matrix object and store it in cache.

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    
    set <- function(y){
        x <<- y
        m_inv <<- NULL
    }
    
    get <- function() x
    set_inv <- function(inv) m_inv <<- inv
    get_inv <- function() m_inv
    list( get = get, set = set,
          set_inv = set_inv,
          get_inv = get_inv)
}


## this function computes the inverse of the special matrix object 
## created by the previous function

cacheSolve <- function(x, ...) {
    m_inv <- x$get_inv()
    
    if(!is.null(m_inv)){
        message("getting cashed result")
        return(m_inv)
    }
    
    data <- x$get()
    m_inv <- solve(data, ...)
    x$set_inv(m_inv)
    m_inv
    
}
