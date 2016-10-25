## These functions cache a matrix and then return its inverse either by calculating it
## for the first time or by returning the cached matrix if it has already been calculated. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {             # define the argument, default mode = "matrix"
        inv <- NULL                                     # initialize inv as NULL; will hold the cached value of inverse
        set <- function(y) {                            # define the set function; stores new value for x and if there
                x <<- y                                 # is a new matrix, resets inv to NULL
                inv <<- NULL
        }
        get <- function() x                             # define get function; returns the matrix x (stored in parent env.)
        setmat <- function(inverse) inv <<- inverse     # define setmat function; stores inverse matrix as in (in parent env.)
        getmat <- function() inv                        # define getmat function; returns cached inv (from parent env.)
        list(set = set, get = get, setmat = setmat, getmat = getmat)    # stores all the functions above as a list so that they can
                                                                        # be called elsewhere. makeCacheMatrix should be stored
                                                                        # as an object.
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                        # define the arguments of cacheSolve; x is the list created
                                                        # by the makeCacheMatrix function.
        inv <- x$getmat()                               # retrieves the object inv from parent env.
        
        if(!is.null(inv)) {                             # if inv isn't NULL, print "getting cached data" and
                message("getting cached data")          # return value of inv
                return(inv)
        }
        
        data <- x$get()                                 # if inv is NULL, get the value of the matrix and store as data
        inv <- solve(data, ...)                         # solve the matrix data and store as inv
        x$setmat(inv)                                   # setmat caches new value for inv in the parent env.
        inv                                             # return inv
}
