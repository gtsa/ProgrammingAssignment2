## We create a function that returns an object that:
# 1) contains either a matrix object given by the user (named "x") or else a null matrix object (always named "x")
makeCacheMatrix <- function(x = matrix()) {
        # and presets(resets) to NULL a m value (container for the invert matrix)
        m <- NULL
# 2) creates 4 minifunctions and group them in a list  
        # creates the minifunction "set" for setting/storing a value x and reset the m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # creates the minifunction "get" for accessing a set/stored value x
        get <- function() x
        # creates the minifunction "setinvert" for computing and storing x's invert value (that is the "m")
        setinvert <- function(solve) m <<- solve
        # creates the minifunction "getinvert" for accessing the already computed m (that is x's invert value) 
        getinvert <- function() m
        # put these mini-functions into a list fromw where they can be called
        # using the homonym (for not to get confused) list-elements' names:
        list (set = set, get = get,
              setinvert = setinvert,
              getinvert = getinvert)
        }


## We create a function whose input is an object of makeCacheMatrix type (x,m,list(set,get,setinvert,getinvert))
cacheSolve <- function(x, ...) {
        # and takes its "m" value by the list of the "x" (objet of makeCacheMatrix type)
        m <- x$getinvert()
        # if this "m"'s value is indeed something else than NULL (that means something that has been computed before
        # for the same given "x"), then it returns this precomputed/cached value with an information message
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # else access/get the x value
        data <- x$get()
        # compute its inverted value and name it "m"
        m <- solve(data, ...)
        # store it to the x$setinvert element of the makeCacheMatrix object
        x$setinvert(m)
        #In both scenarios return eventyally the "m"'s value (either cached or newly computed)
}
