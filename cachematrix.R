################### Short description ################### 
## makeCacheMatrix() creates a list of functions that can cache an inverse of matrix x
## cacheSolve() calculates inverse of matrix x;
##              if inverse is calculated, it returns cached inverse value
#########################################################

################### FUNCTION START ################### 

## assign output of makeCacheMatrix() to some arbitraty value %NAME%
## %choose_your_name% to be used as input for cacheSolve()

## uncomment matrix_test to create 
## a 5x5 matrix with random integers from 0 to 20 for dummy calculations;
matrix_test <- matrix(sample.int(20, size = 5*5, replace = TRUE), 5, 5)


################### Description of makeCacheMatrix() ################### 
## makeCacheMatrix() creates a list of four functions
## set(), get(), set_inverse() and get_inverse()
## taking some matrix x as an argument
########################################################################

makeCacheMatrix <- function(x = matrix()) {        
        m <- NULL
        set <- function(y) { ## set a matrix for calculating inverse
                             ## set a new matrix using %NAME%$set(y = matrix())
                x <<- y
                m <<- NULL
                print("assigning matrix for calculating inverse")
        }
        get <- function() x    ## return matrix used for inverse calculation                         
        set_inverse <- function(inverse) m <<- inverse 
        ## set set_inverse as inverse of matrix
        get_inverse <- function() m ## get inverse of matrix, else return "NULL"
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
print("set output as %NAME%, use %NAME% as input for cachesolve")
        
}

################### Description of cacheSolve() ################### 
## cacheSolve() takes output of makeCacheMatrix() and 
## a) computes inverse of x
## b) if inverse is computed, cacheSolve outputs cached inverse of x
###################################################################

cacheSolve <- function(x, ...) {
        m <- x$get_inverse() ## assigns value of inverse to m
        if(!is.null(m)) { ## if m has value assigned, 
                          ## write "getting cached data" and print m
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## if m is NULL, calculate inverse with solve()
        x$set_inverse(m)
        m
}
