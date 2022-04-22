# This function is used to inverse a matrix and return the matrix inversion result from cache.

# I use a package matlib to make inversion calculator.

install.packages("matlib")
library(matlib)

# Create a special vector to store the matrix data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv_matrix <- function(inv) m <<- inv
        get_inv_matrix <- function() m
        list(set = set, get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}

# Create an object to store the inversion of matrix in cache and return its value

cacheSolve <- function(x, ...) {
        m <- x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inv(data, ...)
        x$set_inv_matrix(m)
        m
}

# Test the function
right <- makeCacheMatrix(matrix(rnorm(25),nrow=5)) # Create a random matrix has the same number of columns and rows
right$get() # See the matrix

det(right$get()) # Calculate the determinant

det(right$get()) != 0 # Check the determinant is not equal zero, so it can be inversible

cacheSolve(right) # Inverse the matrix

cacheSolve(right) # Calling the matrix inversion from cache
