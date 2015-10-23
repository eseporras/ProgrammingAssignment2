## I create two functions: one creates a special object that can store its inverse and the other just calculates the inverse of a square matrix taking the previous special object as input and verifying that the inverse hasn't been calculated before.   

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() {             # 1.get the value of the matrix.
                x
        }
        setsolve <- function(solve){    # 2.set the value of the inverse matrix. 
                s <<- solve
        }
        getsolve <- function() {        # 3.get the value of the inverse matrix, if it exists.
                s
        }
        list(get = get,                 # 4.return a list with the three elements defined above. 
             setsolve = setsolve, 
             getsolve = getsolve)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()               # 1.first check if an inverse matrix has already been calculated. If that's so, print a message on screen, return the value and exit the function. 
        if(!is.null(s)){
                message("Getting cached data...")       
                return(s)
        }
        input <- x$get()                # 2.if there is no cached inverse, then get the matrix and store it in an object called "input"
        s <- solve(input, ...)          # 3.solve the matrix and store the result in "s"
        x$setsolve(s)                   # 4.set the "setsolve" element in the "x" list to the inverse matrix, so it is cached.
        s                               # 5.finally, return the newly calculated inverse matrix
}

############    Testing....   ###########

# First I create a matrix M of 20x20 random numbers

M <- matrix(runif(25),5,5)

# Then I store it as a cached matrix in the object cM

cM <- makeCacheMatrix(M)

# Finally, I solve the cached matrix with my makeCacheMatrix function. 

cacheSolve(cM)

# If I re-run the line of command above, I should get a message stating that the data used is actually cached. 

# I can empty the cached data for cM by running this line of code: 

cM$setsolve(NULL)

# If I run cacheSolve on cM again, I should not get the "Getting cached data..." message. 