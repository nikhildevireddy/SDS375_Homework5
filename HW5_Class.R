## HW5 Class/Methods


setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  len  <- object@length
  pos  <- object@pos
  vals <- object@value
  
  ## length: single non-negative integer
  if (length(len) != 1L || is.na(len) || len < 0L) {
    return("slot 'length' must be a single non-negative integer")
  }
  
  ## value and pos must match in length
  if (length(vals) != length(pos)) {
    return("slots 'value' and 'pos' must have the same length")
  }
  
  ## if there are positions, check they are valid indices
  if (length(pos)) {
    if (anyNA(pos)) {
      return("slot 'pos' cannot contain NA")
    }
    if (any(pos < 1L) || any(pos > len)) {
      return("slot 'pos' must be between 1 and 'length'")
    }
  }
  
  TRUE
})


## Coercion methods

## numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  n <- length(from)
  if (n == 0L) {
    return(new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = 0L))
  }
  nz <- which(from != 0)
  new("sparse_numeric",
      value  = if (length(nz) > 0L) from[nz] else numeric(0),
      pos    = as.integer(nz),
      length = as.integer(n))
})

## sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L) {
    out[from@pos] <- from@value
  }
  out
})

## Helper method that checks matching length

.check_same_length <- function(x, y) {
  if (x@length != y@length) {
    stop("sparse_numeric objects must have the same 'length'")
  }
}

## Generics

setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_sub",  function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))


## sparse_add, sparse_mult, sparse_subm sparse_crossprod

## Addition
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            
            # both all-zero
            if (length(x@pos) == 0L && length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            
            # combine positions and values, then sum by position
            all_pos <- c(x@pos, y@pos)
            all_val <- c(x@value, y@value)
            
            sums <- tapply(all_val, all_pos, sum)
            
            pos  <- as.integer(names(sums))
            vals <- as.numeric(sums)
            
            nz <- vals != 0
            if (!any(nz)) {
              new("sparse_numeric",
                  value  = numeric(0),
                  pos    = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value  = vals[nz],
                  pos    = pos[nz],
                  length = x@length)
            }
          })



## Subtraction
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            
            if (length(x@pos) == 0L && length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            
            all_pos <- c(x@pos, y@pos)
            all_val <- c(x@value, -y@value)
            
            diffs <- tapply(all_val, all_pos, sum)
            
            pos  <- as.integer(names(diffs))
            vals <- as.numeric(diffs)
            
            nz <- vals != 0
            if (!any(nz)) {
              new("sparse_numeric",
                  value  = numeric(0),
                  pos    = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value  = vals[nz],
                  pos    = pos[nz],
                  length = x@length)
            }
          })



## Multiplication
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            
            if (length(x@pos) == 0L || length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            
            pos_common <- intersect(x@pos, y@pos)
            if (length(pos_common) == 0L) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            
            idx_x <- match(pos_common, x@pos)
            idx_y <- match(pos_common, y@pos)
            vals  <- x@value[idx_x] * y@value[idx_y]
            
            nz <- which(vals != 0)
            if (length(nz) == 0L) {
              new("sparse_numeric",
                  value  = numeric(0),
                  pos    = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value  = vals[nz],
                  pos    = as.integer(pos_common[nz]),
                  length = x@length)
            }
          })

## Cross product
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            
            if (length(x@pos) == 0L || length(y@pos) == 0L) {
              return(0)
            }
            pos_common <- intersect(x@pos, y@pos)
            if (length(pos_common) == 0L) {
              return(0)
            }
            idx_x <- match(pos_common, x@pos)
            idx_y <- match(pos_common, y@pos)
            sum(x@value[idx_x] * y@value[idx_y])
          })

## Operator methods (+, -, *)

setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_mult(e1, e2)
          })

## show() method

setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat(" Length: ", object@length, "\n", sep = "")
  cat(" Non-zero entries:", length(object@value), "\n")
  if (length(object@pos) > 0L) {
    df <- data.frame(
      pos   = object@pos,
      value = object@value
    )
    print(df, row.names = FALSE)
  } else {
    cat(" (all entries are zero)\n")
  }
})

## plot() method

setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    plot(x@pos, x@value, pch = 16, col = "blue",
         xlab = "Position", ylab = "Value",
         main = "Sparse vectors (x in blue, y in red)", ...)
    if (length(y@pos))
      points(y@pos, y@value, pch = 17, col = "red")
    invisible(NULL)
  }
)


## Additional method: length of sparse_numeric vectors

setMethod("length", "sparse_numeric", function(x) {
  x@length
})



