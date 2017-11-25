
#' Return TRUE if variable is vector or Date
#'
#' @param v Any variable
#'
#' @return TRUE if variable is vector or Date
#' @export
isVector <- function(v) {
  is.vector(v) || (class(v) == "Date")
}


#' Return vector of lengths of list elements
#'
#' @param l List
#'
#' @return Vector of lengths
#' @export
getLens <- function(l) {

  lens <- numeric()

  for (i in 1:length(l)) {
    if (isVector(l[[i]])) {
      lens[i] <- length(l[[i]])
    } else {
      lens[i] <- 1
    }
  }

  return (lens)

}


#' Check that all params have length = 1 or same length > 1
#'
#' @param ... Any number of vectors
#'
#' @return Maximum length of params
#' @export
getLen <- function(...) {

  lens <- getLens(list(...))
  maxLen <- max(lens)

  if (!all(lens == 1 | lens == maxLen))
    stop("ERROR! Vector length mismatch\n")

  return (maxLen)

}


#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any vector
#' @param i Index of value
#'
#' @return Value with index i
#' @export
getElem <- function(a, i) {

  if (isVector(a)) {

    len <- length(a)

    if (i <= len)
      b <- a[[i]]
    else
      b <- a[[len]]

  } else {

    b <- a

  }

  return (b)

}


#' Return value of object's attribute
#'
#' @param obj Object or list of objects
#' @param attrName Attribute name as character
#'
#' @return Attribute value or values
#' @export
getAttr <- function(obj, attrName) {

  if (is.object(obj)) {

    a <- obj[[attrName]]
    if (is.null(a)) a <- NA

  } else if (is.list(obj)) {

    a <- list()
    len <- length(obj)

    for (i in 1:len) {
      a[[i]] <- obj[[i]][[attrName]]
      if (is.null(a[[i]])) a[[i]] <- NA
    }

    if (all(mapply(is.atomic, a))) a <- mapply(c, a)

  } else {

    a <- NA

  }

  return (a)

}


#' Apply function to arguments that can be vectors
#'
#' @param fun Function name
#' @param ... Argumetns that can be vectors
#'
#' @return Function's result or results as vector
#' @export
applyFun <- function(fun, ...) {

  result <- list()

  len <- getLen(...)

  params <- list(...)

  for (i in 1:len) {

    localParams <- list()

    for (j in 1:length(params)) {
      localParams[[j]] <- getElem(params[[j]], i)
    }

    result[[i]] <- do.call(fun, localParams)

  }

  if (all(mapply(is.atomic, result))) result <- mapply(c, result)

  return (result)

}


