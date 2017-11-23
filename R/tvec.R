
#' Return vector of lengths of list elements
#'
#' @param l List
#' @param atoms Vector of class names that lengths are assumed to be 1
#'
#' @return Vector of lengths
#' @export
getLens <- function(l, atoms = c("")) {

  lens <- numeric()

  for (i in 1:length(l)) {
    if (any(class(l[[i]]) == atoms)) {
      lens[i] <- 1
    } else {
      lens[i] <- length(l[[i]])
    }
  }

  return (lens)

}


#' Check that all params have length = 1 or same length > 1
#'
#' @param ... Any number of atoms, vectors, lists
#' @param atoms Vector of class names that lengths are assumed to be 1
#'
#' @return Maximum length of params
#' @export
getLen <- function(..., atoms = c("")) {

  lens <- getLens(list(...), atoms)
  maxLen <- max(lens)

  if (!all(lens == 1 | lens == maxLen))
    stop("ERROR! Vector length mismatch\n")

  return (maxLen)

}


#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any atom, vector, list
#' @param i Index of value
#' @param atoms Vector of class names that cannot be broken apart by index referencing
#'
#' @return Value with index i
#' @export
getElem <- function(a, i, atoms = c("")) {

  if (any(class(a) == atoms)) {

    b <- a

  } else {

    len <- length(a)

    if (i <= len)
      b <- a[[i]]
    else
      b <- a[[len]]

  }

  return (b)

}


#' Return value of object's attribute
#'
#' @param obj Object or list of objects
#' @param attrName Attribute name as character
#'
#' @return Attribute of object
#' @export
getAttr <- function(obj, attrName) {

  if (is.atomic(obj)) {

    a <- NA

  } else {

    if (class(obj) != "list") {

      a <- obj[[attrName]]
      if (is.null(a)) a <- NA

    } else {

      a <- list()
      len <- length(obj)

      for (i in 1:len) {
        a[[i]] <- obj[[i]][[attrName]]
        if (is.null(a[[i]])) a[[i]] <- NA
      }

      if (all(mapply(is.atomic, a))) a <- mapply(c, a)

    }

  }

  return (a)

}


#' Apply function to arguments that can be vectors and lists
#'
#' @param fun Function name
#' @param ... Argumetns that can be vectors and lists
#' @param atoms Vector of class names that cannot be broken apart by index referencing
#'
#' @return Function's result that can be vector or list
#' @export
applyFun <- function(fun, ..., atoms = c("")) {

  result <- list()

  len <- getLen(..., atoms = atoms)

  params <- list(...)

  for (i in 1:len) {

    localParams <- list()

    for (j in 1:length(params)) {
      localParams[[j]] <- getElem(params[[j]], i, atoms)
    }

    result[[i]] <- do.call(fun, localParams)

  }

  if (all(mapply(is.atomic, result))) result <- mapply(c, result)

  return (result)

}



