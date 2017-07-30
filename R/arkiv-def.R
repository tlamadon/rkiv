#' set of functions that always reload and store the
#' main archive, this makes is simpler
#'
#' I chose not to implement this trhough optional
#' argument just because I wanted the first argument
#' to be the rk in the other functions


#' get rkiv from default location
rkiv0.cfg <- function() {
  return(rkiv())
}

#' start a resoource at default location
#' @export
rkiv0.start <- function(...) rkiv.start(rkiv0.cfg(),...)

#' start a resoource at default location
#' @export
rkiv0.list <- function() rkiv0.cfg()

#' start a resoource at default location
#' @export
rkiv0.adddep <- function(...) rkiv.adddep(rkiv0.cfg(), ... )

#' start a resoource at default location
#' @export
rkiv0.put <- function(...) rkiv.put(rkiv0.cfg(),...)

#' start a resoource at default location
#' @export
rkiv0.load <- function(...) rkiv.load(rkiv0.cfg(),...)

#' start a resoource at default location
#' @export
rkiv0.list <- function(...) rkiv0.cfg()
