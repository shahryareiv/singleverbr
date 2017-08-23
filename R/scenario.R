#' Keep track of the active scenarios name and give a clojure to check them later
#'
#' @param ...
#' a list of scenarios (probably their names)
#'
#' @return
#' a clojure function that gets a list of scenarios (probably their names) and return TRUE/FALSE if they are in the set
#' @export
#'
#' @examples
#' is_there <- sv_set_scenario('a', 'b')
#' print(is_there('a')) # prints TRUE
sv_set_scenario <- function(
  ...
){

  the.scenarios = c(...)

  cloj.is_scenario <- function(...){

    some.scenarios <-c(...)
    result <- any(some.scenarios %in% the.scenarios)
    return(result)

  }

  return(cloj.is_scenario)

}
