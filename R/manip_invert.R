#' Title
#'
#' @param data.df
#'
#' @return
#' @export
#'
#' @examples
sv_invert_likert <- function(data.df, max){
  result <- max - data.df + 1
  return(result)
}
