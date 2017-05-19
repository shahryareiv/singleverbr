
# from here: https://gist.github.com/aammd/9ae2f5cce9afd799bafb (by Hadley)
#' Converts a list to a dataframe with 'list.element' and 'names' columns
#'
#' @param listfordf
#'
#' @return
#' @export
#'
#' @examples
sv_list_to_df <- function(listfordf){

  if(!is.list(listfordf)) stop("it should be a list")

  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))

  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }

  df
}



#' Title
#'
#' @param data.df
#' @param the.levels
#' @param be.ordered
#'
#' @return
#' @export
#'
#' @examples
as.factor.dataframe <- function(data.df, the.levels = 5, be.ordered = TRUE){
  # as.data.frame(lapply(data.df, factor, levels = 1:the.levels))
  as.data.frame( lapply( data.df, function(x){factor(x, ordered = be.ordered)}) )
}


#' Title
#'
#' @param data.df
#'
#' @return
#' @export
#'
#' @examples
as.integer.dataframe <- function(data.df){

  as.data.frame( lapply( data.df, function(x){as.integer(x)}) )
}
