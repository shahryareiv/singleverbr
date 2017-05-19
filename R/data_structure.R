
# from here: https://gist.github.com/aammd/9ae2f5cce9afd799bafb (by Hadley)
#' Converts a list to a dataframe with 'list.element' and 'names' columns
#'
#' @param listfordf
#'
#' @return
#' @export
#'
#' @examples
uv_list_to_df <- function(listfordf){

  if(!is.list(listfordf)) stop("it should be a list")

  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))

  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }

  df
}
