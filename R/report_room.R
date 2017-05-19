#' Applies a function on a dataframe, with specific response columns, and report on the specified variables from the output
#'
#' @param data.df
#' @param all.response.names.v
#' @param the.method
#' @param report.items.v
#'
#' @return
#' @export
#'
#' @examples
reportage <- function(data.df, all.response.names.v, the.method, report.items.v, ...) {

  the.method.name <- deparse(substitute(the.method))

  lapply(
    all.response.names.v,
    function(the.response.name) {
      writeLines('*******************')
      writeLines(paste(the.method.name, "for", the.response.name))
      writeLines('*******************')
      other.responses <- all.response.names.v[all.response.names.v != the.response.name]
      the.data.part <- dplyr::select(data.df,-one_of(other.responses))# %data.df[, !(names(data.df) %in% all.response.names.v[-eval(quote(the.response.name))])]
      result <- do.call(the.method, list(the.data.part, the.response.name) )
      lapply(
        report.items.v,
        function(the.report.itme){
          print(result[the.report.itme])
        }
      )
      writeLines('*******************\n\n\n')
    }
  )

}
