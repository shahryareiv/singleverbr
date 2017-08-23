
#' Applies a function on a dataframe, with specific response columns, and reports on the specified variables from the output
#'
#' @param data.df
#' data input
#' @param all.response.names.v
#' the variables in the data inputh which are the responses
#' @param the.method
#' the method to be called
#' @param report.items.v
#' what items to be returned back from the the.method result (specific to the.method)
#' @param draw.plots
#' will draw plots
#' @param plots.prefix
#' what name attached at the begining of plot files
#' @param ...
#' @param response.selection
#' extra variables to be pass to the the.method
#' @return
#' @export
#'
#' @examples
sv_reportage <- function(
  data.df,
  all.response.names.v,
  the.method,
  report.items.v,
  response.selection = c('all.out.except.one','all.in.except.one'),
  draw.plots = FALSE,
  plots.prefix = NULL,
  ...
) {


  response.selection <- match.arg(response.selection)


  the.method.name <- deparse(substitute(the.method))

  all.results <- lapply(
    all.response.names.v,
    function(the.response.name) {

      # writeLines('*******************')
      # writeLines(paste(the.method.name, "for", the.response.name))
      # writeLines('*******************')

      other.responses <- all.response.names.v[all.response.names.v != the.response.name]

      if (response.selection == 'all.out.except.one'){
        the.data.part <- dplyr::select(data.df,-one_of(other.responses))# %data.df[, !(names(data.df) %in% all.response.names.v[-eval(quote(the.response.name))])]
      }else if (response.selection == 'all.in.except.one'){
      }else{
        stop("Only one of the valid response.selection should be choosed")
      }

      result <- do.call(the.method, list(the.data.part, the.response.name, ...) )

      result.report <- lapply(
        report.items.v,
        function(the.report.item){
          # print(result[[the.report.item]])
          return(result[[the.report.item]])
        }
      )

      # writeLines('*******************\n\n\n')

      return(result.report)
    }
  )

  return(all.results)

}

