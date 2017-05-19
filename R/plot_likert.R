

#' #' Title
#' #'
#' #' @param data.df
#' #' @param miss.method
#' #' @param legend
#' #' @param caption
#' #' @param subcaption
#' #' @param method
#' #' @param order
#' #' @param type
#' #' @param coloring
#' #' @param diag
#' #' @param bg
#' #' @param the.name
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' uv_singleplot_corr_mat <- function(
#'   data.df,
#'   the.name='',
#'   ...
#' ){
#'
#'
#'   #check the args
#'   the.arg.list <- this_args.for_plot(remove.list = c('the.name'))
#'
#'   log_debug('arg list is:',the.arg.list)
#'
#'   #wrap the main function
#'   the_plot_func <- wrap_plot_call(uv_plot_corr_mat,the.name)
#'
#'   #call the wrapped with args
#'   the.out <- do.call(the_plot_func, the.arg.list)
#'
#'   #returns the_out that can be the corr.mat or the plot, depending on the settings (handle by the wrap_plot_call)
#'   return(the.out)
#'
#' }




uv_plot_likert <- function(the.data, the.n.levels=5, ...){

  # require(likert)
  #log_debug_var('liker.dots',...)
  log_debug_var('likert.data', the.data)
  the.plot <- tryCatch({
      the.res <- likert::likert(as.data.frame((the.data)), nlevels=the.n.levels,  ...)#
      log_debug_var('likert.res', the.res)
      plot(the.res, ordered = FALSE)
    },
    error=function(e){
      print(e)
      plot_zero("Error in likert ", e)
    }
  )

  print(the.plot)
  return(the.plot)

}

