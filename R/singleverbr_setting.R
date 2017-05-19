# S E T T I N G =====


#' Closure factory for uv_set()
#'
#' @return
#' @export
#'
#' @examples
setting.fun <- function(){

  # creat an initial setting in the parent environment
  initial.setting.ls <- list(
    func.white.list=NULL,#an empty list means all functions
    debug.mode = TRUE,
    plot.return.mode = 'data',#plot
    plot.plot.mode = 'single',#c('single','page','multi-page')
    plot.filename.prepend = 'uvon_',
    plot.device = 'pdf',#c('viewport','pdf','tikz','png'),
    plot.device.external = NULL,
    plot.filename.timestamp = FALSE,
    plot.all.in.one = FALSE,
    plot.single.mar = c(0,0,0,0),
    plot.pdf.pointsize = 3.5,
    plot.png.dpi = 300,
    plot.tikz.width = 8.27,
    plot.tikz.height = 11.69
  )

  internal.setting.ls <- initial.setting.ls

  # create a closure that takes new settings and merge it with the initial setting the parent environment
  cloj.setting <- function(...){
    new_setting.ls <- as.list(match.call(expand.dots = TRUE)[-1])
    internal.setting.ls <<- modifyList(internal.setting.ls,new_setting.ls)
    return(internal.setting.ls)
  }

  return(cloj.setting)

}

#' Setup the UVON package
#'
#' @return
#' @export
#'
#' @examples
#'
#' #just return the closure
uv_setting <- setting.fun()

