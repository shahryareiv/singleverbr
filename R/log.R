
#Time and date things

# L O G =====




#' Show the debug message
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
log_debug<-function(...){

  is.debug.mode <- uv_setting()$debug.mode

  if (is.debug.mode){

    #to avoid clusures that have no name
    the.name <-caller_name.str(2)

    cat(paste('\n>>>>> ',the.name ,' ',...,'<<<<<\n'))

  }
}


#' Title
#'
#' @param x
#' @param value
#'
#' @return
#' @export
#'
#' @examples
log_debug_var<-function(x,value){

  is.debug.mode <- uv_setting()$debug.mode

  if (is.debug.mode){

    assign(paste('debug_',x,sep = ''),value,envir = .GlobalEnv)
    print(paste("debug assigned",x))

  }
}



#' Show where you are
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
log_where<-function(...){
  cat(paste('\n* Here at: ',caller_name.str(2),...,'*\n'))
}



#' Show the output nicely
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
log_output<-function(msg){
  cat("\n\n\n+----------------------------------------------+")
  cat('\n|                    Result                    |')
  cat('\n+----------------------------------------------+\n')
  print(msg)
  cat('\n+------------------End of Result---------------+\n\n\n')

}


