#' Finding out who called the parent function
#'
#' @return caller of the parent function (i.e call of caller)
#' @export
#'
#' @examples
caller_name.str <- function(n=2){

  #What is the name of caller function? (actually caller of the caller)
  the.name <- '(no name!)'
  the.name <- tryCatch({
    toString(sys.call(sys.parent(n=n))[[1]])#match.call()#sys.call(-1)#parent.frame()#deparse(sys.calls()[[sys.nframe()-1]])
    #print(paste('found a name!!!:n=',n,'',the.name))
  },
  error = function(e){
    if (n<=8){
      #print (paste('err: for n=',n,'up!'))
      return(paste('^_', caller_name.str(n+7), sep=''))
    }else{
      return('(err lost name!)')
    }
  },
  warning = function(w){
    if (n<=8){
      #print (paste('wrn: for n=',n,'up!'))
      return(paste('^+', caller_name.str(n+7), sep=''))
    }else{
      return('(warn lost name!)')
    }
  }
  #,
  #finally = {}
  )#closures make problem




  return(the.name)

}


is_in_whitelist <- function(the.name){#ISW:

  #so, what is the white list
  the.white.list <- uv_setting()$func.white.list

  #a null white list means all are white
  if(!is.null(the.white.list)){

    #is the parent inside the white list or not?
    if(the.name %in% the.white.list){
      log_debug('found ',the.name,' in the white list.')
      return(TRUE)

    }else{
      log_debug('Could not find ',the.name,' in the white list. Will skip.')
      return(FALSE)
    }

  }

  log_debug('no white list found, so ',the.name,' defults to be in the white list.')
  return(TRUE)

}



#' Does some wrapping
#'
#' @param the_func
#'
#' @return
#' @export
#'
#' @examples
func_wrapper <- function(calc_fun){

  #let's force it, so no problem with lazy eval
  force(calc_fun)

  return(memoise::memoise(calc_fun))

}
