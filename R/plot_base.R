

#' Wraps a plotting function with required device settings
#'
#' @return
#' @export
#'
#' @examples

wrap_plot_call <- function(plot_fun,the.name=''){




  #let's force it, so no problem with lazy eval
  log_debug('forcing the plot fun')
  force(plot_fun)

  #Wrapping closure
  log_debug('defining plot fun closure')
  cloj.plot_fun <- function(...){

    log_debug('in the wrapped cloj func')

    the.caller.name <- caller_name.str()

    #if I should skip this plot, then let me skip
    if (!is_in_whitelist(the.caller.name)) return()

    #some logging
    log_debug('continute in closure : ',the.caller.name)


    #fix the name if needed
    if (is.null(the.name)){
      the.name <- the.caller.name
    }

    #the data variable should be the first one
    the.arg.list <-  as.list(match.call(expand.dots = TRUE)[-1])
    log_debug_var("cloj.arg.list",the.arg.list)
    the.data <- the.arg.list[1]
    log_debug_var("cloj.data",the.data)
    print(the.data)

    #some logging
    log_debug('plot begining: ',the.caller.name)


    #prepare ploting device, as in settings
    begin_plot(the.name=the.name)

    #some logging
    log_debug('plot main: ',the.caller.name)#,'is',...

    #RUN!
    the.plot <- plot_fun(...)

    #some more logging
    log_where('plot ended: ',the.caller.name)

    #close the ploting device, as in settings
    end_plot()

    #return data or plot, depending on the settings
    return.mode <-uv_setting()$plot.return.mode
    if (return.mode=='plot'){
      return(the.plot)
    }else{#data?
      return(the.data)
    }


  }

  #return the closure
  return(cloj.plot_fun)

}



this_args.for_plot <- function(n=2,remove.list){


  cl <- sys.call(-1)
  f <- get(as.character(cl[[1]]), mode="function", sys.frame(-2))
  cl <- match.call(definition=f, call=cl)
  the.arg.list <-  as.list(cl)[-1]
  the.arg.list <-  the.arg.list[which(!(names(the.arg.list) %in% remove.list))]
  return(the.arg.list)

}


single_of <- function(the.data, plot_func, the.name='',...){


  #force it
  force(the.data)

  #check the args
  #the.arg.list <- this_args.for_plot(remove.list = c('plot_func','the.name'))
  the.arg.list <- c(the.data=list(the.data), list(...))#environment() the.name=list(the.name),


  log_debug_var('single.arg.list',the.arg.list)
  log_debug_var('single.data',the.data)


  #wrap the func
  the_wrapped_plot_func <- wrap_plot_call(plot_func, the.name = the.name)

  #call the wrapped with args
  the_out <- do.call(the_wrapped_plot_func, the.arg.list)


  #returns the_out that can be the corr.mat or the plot, depending on the settings (handle by the wrap_plot_call)
  return(the_out)
}

groupby_of <- function(
  the.data,
  plot_func,
  the.name = '',
  the.groupby = NULL,
  the.mode = c('separate','table'),
  the.mode.flow = c('top-down','left-right','right-left'),
  the.mode.flow.cols = NULL,
  the.mode.flow.rows = NULL,
  ...
){

  #check the input
  the.mode <- match.arg(the.mode)

  #you had to call single_of instead of me! I will do it for you
  if (is.null(the.groupby)){
    return(single_of(the.data, plot_func, the.name, ...))
  }


  #check the args
  # the.arg.list <- this_args.for_plot(remove.list = c(
  #       'the.data','plot_func','the.name','the.groupby','the.mode','the.mode.flow','the.mode.flow.cols','the.mode.flow.rows'))
  the.arg.list <- c(the.data=list(the.data), list(...))#environment() the.name=list(the.name),

  #make groups
  data.groups.ls <- the.data %>%
  dplyr::group_by(case) %>%
  dplyr::do(groups=(.))

  data.groups.ls <- data.groups.ls[["groups"]]

  #apply plots to each group
  the.out <- lapply(data.groups.ls, function(x){
    the.group.name <- x[[1,the.groupby]]# %>% dplyr::summarise("case")#dplyr::select(one_of(c("case")))
    file.name <- paste(the.name,the.group.name,sep = "_")
    x <- dplyr::select(x,-one_of(c(the.groupby)))
    log_debug_var('groupby.x',x)

    single_of(the.data=x, plot_func, the.name=file.name, ...)
  })


  #returns the_out that can be the corr.mat or the plot, depending on the settings (handle by the wrap_plot_call)
  return(the.out)
}

plot_zero <- function(...){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste(...,sep=""),cex = 1.6, col = "black")
}


begin_single_plots <- function(){

}
end_single_plots <- function(){

}

begin_page_plots <- function(){

}
end_page_plots <- function(){

}


#' Begin a plot
#'
#' @param the.name
#' @param g_hand
#'
#' @return
#'
#' @examples
begin_plot <-function(the.name, g_hand=NULL){


  #no_plot<-attr(corr.mat,'no_corrplot')
  #if (!is.null(no_plot) && no_plot==TRUE) return(corr.mat)

  the.setting <- uv_setting()

  #Create a filename
  ##add filename prepend
  the.name <- paste(the.setting$plot.filename.prepend,the.name,sep='')
  ##add filename timestap
  if (the.setting$plot.filename.timestamp){
    the_date <- lubridate::now()
    stamp <- format.Date(a,format="_%y%m%d_%H%M")
    the.name <- paste(the.name,stamp,sep='')
  }
  ##Hey! we are here!
  log_debug('I am going to draw:',the.name)


  #if only an all-in-one plot needed then we do not need
  if (the.setting$plot.plot.mode!='single'){
    log_debug('But, I am not going to open a device for: ',the.name)
    return()
  }else{
    log_debug('Single draw for: ',the.name)
  }



  if (is.null(g_hand)){
    log_debug ('You did not provide a separate graphic device, so I would make one for the ',caller_name.str())
  }



  #If separate figures are required (not a whole)


    if (the.setting$plot.device=='pdf'){

      the.pdf.file.name<-paste(the.name,'.pdf',sep='')

      log_debug('I am going to draw: ',the.pdf.file.name)

      #pdf(the_file_name ,paper = "a4",font="sans",pointsize = 3.5,width = 8,height = 10)
      pdf(the.pdf.file.name,pointsize = the.setting$plot.pdf.pointsize )#,xaxs=0,yaxs=0 #,family="Garamond"
      #embed_fonts(the_file_name)
      # CairoPDF(
      #       paper = "a4",
      #       font="sans",
      #       pointsize = 3.5,
      #       # surface = 'pdf',
      #       # width = 7,
      #       # height = 7,
      #       # units="in",
      #       # dpi = 72
      #       file='correlations.pdf'
      # )
      # par(mfrow=c(2,2) )#,mai = rep(.1,4), pin = c(0.5, 0.3),oma=c(0.1,1,1,1) ,pin = c(0.5, 0.3)

      #par(oma=c(1,1,10,1))
      par(xaxs="i", yaxs="i")
      #setPagenum(1)

    }else if (the.setting$plot.device=='tikz'){

      the.tikz.file.name<-paste(the.name,'.tikz.tex',sep='')

      tikzDevice::setTikzDefaults()
      options(tikzDocumentDeclaration = "% !TEX encoding = UTF-8 Unicode\n\\PassOptionsToPackage{usenames,dvipsnames,svgnames,table}{xcolor}\n\\documentclass[tikz]{standalone}")#,convert={outfile=\\jobname.svg}
      options(tikzLatexPackages =c( "\\usepackage{my_graphics_v2}"))
      #options(tikzDefaultEngine = 'luatex')#PROBLEM
      #options(tikzMetricPackages =c("\\usepackage[T1]{fontenc}\n","\\usetikzlibrary{calc}\n"))


      log_debug('I am going to draw:',the.tikz.file.name)


      tikzDevice::tikz(the.tikz.file.name,standAlone = TRUE, width = the.setting$plot.tikz.width,height = the.setting$plot.tikz.height,sanitize=TRUE )

    }else if (the.setting$plot.device=='png'){

      the.png.file.name<-paste(the.name,'.png',sep='')

      log_debug('I am going to draw:',the.png.file.name)

      dpi<-the.setting$plot.png.dpi
      CairoPNG(the.png.file.name)#,paper = "a4",font="sans",pointsize = 24,width = dpi*8.27,height = dpi*11.69

    }

}



#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
end_plot <-function(){

  the.setting <- uv_setting()
  the.name <- caller_name.str()

  if (the.setting$plot.plot.mode=='single'){
    dev.off()
  }else{
    log_debug('ENPlot: I do not need to end device for: ',the.name)
  }

}






#' Grabs the last drawing as a Grob and returns it (to be used by other Grob things)
#'
#' @return a grob
#' @export
#'
#' @examples
grab_grob <- function(){

  grid.echo()
  the_grabed<-grid.grab()
  plot.new()
  return (the_grabed)

}







#Define functions for all in one drawings
#' Title
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
begin_plot_all<-function(type='pdf'){
  if (type=='pdf'){

    ## PDF
    pdf('correlations.pdf',paper = "a4",font="sans",pointsize = 3.5,width = 8,height = 10)
    # CairoPDF(
    #       paper = "a4",
    #       font="sans",
    #       pointsize = 3.5,
    #       # surface = 'pdf',
    #       # width = 7,
    #       # height = 7,
    #       # units="in",
    #       # dpi = 72
    #       file='correlations.pdf'
    # )
    # par(mfrow=c(2,2) )#,mai = rep(.1,4), pin = c(0.5, 0.3),oma=c(0.1,1,1,1) ,pin = c(0.5, 0.3)
    par(oma=c(1,1,10,1))
    setPagenum(1)

  }else if (type=='tikz'){

    # tikz
     #tikz('correlations.tikz.tex',standAlone = TRUE, width = 8.27,height = 11.69,sanitize=TRUE )
     #my_Scenario_1()
    #dev.off()
    #

  }else if (type=='svg'){

    # SVG
    # svg('correlations.svg',width = 8.27,height = 11.69)
    # my_Scenario_1()
    # dev.off()
    #

  }else if (type=='png'){

    # PNG
    # dpi<-300
    # CairoPNG('correlations.png',paper = "a4",font="sans",pointsize = 24,width = dpi*8.27,height = dpi*11.69)
    # my_Scenario_1()
    # dev.off()

  }

}

end_plot_all<-function(){
  dev.off()
}


