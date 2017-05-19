

# For each plotting there are:
# 1. A core plotting function that gets parameters and return a plot
#   - the core plotting tries to set default parameters in its input arguments so
#     there is nothing fixed
# 2. An interface function that takes some limited parameters, make a wrapped function and calls it
#   - the interface plotting takes three dots ... and transfer them to actual ploting so
#     even the uknown parameters can reach the function
# 3. A wrap-maker function for plots, called by the interface function. This will do lots
#    of house keeping, debug, performance, graphic device setting things

# U T I L S ====


# C O R R E L A T I O N S ====




## sv_plot_corr_mat.in_mat-----


#' #' Plots 1-figure of correlation
#' #'
#' #'
#' #' @param corr.mat
#' #' @param corr.sig.mat
#' #' @param legend
#' #' @param caption
#' #' @param subcaption
#' #' @param plot.method
#' #' @param order
#' #' @param type
#' #' @param coloring.method
#' #' @param diag
#' #' @param bg
#' #' @param filename
#' #' @param ...
#' sv_singleplot_corr_mat.in_mat <- function(
#'   corr.mat,
#'   corr.sig.mat,
#'   filename='',
#'   ...
#' ){
#'
#'
#'   #check the args
#'   the.arg.list <- this_args.for_plot(remove.list = c("filename"))
#'
#'   #wrap the func
#'   the_plot_func <- wrap_plot_call(sv_plot_corr_mat.in_mat, the.name = filename)
#'
#'   #call the wrapped with args
#'   the_out <- do.call(the_plot_func, the.arg.list)
#'
#'   # the_out <- the_plot(
#'   #   corr.mat,
#'   #   corr.sig.mat,
#'   #   legend = legend,
#'   #   caption = caption,
#'   #   subcaption = subcaption,
#'   #   plot.method = plot.method,
#'   #   order = order,
#'   #   type = type,
#'   #   coloring.method = coloring.method,
#'   #   diag = diag,
#'   #   bg = bg,
#'   #   ...
#'   # )
#'
#'   #returns the_out that can be the corr.mat or the plot, depending on the settings (handle by the wrap_plot_call)
#'   return(the_out)
#'
#' }

#the core plot
sv_plot_corr_mat.in_mat <- function(
  corr.mat,
  corr.sig.mat,
  plot.method = c('shade','circle'),
  order = c('original'),
  type = c("full"),
  coloring.method = c('diverging-map'),
  diag = TRUE,
  legend = '',
  caption = '',
  subcaption = '',
  bg = NULL,
  tck = -0.1,
  tl.col = "black",
  tl.cex = 1.5,
  number.cex = 1.0,
  shade.lwd = 0.01,
  shade.col = "black",
  the.mar = c(0,0,0,0),#sv_setting()$plot.single.mar
  bty = 'u ',
  insig = "blank",
  ...

){

  #check the input
  plot.method <- match.arg(plot.method)
  coloring.method <- match.arg(coloring.method)

  log_debug("Just before corrplot")

  the.plot <- tryCatch({
  corrplot::corrplot(
    corr.mat,
    p.mat = corr.sig.mat,
    method =plot.method,#method,  shade,color
    type = type,#"full",#"lower", type
    insig = insig,
    outline = FALSE,
    diag = diag,
    col = color_pallete(10,type=coloring.method),
    order = order,#order,AOE,hclust,FPC,
    #hclust.method = 'average', #for hclust
    #addrect = 3 #for hclust
    #bg=bg,
    title = caption,
    mar = the.mar,
    tck = tck,
    tl.col = tl.col,
    tl.cex = tl.cex,
    number.cex = number.cex,
    shade.lwd = shade.lwd,
    shade.col = shade.col,
    bty = bty,
    ...
  )}#, method="circle",'pie' order ="AOE".mixed , order="hclust",addrect=2,
  ,error=function(e){
      plot_zero("Error in corrplot ",e)
  }
  )

  log_debug("Just after corrplot")


  return(the.plot)

}


## plot_corr_mat -----


sv_plot_corr_mat <- function(
  the.data,
  calc.method = c("kendall","spearman","pearson"),
  conf.level=0.95,
  miss.method=c("pairwise.complete.obs", "everything", "complete.obs", "all.obs", "na.or.complete"),
  group_by=NULL,
  ...
){

  #we do this crazy thing because late more complicated the.data can be
  data.df <- the.data

  log_debug("here we have this data lost")
  log_debug_var("uvplot.corr.mat",the.data)

  #check the input
  calc.method <- match.arg(calc.method)
  miss.method <- match.arg(miss.method)

  log_debug_var("calc.method",calc.method)
  log_debug_var("miss.method",miss.method)

  tryCatch(
    {
      #calculate the correlation matrix and correlation significance matrix
      the.corr.mat <- cor(data.df, method= calc.method, use=miss.method)
      log_debug_var("the.corr.mat",the.corr.mat)
      the.corr.sig.mat <- sv_corr_sig.mat(data.df,conf.level = conf.level,method=calc.method)[[1]]#it gives a list as output
      log_debug_var("the.corr.sig.mat",the.corr.sig.mat)


      #call the method
      sv_plot_corr_mat.in_mat(
        corr.mat = the.corr.mat,
        corr.sig.mat = the.corr.sig.mat,
        ...
      )

    },
    error = function(e){
      plot_zero("Error in Corr (cor,cor.test, or corrplot) ",e)
    }
  )


}



#NOTE: data and mapping should not change name, becuase it is wrapped
sv_plot_corr_scatter.noplot <- function(data, mapping, low = "#132B43", high = "#56B1F7", ...) {

  cat_data <- data.frame(lapply(data, function(x) factor(x, ordered = TRUE)))

  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  xy.df <-data.frame(x=x,y=y)

  x_cat <- cat_data[[deparse(mapping$x)]]
  y_cat <- cat_data[[deparse(mapping$y)]]
  xy_cat.df <-data.frame(x=x_cat,y=y_cat)


  the.plot <- ggplot2::ggplot(data = xy.df, mapping = ggplot2::aes(x=x, y=y)) +#mapping = mapping,aes(label="2")
    #geom_bin2d(...) +
    #geom_point(data=xy.df[,1],...,color="red",size=0.05,alpha=0.02) +#,shape="."
    #geom_point(data=xy.df[,2],...,color="blue",size=0.05,alpha=0.02) +#,shape="."
    #geom_point(mapping=aes(color=x),...,size=0.05,alpha=0.02) + #,shape="."
    ggplot2::geom_jitter(size=0.5,alpha=0.2,stroke=0)+
    ggplot2::geom_smooth(...,n=20) + #default n=80 creates too big Tikz
    ggplot2::theme_light()+
    #theme_tufte() +
    ggplot2::theme(legend.position = "none",
            panel.grid.major = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank())
    #scale_fill_gradient(low = low, high = high)

    return(the.plot)
}



#NOTE: data and mapping should not change name, becuase it is wrapped
sv_plot_corr_coef.noplot <- function(data, mapping, the.method = "kendall", the.use="complete.obs" , low = "#132B43", high = "#56B1F7", ...) {


  if (sv_corr_test.pair(x, y, conf.level = 0.95, method=the.method) < 0.5){

    cor <- round( cor(data, method = mapping, use = the.use )[deparse(mapping$x),deparse(mapping$y)],digits = 2 )

  }else{
    cor <- ""
  }

  the.plot <- GGally::ggally_text(
    label=cor,
    color=cor,
    #mapping=aes(color=cor),

    ...
  )+
  #theme_light()+
  ggplot2::theme(legend.position = "none",
          panel.grid.major = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank()) #element_rect(linetype = "dashed", colour = "black", fill = NA))
  #+
  #scale_fill_gradient(low = low, high = high)

  #ggplot2::geom_text(
  #  data = data.frame(
  #    x = min(x, na.rm = TRUE),
  #    y = max(y, na.rm = TRUE),
  #    lab = round(cor, digits = 2)
  #  ),
  #  mapping = ggplot2::aes(x = x, y = y, label = lab, color = NULL),
  #  hjust = 0, vjust = 1,
  #  size = 1, fontface = "bold"
  #)
  #ggplot(data = data, mapping = the.mapping) +#mapping = the.mapping,aes(label="2")
   # geom_text(mapping=aes(xlab="2"),...)

  return(the.plot)
}


#Pairs
sv_plot_corr_pair <- function(the.data, ...) {

  the.plot<-GGally::ggpairs(
    the.data,
    #upper = list(
    #  continuous=wrap(sv_plot_corr_coef.noplot,size=3),
    #  mapping= aes(color = sv_plot_corr_coef.noplot)
    #),
    upper=list(continuous="blank"),
    lower = list(
      continuous=GGally::wrap(sv_plot_corr_scatter.noplot, size=0.3, alpha=0.8)#"points",ratio
    ),
    diag = list(
      continuous=GGally::wrap(sv_plot_histo.noplot)
    ),
    axisLabels = "none",
    switch = 'both'#move strip left and down
    #showStrips = FALSE


    #lower = list(discrete=wrap("ratio", size = 0.4, color = "red"))
  ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text.x= ggplot2::element_text(angle = 90, vjust = 1, size=5),
      strip.text.y= ggplot2::element_text(angle = 180, hjust = 1, size=5)
    )
  #theme()
  #+
  #ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue"))

    #+ rotateTextX()
  print(the.plot)


  return(the.plot)


}
