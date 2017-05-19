#NOTE: data and mapping should not change name, becuase it is wrapped
sv_plot_histo.noplot <- function(data, mapping, low = "#132B43", high = "#56B1F7", ...) {

  cat_data <-data.frame(lapply(data,function(x) factor(x,ordered=TRUE)))
  the.plot <- ggplot2::ggplot(data = cat_data, mapping = mapping) + #mapping = the.mapping, aes(label="2")
    ggplot2::geom_bar(...)

  return(the.plot)
}
