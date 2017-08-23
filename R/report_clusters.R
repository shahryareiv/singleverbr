#' Reports on the cluster
#'
#' @param data.df
#' The input data
#' @param the.name
#' The name of the report (file)
#'
#' @return
#' @export
#'
#' @examples
sv_report_cluster <- function(data.df, the.name, to.impute = FALSE){


  if (to.impute) {
    data.df <- sv_impute(data.df)
  }

  require(Hmisc)
  data.varclus.spearman <- Hmisc::varclus(data.matrix(data.df))
  begin_plot(paste(the.name,"_varclus_spearman",sep=""))
  plot(data.varclus.spearman)
  end_plot()

  require(amap)
  data.varclus.kendall <- amap::hcluster(t(data.matrix(data.df)), method = 'kendall')
  begin_plot(paste(the.name,"_varclus_kendall",sep=""))
  plot(data.varclus.kendall)
  end_plot()


  require(cluster)
  require(factoextra)
  # data.clustergap <- factoextra::fviz_nbclust(data.matrix(data.df), FUN = kmeans, method = 'gap_stat')
  begin_plot(paste(the.name,"_varclusgap",sep=""))
  old.par <- par(mfrow = c(3,1))

  data.cluster.gap <- cluster::clusGap(data.matrix(data.df), FUN = kmeans, K.max = ncol(data.df)-1)#
  # factoextra::fviz_gap_stat(data.clustergap)
  print(data.cluster.gap, method = "firstmax")
  data.cluster.gap.sel <- factoextra::fviz_gap_stat(data.cluster.gap)
  # +
  # geom_vline(xintercept = 4, linetype = 2)
  # plot(data.cluster.gap, main = paste(the.name,'Gap(k)'))
  plot(data.cluster.gap.sel, main = paste(the.name,'Gap(k)'))

  data.cluster.wss <- factoextra::fviz_nbclust(data.matrix(data.df), FUN = kmeans, method = 'wss')
  print(summary(data.cluster.wss))
  plot(data.cluster.wss, main = paste(the.name,'WSS'))

  data.cluster.silhouette <- factoextra::fviz_nbclust(data.matrix(data.df), FUN = kmeans, method = 'silhouette')
  print(summary(data.cluster.silhouette))
  plot(data.cluster.silhouette, main = paste(the.name,'silhouette'))

  # factoextra::fviz_nbclust(data.matrix(data.df), FUN = kmeans, method = 'silhouette') +
# geom_vline(xintercept = 3, linetype = 2)
  par(old.par)
  end_plot()

  return(list(
    data.cluster.gap = data.cluster.gap,
    data.cluster.wss = data.cluster.wss,
    data.cluster.wss = data.cluster.silhouette
  ))

}
