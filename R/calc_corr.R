
#' Test a correlation matrix for significance
#'
#' @return
#' @export
#'
#' @examples
sv_corr_sig.mat <- function(...){

  the_func <-func_wrapper(calc_corr_sig.mat)
  return(the_func(...))

}
calc_corr_sig.mat <- function(corr.data, conf.level = 0.95,method="kendall"){#="kendall"
  # method="pearson"
  mat <- as.matrix(corr.data)
  n <- ncol(mat)
  pvalue.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(pvalue.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
      for(j in (i+1):n){

          if (method=='pearson'){ # Pearson

            corr_val <- cor.test(mat[,i], mat[,j], conf.level = conf.level, method=method)#
            lowCI.mat[i,j] <- lowCI.mat[j,i] <- corr_val$conf.int[1]
            uppCI.mat[i,j] <- uppCI.mat[j,i] <- corr_val$conf.int[2]

          }else if (method=='kendall'){ #Kendall

            corr_val <- cor.test(mat[,i], mat[,j],method=method, conf.level = conf.level,continuity =TRUE)#, method=method

          } else if (method=='spearman'){ #Spearman

            corr_val <- cor.test(mat[,i], mat[,j],method=method, conf.level = conf.level,continuity =TRUE)#, method=method

          }else{ #something wrong

            stop("You should specify the method for corr_test.mat function")

          }

          #anyway
          pvalue.mat[i,j] <- pvalue.mat[j,i] <- corr_val$p.value
      }
  }
  return(list(pvalue.mat, lowCI.mat, uppCI.mat))
}



sv_corr_test.pair <- function(...){

  the_func <-func_wrapper(corr_test.pair)
  return(the_func(...))

}
corr_test.pair <- function(x, y, conf.level = 0.95,method){#="kendall"# method="pearson"

  pvalue <- NA
  if (method=='pearson'){ # Pearson

    corr_val <- cor.test(x, y, conf.level = conf.level, method=method)#
    lowCI.mat[i,j] <- lowCI.mat[j,i] <- corr_val$conf.int[1]
    uppCI.mat[i,j] <- uppCI.mat[j,i] <- corr_val$conf.int[2]

  }else if (method=='kendall'){ #Kendall

    corr_val <- cor.test(x, y,method=method, conf.level = conf.level,continuity =TRUE)#, method=method

  } else if (method=='spearman'){ #Spearman

    corr_val <- cor.test(x, y,method=method, conf.level = conf.level,continuity =TRUE)#, method=method

  }else{ #something wrong

    stop("You should specify the method for corr_test.mat function")

  }

  #anyway
  pvalue <- corr_val$p.value
  return(pvalue)
}
