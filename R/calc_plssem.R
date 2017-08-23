#' Do PLS-SEM
#'
#' @param data.df
#' @param struct.mat
#' @param measure.mat
#' @param the.lv
#' @param the.filename
#' @param the.model
#' If model is presented then there would be a MatrixPLS result
#' @param show.rsquared
#'
#' @return
#' @export
#'
#' @examples
sv_plssem <- function(
  data.df,
  struct.mat,
  measure.mat,
  the.model,
  the.lv,
  the.filename = 'plssem_result',
  show.rsquared = TRUE
){


  the.result <- list()

  # library(pls)
  # pls.fit <- plsr(
  #   the.formula,
  #   data = data.df,
  #   validation = 'CV'
  # )
  # pls.predict <- predict(
  #   pls.fit
  # )
  # validationplot(pls.fit, val.type = 'RMSEP')
  # the.result <- list(
  #   pls.fit = pls.fit,
  #   pls.predict = pls.predict
  # )


  # library(plspm)
  # plspm.fit <- plsr(
  #   the.formula,
  #   data = data.df,
  #   validation = 'CV'
  # )
  # plspm.predict <- predict(
  #   plspm.fit
  # )
  # plot(plspm.fit)
  # the.result <- list(plspm.fit = plspm.fit)

  require(matrixpls)
  if(!missing(the.model)){
    matrixpls.res <- matrixpls::matrixpls(cov(as.integer.dataframe(data.df)),the.model)
  }else{
    matrixpls.res <- NULL
  }

  library(semPLS)

  the.pls <- plsm(
    data = data.df,
    strucmod = struct.mat,
    measuremod = measure.mat
  )
  # mvpairs(
  #   model = the.pls,
  #   data = data.df,
  #   LVs = the.lv
  # )
  the.pls.res <- sempls(
    model = the.pls,
    data = data.df,
    wscheme = "pathWeighting", #centroid, factorial
    method = 'spearman',
    maxit = 100
  )

  rSquared <- NULL
  if (show.rsquared){
    rSquared <- semPLS::rSquared(the.pls.res)
  }

  pathDiagram(
    the.pls.res,
    file = the.filename,
    full = TRUE,
    edge.labels = "values",
    output.type = "graphics",
    digits = 2,
    graphics.fmt = "pdf",
    rSquared = rSquared
  )

  the.pls.res$matrixpls.res <- matrixpls.res
  the.result <- the.pls.res

  return(the.result)

}

