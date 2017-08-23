#' Do all LASSO things
#'
#' @param data.df
#' @param response.name
#' @param pred.vars.handling
#' @param resp.vars.handling
#' @param best.lambda
#' @return
#' @export
#'
#' @examples
sv_lasso <- function(
  data.df,
  response.name,
  the.response.mode = c('gaussian', 'mgaussian', 'ordinal', 'bionomial', 'multinomial'),
  # pred.vars.handling = c('2num'),
  # resp.vars.handling = c('2num'),
  best.lambda = NULL,
  alpha = 1,
  nlambda = 100, #default value in glmnet
  type.measure = 'deviance', #default value in cv.glmnet
  file.prefix = "",
  ...
  ){


  the.response.mode <- match.arg(the.response.mode)


  # extract 4 datasets for train/test and predicator/response
  data.groups <- sv_test_and_train_sets( data.df, response.name )

  #!!!!!!!!!!!!!!!!!!!!
  # g <<- NULL
  # g$data.groups <<- data.groups


  # tune things according to the requested
  if (the.response.mode == 'gaussian'){

    the.response.family <- 'gaussian'


  }else if (the.response.mode == 'mgaussian'){

    the.response.family <- 'mgaussian'


  }else if (the.response.mode == 'ordinal'){

    the.response.family <- 'ordinal'


  }else if (the.response.mode == 'bionomial'){

    the.response.family <- 'bionomial'


  }else if (the.response.mode == 'multinomial'){

    the.response.family <- 'multinomial'


  }else{

    stop("You did not specify a valid response mode.")


  }



  # what are the LASSO
  require(glmnet)


  fit.glmnet <- glmnet::glmnet(
    x = data.groups$train.predicators.matrix, #as.matrix(patients.design.df)
    y = data.groups$train.response.c, #patients.response.1.df[train.v], #as.factor()
    alpha = alpha, #for LASSO then alpha=1
    standardize = TRUE,
    nlambda = nlambda,
    # lambda = grid
    family = the.response.family
  )

  # what is the cross-validation of LASSO
  cvfit.glmnet <- glmnet::cv.glmnet(
    x = data.groups$train.predicators.matrix, #as.matrix(patients.design.df),
    y = data.groups$train.response.c,
    alpha = alpha, #LASS then alpha=1
    nfolds = 5,
    family = the.response.family,
    type.measure = type.measure
  )



  # if best.lambda is not defined then ask the cross-validation what is the best (minimum)
  if (is.null(best.lambda)){
    best.lambda <- cvfit.glmnet$lambda.min
  }

  #try the test data with the best lambda on the model
  lasso.pred <- predict(
    fit.glmnet,
    s = best.lambda,
    newx = data.groups$test.predicators.matrix#as.matrix(patients.design.df[test.v,])#patient.test.1.matrix
  )



  #what is the MSE
  if (NROW(dim(lasso.pred)) == 3){
    mse <- ((lasso.pred[,,1] - data.groups$test.response.c)^2)
  }else{
    mse <- ((lasso.pred - data.groups$test.response.c)^2)
  }


  #then what coefs?
  lasso.coef <-predict(
    fit.glmnet,
    type = "coefficients",
    s = best.lambda
  )
  lasso.coef.mt <- as.matrix(lasso.coef)

  # #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # g$fit.glmnet <<- fit.glmnet
  # g$cvfit.glmnet <<- cvfit.glmnet
  # g$lasso.pred <<- lasso.pred
  # g$mse <<- mse
  # g$lasso.coef <<- lasso.coef.mt

  #draw
  #creat legeneds
  singleverbr::begin_plot(paste(file.prefix,"lasso_fit_",response.name,sep=""))
  if (the.response.mode == 'mgaussian'){
    # lasso_plot_legends <- function(fit, ...) {
    #         L <- length(fit$lambda)
    #         # x <- log(fit$lambda[L])
    #         # y <- fit$beta[, L]
    #         # labs <- names(y)
    #         # text(x, y, labels=labs, ...)
    #         # legend('topright', legend=labs, col=1:6, lty=1) # only 6 colors
    # }
    plot(fit.glmnet, label = TRUE, type.coef = "2norm")
    # lasso_plot_legends(fit.glmnet)

  }else{
    lasso_plot_legends <- function(fit, ...) {
            L <- length(fit$lambda)
            x <- log(fit$lambda[L])
            y <- fit$beta[, L]
            labs <- names(y)
            text(x, y, labels=labs, ...)
            legend('topright', legend=labs, col=1:6, lty=1) # only 6 colors
    }
    plot(fit.glmnet, label = TRUE)
    lasso_plot_legends(fit.glmnet)

  }
  singleverbr::end_plot()
  singleverbr::begin_plot(paste(file.prefix,"lasso_cvfit_",response.name,sep=""))
  plot.cv.glmnet(cvfit.glmnet, label = TRUE)
  singleverbr::end_plot()


  if (the.response.mode == 'mgaussian'){
    return(list(
        lasso.coef = lasso.coef,
        # lasso.coef.nonzero = (data.frame(
        #   coef.name = rownames(lasso.coef) [which(lasso.coef != 0)],
        #   coef = lasso.coef[lasso.coef!=0])), #[order(coef),]
        mse=mse,
        lasso.pred = lasso.pred,
        fit.glmnet = fit.glmnet,
        cvfit.glmnet = cvfit.glmnet,
        best.lambda = best.lambda,
        data.groups = data.groups
      )
    )

  }else{
    return(
      list(
        lasso.coef = lasso.coef.mt,
        lasso.coef.nonzero =
          data.frame(
            coef.name = rownames(lasso.coef.mt)[which(lasso.coef.mt != 0)],
            coef = lasso.coef[lasso.coef.mt != 0]
          ), #[order(coef),]
        mse=mse,
        lasso.pred = lasso.pred,
        fit.glmnet = fit.glmnet,
        cvfit.glmnet = cvfit.glmnet,
        best.lambda = best.lambda,
        data.groups = data.groups
      )
    )

  }
}
