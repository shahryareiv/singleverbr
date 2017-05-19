#' prepares 4 datasets : train and test both in predictors and response
#'
#' @param data.df
#' input data
#' @param response.name
#' the name of the response column
#' @param the.train.ratio
#' the percent of train data to the whole data (the rest for test)
#' @param pred.vars.handling
#' how to handle predictor variables. It can be 'number' or 'dummify'
#' @param resp.vars.handling
#' how to handle response variables. It can be 'number' or 'dummify'
#' @return
#' Returns a list of 4 dataset for train and test both in predictors and response
#' @export
#'
#' @examples
uv_test_and_train_sets <- function(
  data.df,
  response.name,
  the.train.ratio = 0.75,
  pred.vars.handling = c('number','dummify'),
  resp.vars.handling = c('2num')
  ){

  # match the optional arguments
  pred.vars.handling <- match.arg(pred.vars.handling)
  resp.vars.handling <- match.arg(resp.vars.handling)

  # set half numbers to train and half to test
  set.seed(1)
  train.v <- sample(1:nrow(data.df), nrow(data.df) * the.train.ratio)
  test.v <- (-train.v)

  train.df <- data.df[train.v,]
  test.df <- data.df[test.v,]


  # create train and test predicator datasets as model matrix (automatically dummified)
  if (pred.vars.handling == 'number'){
    train.predicators.matrix <- data.matrix(data.df[train.v, !(names(data.df) %in% response.name)])
    test.predicators.matrix <- data.matrix(data.df[test.v, !(names(data.df) %in% response.name)])
  }
  else if (pred.vars.handling == 'dummify'){
    formula <- as.formula(paste(response.name, "~.", sep = ""))
    train.predicators.matrix <- model.matrix(
      formula,
      data = data.df[train.v,]
    )
    test.predicators.matrix <- model.matrix(
      formula,
      data = data.df[test.v,]
    )
  }
  train.response.c <- (data.df[train.v,  response.name])#[[1]]#!(names(data.df) %in% response.name)
  test.response.c <- (data.df[test.v, response.name])#[[1]]#!(names(data.df) %in% response.name)

  # after dummifiying should it be numeric or categorial?
  # if numeric
  train.response.c <- as.integer(train.response.c)
  test.response.c <- as.integer(test.response.c)

  return(list(
      all = data.df,
      train.predicators.matrix = train.predicators.matrix,
      test.predicators.matrix = test.predicators.matrix,
      train.response.c = train.response.c,
      test.response.c = test.response.c,
      train.df = train.df,
      test.df = test.df
    )
  )

}
