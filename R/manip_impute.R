
#' Impute it!
#'
#' @param data.df
#' The data.frame to be imputed
#' @return
#' @export
#'
#' @examples
sv_impute <- function(data.df, the.method = c('remove')){


  the.method <- match.arg(the.method)

  #Miss Forest
  # library(missForest)
 # patients.q.df.imp <- missForest::missForest(patients.q.df)
 # library(Amelia)
 # patients.q.df.imp <- Amelia::amelia(patients.q.df)

  #Mice
  # require(mice)
  # # mice::md.pattern(patients.q.df)
  # data.df.imp <- mice::mice(data.df, printFlag = FALSE)
  # return(mice::complete(data.df.imp,1))

  # #Another Level
  # data.df[is.na(data.df)] <- as.integer(0)
  # return(data.df)

  # Remove them!
  return(data.df[complete.cases(data.df),])

}


#' Title
#'
#' @param data.df
#'
#' @return
#' @export
#'
#' @examples
sv_impute_na_report <- function(data.df){
  res <- lapply(
    data.df,
    function (x){
      nrow(x[is.na(x)])
    }
  )
}


