
#' Impute it!
#'
#' @param data.df
#'
#' @return
#' @export
#'
#' @examples
impute_it <- function(data.df){

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

