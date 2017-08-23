#' average of the factors
#'
#' @param the.levels
#' The number of levels in the output.
#' It cannot be less than the maximum number in the input factos.
#' A value of -1 means it should be adjusted to the maximum number in the input.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sv_factor_mix <- function(the.levels, ...){

  # the factors as numbers
  the.nums <- sapply(list(...), function(v){as.numeric(as.character(v))})

  # check the maximum level things
  if (the.levels == -1){

    the.levels <- max(the.nums)

  }else if (max(the.nums) > the.levels){

    stop('You have a number in the inputs that is larger than the.levels')

  }

  # calculate
  the.nums.sum <- rowSums(the.nums)
  average.input <- as.integer(the.nums.sum / NCOL(the.nums))
  the.score <- factor(average.input, levels = 1:the.levels, ordered = TRUE)

  # for debuging
  # g <<- NULL
  # g$the.nums <<- the.nums
  # g$the.nums.sum <<-the.nums.sum
  # g$average.input <<- average.input
  # g$the.score <<- the.score

  return(the.score)


}
