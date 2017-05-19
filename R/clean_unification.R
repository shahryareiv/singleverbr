
#' Title
#'
#' @param the.data
#'
#' @return
#' @export
#'
#' @examples
uv_meta_guess_messy <- function(the.data){

  the.meta <- uv_meta_spss_variables.df(the.data)

  the.data
}


#' Title
#'
#' @param the.data.1
#' @param the.data.2
#'
#' @return
#' @export
#'
#' @examples
uv_meta_compare <- function(the.data.1,the.data.2){

  the.meta.1 <- uv_meta_spss_variables.df(the.data.1)
  the.meta.2 <- uv_meta_spss_variables.df(the.data.2)

  the.compare <- dplyr::anti_join(the.meta.1,the.meta.2,by='the.variable')

  the.compare

}


#' Title
#'
#' @param the.data
#' @param the.type
#' @param the.mains.and.alternatives.ls
#'
#' @return
#' @export
#'
#' @examples
uv_meta_harmonize <- function(the.data,the.type=c('label','value.labels','value.values'), the.mains.and.alternatives.ls){

  the.meta <- uv_meta_spss_variables.df(the.data)

  lapply(the.mains.and.alternatives.ls, function(the.pair){

    the.main <- the.pair[[1]]
    the.alternatives <- the.pair[-1]

    lapply(the.alternatives, function(the.alternative){

      switch(
        the.type,
        label = {1

        },
        value.labels = {2

        },
        value.values = {3

          # the.data[] <-

        }
      )

    })



  }
  )

  the.data

}


#' Title
#'
#' @param the.data
#' @param the.variable
#' @param the.value.label.src
#' @param the.value.label.des
#'
#' @return
#' @export
#'
#' @examples
uv_meta_change_value_label <- function(the.data, the.variable, the.value.label.src, the.value.label.des){


  # the.data[,attr(the.data$the.variable, which='label')='']
  attr(the.data[the.variable][[1]], which='labels')#
# the.data[the.variable]



}
