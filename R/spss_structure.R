
#' Reports each variable, its possible values, the labels of those possible values
#'
#' @param the.data
#'
#' @return
#' @export
#'
#' @examples
sv_meta_spss_variables.df <- function(the.data){

  the.result <- the.data %>%
    lapply(function(x){attributes(x)}) %>% #make a list out of the labels
    sv_list_to_df() %>% #convert the list to df
    dplyr::rowwise() %>% #look at the rows
    dplyr::do( #for each row
      {
        dplyr::data_frame(
          the.variable=.$name, #what is the variable name?
          the.spss.format=ifelse( #what is the spss format?
            is.null(.$list.element$format.spss),
            list('no_spss_format'),
            list(.$list.element$format.spss)
          ),
          # the.class=ifelse(
          #   is.null(.$list.element$class),
          #   list('no_class'),
          #   list(.$list.element$class)
          # ),
          the.label=ifelse(  #what is the label?
            is.null(.$list.element$label),
            list('no_lable'),
            list(.$list.element$label)
          ),
          the.value.labels=ifelse( #what are the possible values labels?
            is.null(.$list.element$labels),
            list('no_value_set'),
            list(attr(.$list.element$labels,'names'))
          ),
          the.value.values=ifelse( #what are the labels for the possible values
            is.null(.$list.element$labels),
            list(0),
            list(.$list.element$labels)
          ) #what are the possible values
        )
      }
    ) %>%
    dplyr::ungroup() %>% #again, look at columns instead of rows
    tidyr::unnest(the.spss.format,the.label) %>% #expand the lists of value-lable to individual rows
    tidyr::unnest(the.value.labels,the.value.values) %>% #expand the lists of value-lable to individual rows
    dplyr::arrange(the.value.labels)

  the.result
}

