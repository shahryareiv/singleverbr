
#' Reports each variable, its possible values, the labels of those possible values
#'
#' @param the.data
#'
#' @return
#' @export
#'
#' @examples
sv_meta_variables.df <- function(the.data){

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


#' Returns structural reports on the dataset
#'
#' @param data.df
#'
#' @return
#' @export
#'
#' @examples
sv_meta_variables_detailed <- function(data.df, the.origin = c('spss','r')){

  the.origin <- match.arg(the.origin)
  if (the.origin != 'spss'){
    writeLines('*** Only SPSS format is supported at the moment.')
    return()
  }

  the.structure <- singleverbr::sv_meta_variables.df(data.df)


  the.variables.df <- the.structure %>%
    dplyr::select(the.variable) %>%
    dplyr::distinct()

  the.dim.variables.df <- the.structure %>%
    dplyr::filter(the.value.labels !='no_value_set' ) %>%
    dplyr::select(the.variable) %>%
    dplyr::distinct()

  the.date.variables.df <- the.structure %>%
    dplyr::filter(the.value.labels =='no_value_set' & the.spss.format =='SDATE10') %>%
    dplyr::select(the.variable) %>%
    dplyr::distinct()

  the.num.variables.df <- the.structure %>%
    dplyr::filter(the.value.labels =='no_value_set' & the.spss.format !='SDATE10') %>%
    dplyr::select(the.variable) %>%
    dplyr::distinct()


  all.cat.dimensions.ls <- lapply(as.list(the.dim.variables.df [[1]]), function(x){
    the.substructure <- the.structure %>%
      dplyr::filter(the.variable == x)
    n <- nrow(the.substructure)
    pid <- 1:n
    category.label <- the.substructure [['the.value.labels']]
    the.dimension <- data_frame(pid,category.label)
    attr(the.dimension,"name") <- the.substructure [[1,'the.variable']]
    attr(the.dimension,"description") <- the.substructure [[1,'the.label']]
    attr(the.dimension,"spss.format") <- the.substructure [[1,'the.spss.format']]
    the.dimension
  })

  return(
    list(
      the.structure = the.structure,
      the.variables.df = the.variables.df,
      the.dim.variables.df = the.dim.variables.df,
      the.date.variables.df = the.date.variables.df,
      the.num.variables.df = the.num.variables.df,
      all.cat.dimensions.ls = all.cat.dimensions.ls
    )
  )
}

