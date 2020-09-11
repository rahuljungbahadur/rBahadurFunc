library(roxygen2)
library(devtools)
#' Function for converting a data frame into a beautifully formatted flextable.
#' @param data_frame input data frame.
#' @param header.name (Optional) name of the table header. Accepts a string.
#' @param tableType (Optional) A string of one of the following * modComp - dataframe with anova comparing models,
#'  *parEst - estimates of the parameters, ideally from the summary table,
#'   *fixAnova - dataframe with fixed effects anova.
#' @return  flextable object

#' @export
funcTableDesign <- function(data_frame, header.name = NULL, tableType = NULL){
  if(is.null(tableType)) {
    data_frame %>% flextable::flextable() %>% flextable::add_header_lines(header.name) %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(size = 12, part = "header") %>%
      flextable::fontsize(size = 9.5, part = "body") %>%
      flextable::autofit() %>% return()
  }else if(tableType == "modComp"){   ## tables for comparing models using anova
    data_frame %>%
      tibble::rownames_to_column(var = "Model Name") %>%
      dplyr::mutate(`Model Name`= stringr::str_extract(`Model Name`, pattern = "_.+$")) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-c(call)) %>%
      flextable::flextable() %>% flextable::add_header_lines("Model Comparison") %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(size = 12, part = "header") %>%
      flextable::fontsize(size = 9.5, part = "body") %>%
      flextable::autofit() %>% return()
  }else if(tableType == "parEst"){   ## tables for displaying parameter estimates
    data_frame %>%
      tibble::rownames_to_column(var = "Parameters") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      flextable::flextable() %>% flextable::add_header_lines("Parameter Estimates") %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(size = 12, part = "header") %>%
      flextable::fontsize(size = 9.5, part = "body") %>%
      flextable::autofit() %>% return()
  }else if(tableType == "fixAnova") {   ## tables for displaying fixed effects anova
    data_frame %>%
      tibble::rownames_to_column(var = "Parameters") %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      flextable::flextable() %>% flextable::add_header_lines("Fixed Effects Anova") %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(size = 12, part = "header") %>%
      flextable::fontsize(size = 9.5, part = "body") %>%
      flextable::autofit() %>% return()
  } else {
    print("tableType should be one of modComp, parEst, fixAnova")
  }
}


#' @title Function for generating table grob for custom annotation in ggplot2 graphs.
#' @description This function converts a data.frame into a tableGrob object with ttheme_minimal using the gridExtra package.
#' @param data_frame A data.frame object which needs to be added to a ggplot graph as a tableGrob
#' @return a tableGrob object which can be added to a ggplot2 object using annotation_custom
#' @example
#' \dontrun{
#' ggplot() + annotation_custom(grob = funcgridExtra::tableGrob(data.frame),xmin = 0.5, xmax= 1.5, ymin=1750, ymax=2500)
#' }
#'
#' @export
funcTableGrob <- function(data_frame){
  table_grob <- gridExtra::tableGrob(data_frame, rows = NULL, theme = gridExtra::ttheme_minimal())
  table_grob <- gtable::gtable_add_grob(table_grob,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                t = 2, b = nrow(table_grob), l = 1, r = ncol(table_grob))
  table_grob <- gtable::gtable_add_grob(table_grob,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                t = 1, l = 1, r = ncol(table_grob))
  return(table_grob)
}
