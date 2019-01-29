#' Binarize a character column where levels are separated by a given separator
#' 
#' Given a dataframe with a character column `col` containing multiple levels
#' separated by a separator `sep` return a new dataframe where this column has
#' been split into multiple logical ones
#' 
#' @param df the data frame to work on
#' @param col the column to binarize
#' @param sep the character sepearating the values
#' @param drop should the original column be dropped? (default = FALSE)
#' 
#' @return a dataframe with n logical columns corresponding to the levels
binarize_multi <- function(df, col, sep = ",", drop = FALSE) {
  `!!` <- rlang::`!!`
  `%>%` <- magrittr::`%>%`
  
  col <- dplyr::enquo(col)
  
  df <- 
    df %>% 
    dplyr::mutate(
      tmp_orig_tmp := !! col,
      !! col := stringr::str_split(!!col, sep)
    ) %>%
    tidyr::unnest(!!col) %>% 
    dplyr::mutate(true_tmp_true = T) %>% 
    tidyr::spread(!!col, true_tmp_true, fill = F, sep = "_") %>% 
    dplyr::rename(!! col := tmp_orig_tmp)
  
  if (drop) {
    df <- dplyr::select(df, - !! col)
  }
  
  df
}
