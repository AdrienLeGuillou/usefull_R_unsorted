############################################################################
# a R function that create n binary columns from a categorical one where 
# there are multiple values separated by a character `p`
#
# for example from the column 'color' with modalities red, green and blue
# a cell containing "red+blue" will be binarized in red=1, blue=1 and green=0 
#
# with `df` our data frame,
# `p` the variable containing the separator (here "\+" because regex)
# `col` the name of the column to be split
#
# # first we transform the column from pure string to vector of strings (in each cell)
# df[[col]] <- str_split(zbis[[col]], p)
#
# # then we binarise
# df <- binarised_multi(df, col)
############################################################################

binarised_multi <- function(df, col) {
  # to transform a categorical column with multiple values (in vectors) into
  # a set of binary ones (not dummy)
  
  lvls <- df[[col]] %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
  for (l in lvls) {
    name <- str_c(col, "_", l)
    df[[name]] <- str_detect(df[[col]], l)
  }
  
  df
}
