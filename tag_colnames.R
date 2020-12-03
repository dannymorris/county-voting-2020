
tag_colnames <- function(x, tag) {
  x %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^[:alnum:]]", "_") %>%
    paste(tag, ., sep = "_")
}
