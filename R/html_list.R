#' Format input items into a html bulleted list
#'
#' This function formats items into a html bulleted list
#' @param items List or dataframe of items to be formatted
#' @export
#' @examples
#' of_import(1000, apiKey)

html_list <- function (items) {

  # DETERMINE DATA TYPE OF ITEMS
  if (is.data.frame(items)) {
    itemsCount <- nrow(items)
  } else {
    itemsCount <- length(items)
  }

  # FORMAT ITEMS INTO HTML BULLETED LIST
  if (itemsCount > 0) {
    list <- paste('<ul><li>', items[1], '</li>', sep="")
  } else {
    list <- paste('', sep="")
  }

  if (itemsCount > 1) {
    i <- 2

    for (i in i:itemsCount) {
      list <- paste(list, '<li>', items[i], '</li>', sep="")
      i <- i + 1
    }
  }

  if (itemsCount > 0) {
    list <- paste(list, '</ul>', sep="")
  }

  return(list)
}
