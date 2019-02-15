#' Imports form data from openforms.com
#'
#' This function returns form response data for a given form ID
#' @param formID The version ID of the form
#' @param apiKey The API key for the form owner's account
#' @export
#' @examples
#' of_import(1000, apiKey)

of_import <- function(formID, apiKey) {

  options(stringsAsFactors = FALSE)

  ########## API CALL TO GET FORM METADATA ##########
  apiMetadata <- httr::GET(paste("https://api.us.openforms.com/api/v4/forms/", formID,"?loadStructure=true", sep=""),
                           httr::add_headers("accept" = "application/json", "X-API-KEY" = apiKey, "content-Type" = "application/json"))

  apiMetadata <- httr::content(apiMetadata)

  ########## PARSE JSON TO GET COLUMN NAMES AND CONTROL ID'S ##########
  i <- 1
  for (i in i:length(apiMetadata$sections)) {
    j <- 1
    for (j in j: length(apiMetadata$sections[[i]]$fields)) {
      if (i == 1 && j == 1) {
        allFields <- as.data.frame(t(apiMetadata$sections[[i]]$fields[[j]]))
      } else {
        fields <- as.data.frame(t(apiMetadata$sections[[i]]$fields[[j]]))
        allFields <- rbind(allFields, fields)
      }
      j <- j + 1
    }
    i <- i + 1
  }

  ########## PARSE DATA FROM API RESPONSE JSON INTO DATAFRAME ##########
  x <- 1
  for (x in x:apiResponse$totalPages) {
    ########## API CALL TO GET FORM RESPONSE DATA ##########

    apiResponse <- httr::GET(paste("http://api.us.openforms.com/api/v4/responses?formId=", formID,"&loadAnswers=true&pageSize=1000&page=", x, sep=""),
                             httr::add_headers("accept" = "application/json", "X-API-KEY" = apiKey, "content-Type" = "application/json"))

    apiResponse <- httr::content(apiResponse)

    # PARSE RESPONSES FOR EACH API CALL
    i <- 1
    for (i in i:length(apiResponse$items)) {
      j<- 1
      # FORMAT EACH RESPONSE AND FORM FIELD CONTROL ID FROM INTO COLUMNS IN DATAFRAME
      for (j in j:length(apiResponse$items[[i]]$answers)) {
        if (j == 1) {
          questionsAnswers <- as.data.frame(t(apiResponse$items[[i]]$answers[[j]]))
          if (is.null(questionsAnswers$value)) {
            if (as.character(questionsAnswers$multiValues) == "list(list())" || is.null(questionsAnswers$multiValues)) {
              questionsAnswers$value <- NA
              questionsAnswers$multiValues <- NULL
            } else {
              questionsAnswers$value <- paste(unlist(questionsAnswers$multiValues), collapse = ",")
              questionsAnswers$multiValues <- NULL
            }
          }
          questionsAnswers <- as.data.frame(questionsAnswers$value)
          names(questionsAnswers)[j] <- apiResponse$items[[i]]$answers[[j]]$fieldId
        } else {
          answers <- as.data.frame(t(apiResponse$items[[i]]$answers[[j]]))
          # CHECK FOR OPTIONAL FIELDS WITH NO RESPONSES AND SET RESPONSE TO NA
          if (is.null(answers$value)) {
            if (as.character(answers$multiValues) == "list(list())" || is.null(answers$multiValues)) {
              answers$value <- NA
              answers$multiValues <- NULL
            } else {
              answers$value <- paste(unlist(answers$multiValues), collapse = ",")
              answers$multiValues <- NULL
            }
          }
          answers <- as.data.frame(answers$value)
          names(answers) <- apiResponse$items[[i]]$answers[[j]]$fieldId
          questionsAnswers <- cbind(questionsAnswers, answers)
        }
        j <- j + 1
      }
      if (i == 1) {
        questionsAnswers$Date <- apiResponse$items[[i]]$submitDateTime
        questionsAnswers$ID   <- apiResponse$items[[i]]$receiptNumber
        pageResponses <- questionsAnswers
      } else {
        questionsAnswers$Date <- apiResponse$items[[i]]$submitDateTime
        questionsAnswers$ID   <- apiResponse$items[[i]]$receiptNumber
        pageResponses <- rbind(pageResponses, questionsAnswers, make.row.names = TRUE, stringsAsFactors = FALSE)
      }
      i <- i + 1
    }
    if (x == 1) {
      allResponses <- pageResponses
    } else {
      allResponses <- rbind(pageResponses, allResponses, make.row.names = TRUE, stringsAsFactors = FALSE)
    }
    x <- x + 1
  }

  ########## SET RESPONSES DATAFRAME COLUMN NAMES TO FIELD NAMES IN OPENFORMS ##########
  names(allResponses)[!is.na(match(names(allResponses), allFields$id))] <- as.character(allFields$name)[!is.na(match(allFields$id, names(allResponses)))]

  # FIX COLUMNS WITH ONLY WHITESPACE IN NAMES IF ANY EXIST
  if (length(names(allResponses)[which(nchar(trimws(names(allResponses))) == 0)]) > 0) {
    names(allResponses)[which(nchar(trimws(names(allResponses))) == 0)] = c(paste("Unnamed Column", 1:length(which(nchar(trimws(names(allResponses))) == 0))))
  }
}
