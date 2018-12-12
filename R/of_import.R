#' Imports form data from openforms.com
#'
#' This function returns form response data for a given form ID
#' @param formID The version ID of the form
#' @param apiKey The API key for the form owner's account
#' @param cache Allows data to be cached in a file to avoid hitting database repeatedly for the same data. False by default. If true, accepts a file path as an argument to cache imported data.
#' @export
#' @examples
#' of_import(1000, apiKey)

of_import <- function(formID, apiKey, cache = FALSE) {

  fullResponses<- jsonlite::fromJSON(paste("http://api.us.openforms.com/api/v2/responseList?apiKey=", apiKey, "&formVersionId=",formID,"&showDetail=true&limit=500", sep=""))

  # EXTRACT RESPONSE DATA INTO SEPARATE DATAFRAME
  responses<-jsonlite::flatten(as.data.frame(fullResponses$responses$answers))
  responses$ID <- fullResponses$responses$receiptNumberId

  #ADD SUBMIT DATE TIME TO RESPONSES DATA FRAME
  responses$Date<-fullResponses$responses$submitDatetime
  responses$Date<-sub("T"," ",responses$Date)
  responses$Date<-sub("-","/",responses$Date)
  responses$Date<-sub("-","/",responses$Date)
  responses$Date<-gsub("\\..*","",responses$Date)

  if (fullResponses$totalCount > 500) {

    # CALCULATE NUMBER OF ADDITIONAL QUERIES TO PULL
    if (is.integer(fullResponses$totalCount/500)) {
      numberQueries <- fullResponses$totalCount/500
    } else {
      numberQueries <- trunc(fullResponses$totalCount/500)
    }

    i <- 1
    for (i in (i:numberQueries)) {
      # Set Query Start and Get Data
      startIndex <- i * 500
      nextResponses <- jsonlite::fromJSON(paste("http://api.us.openforms.com/api/v2/responseList?apiKey=", apiKey, "&formVersionId=",formID,"&showDetail=true&startIndex=", startIndex, "&limit=500", sep=""))
      row.names(nextResponses$responses) <- as.character((as.numeric(row.names(nextResponses$responses)) + startIndex) - 1)

      # ADD RECEIPT NUMBERS AND DATE TO DATAFRAME
      nextResponses$responses$answers$ID <- nextResponses$responses$receiptNumberId

      nextResponses$responses$answers$Date<-nextResponses$responses$submitDatetime
      nextResponses$responses$answers$Date<-sub("T"," ",nextResponses$responses$answers$Date)
      nextResponses$responses$answers$Date<-sub("-","/",nextResponses$responses$answers$Date)
      nextResponses$responses$answers$Date<-sub("-","/",nextResponses$responses$answers$Date)
      nextResponses$responses$answers$Date<-gsub("\\..*","",nextResponses$responses$answers$Date)

      # Avoid Duplicate Row Names and Merge with Previous Query Data
      responses<-rbind(responses, jsonlite::flatten(as.data.frame(nextResponses$responses$answers)))

      i <- i + 1
    }
  }

  #RENAME COLUMNS WITH CONTROLID
  names(responses)<-sub(".value","",names(responses))

  #REMOVE DUPLICATED LIST VERSIONS OF RESPONSES TO MULTI-SELECT RESPONSES
  responses[grepl("selectedValue", names(responses))] <- NULL

  # COLLAPSE LIST RESPONSES INTO COMMA SEPARATED VALUES
  responses[which(sapply(responses, class) == "list")] <- vapply(responses[which(sapply(responses, class) == "list")], paste, collapse = ",", character(1L))

  #IMPORT FORM METADATA FOR IDENTIFYING QUESTIONS
  meta <- jsonlite::fromJSON(paste("http://api.us.openforms.com/api/v2/form?apiKey=", apiKey, "&formVersionId=", formID, sep=""))

  #GET QUESTION TITLES AND CONTROL ID'S FROM METADATA

  #CREATING DATAFRAME WITH ALL QUESTIONS' CONTROL ID'S
  ID = as.data.frame(meta$sections$controls[[1]]$controlId)
  ID$section<-meta$sections$name[1]
  ID$sectionID<-meta$sections$sectionId[1]
  names(ID) = c("controlId", "section", "sectionID")

  if (length(meta$sections$controls) > 1) {
    i<-2
    for (i in 2:(nrow(meta$sections))) {
      ID1<-meta$sections$controls[[i]]
      ID1<-as.data.frame(ID1)
      ID1<-as.data.frame(ID1$controlId)
      names(ID1)=c("controlId")
      ID1$section<-meta$sections$name[i]
      ID1$sectionID<-meta$sections$sectionId[i]
      ID<-rbind(ID,ID1)
      i = i+1
    }
  }

  #CREATING DATAFRAME WITH ALL QUESTIONS' LABELS
  lab = as.data.frame(meta$sections$controls[[1]]$label)
  names(lab) = "label"

  if (length(meta$sections$controls) > 1) {
    i<-2
    for (i in 2:(nrow(meta$sections))) {
      lab1<-meta$sections$controls[[i]]
      lab1<-as.data.frame(lab1)
      lab1<-as.data.frame(lab1$label)
      names(lab1)=c("label")
      lab<-rbind(lab,lab1)
      i = i+1
    }
  }

  #COMBINING DATAFRAMES WITH QUESTIONS' ID'S AND LABELS
  ID<-cbind(ID,lab)

  #MATCHING QUESTION LABELS WITH RESPONSE COLUMN NAMES
  namecolumns<-match(as.character(names(responses)), as.character(ID$controlId))[is.na(match(as.character(names(responses)), as.character(ID$controlId))) == FALSE]
  namecolumns<-ID$label[namecolumns]

  names(responses)=c(paste(namecolumns), "ID", "Date")

  # FIX COLUMNS WITH ONLY WHITESPACE IN NAMES IF ANY EXIST
  if (length(names(responses)[which(nchar(trimws(names(responses))) == 0)]) > 0) {
    names(responses)[which(nchar(trimws(names(responses))) == 0)] = c(paste("Unnamed Column", 1:length(which(nchar(trimws(names(responses))) == 0))))
  }

  # RETURN DATA
  print(responses)
}
