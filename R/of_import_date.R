#' Imports form data from openforms.com
#'
#' This function returns form response data for a given form ID
#' @param formID The version ID of the form
#' @param apiKey The API key for the form owner's account
#' @param startDate The start date of the query in "%m/%d/%Y %H;%M:%S" format
#' @param cache Allows data to be cached in a file to avoid hitting database repeatedly for the same data. False by default. If true, accepts a file path as an argument to cache imported data.
#' @export
#' @examples
#' of_import(1000, apiKey)

of_import_date <- function(formID, apiKey, startDate, cache = FALSE) {
  # FORMAT START DATE
  startDate <- format(as.POSIXct(startDate), "%Y-%m-%d %H:%M:%S")
  startDate <- gsub(" ", "%20", startDate)
  startDate <- gsub(":", "%3A", startDate)

  # RUN QUERY
  fullResponses<- jsonlite::fromJSON(paste("http://api.us.openforms.com/api/v2/responseList?apiKey=", apiKey, "&formVersionId=",formID, "&showDetail=true&start=", startDate,"&limit=500", sep=""))

  #REMOVE UNNEEDED LEVELS OF DATA
  responses<-jsonlite::flatten(as.data.frame(fullResponses$responses$answers))
  responses<-responses[grep("value", names(responses))]

  #RENAME COLUMNS WITH CONTROLID
  names(responses)<-sub(".value","",names(responses))

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
  namecolumns<-match(as.character(names(responses)),as.character(ID$controlId))
  namecolumns<-ID$label[c(namecolumns)]

  names(responses)=namecolumns

  #ADD RESPONSE ID FOR WORKFLOW DASHBOARD
  responses$ID<-fullResponses$responses$receiptNumberId

  #ADD SUBMIT DATE TIME TO FINAL DATA FRAME
  responses$Date<-fullResponses$responses$submitDatetime
  responses$Date<-sub("T"," ",responses$Date)
  responses$Date<-sub("-","/",responses$Date)
  responses$Date<-sub("-","/",responses$Date)
  responses$Date<-gsub("\\..*","",responses$Date)
  as.POSIXct(responses$Date)

  # RETURN DATA
  print(responses)
}
