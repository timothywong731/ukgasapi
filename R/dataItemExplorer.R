#' @title Data Item Explorer API
#' @description This function connects to the UK National Grid's API for Data Item Explorer, which is a major data source for gas-related information. Internet connection must be available.
#' @details The function submits a request to the API using XML over SOAP protocol. The response is in XML format which function will parse internally and returns a R dataframe object.
#' @param dataitems A vector of characters containing data items to enquire via API.
#' @param fromdate A character object specifying the start date. Date is inclusive.
#' @param todate A character object specifying the end date. Date is inclusive.
#' @param datetype A character object specifying the data type. Defaults to \code{gasday}
#' @param latestflag A character object with length of one to specify whether to extract the latest data. This can either be \code{Y} or \code{N}. Defaults to \code{Y}.
#' @param applicableforflag A character object with length of one to specify whether dates specified are 'applicable for' or 'applicable on'. This can either be \code{Y} or \code{N} where \code{Y} indicates 'applicable for'. Defaults to \code{Y}.
#' @param batchsize An interger value indicating the batch size of each API call. To invoke a single API call, use zero or negative values.
#' @param apiurl A character object which points to National Grid's SOAP API. Under most circumstances users do not have to change this. Defaults to 'http://marketinformation.natgrid.co.uk/MIPIws-public/public/publicwebservice.asmx'
#' @return A dataframe object containing API response data.
#' @examples
#' # Specify the data item(s) to enquire from API
#' dataitems <- c("Storage Injection, Actual",
#'                "Storage Withdrawal, Actual")
#'
#' # Invoke the API (requires internet connection at this step)
#' response <- dataItemExplorer(dataitems,
#'                              fromdate = "2015-09-01",
#'                              todate="2015-09-30")
#'
#' # Visualise the results on a chart
#' library(ggplot2)
#' ggplot(response,aes(x=ApplicableFor,y=Value,colour=PublicationObjectName)) +
#'  geom_line()
#' @author Timothy Wong, \email{timothy.wong@@hotmail.co.uk}
#' @references
#' \itemize{
#' \item Graphical User Interface for Data Item Explorer\cr
#' \url{https://mip-prod-web.azurewebsites.net/DataItemExplorer}
#' \item API specification\cr
#' \url{https://marketinformation.natgrid.co.uk/MIPIws-public/public/publicwebservice.asmx?op=GetPublicationDataWM}
#' }
#' @export
dataItemExplorer<- function(dataitems,
                            fromdate,
                            todate,
                            datetype="gasday",
                            latestflag="Y",
                            applicableforflag="Y",
                            batchsize = -1,
                            apiurl = paste0("https://marketinformation.natgrid.co.uk/",
                                            "MIPIws-public/public/publicwebservice.asmx")) {

  # Using batch mode
  if (batchsize > 0) {

    # Calculate batch IDs
    batch.quotient <- base::as.integer(base::as.Date(todate) - base::as.Date(fromdate) + 1) %/% batchsize
    batch.modulo <- base::as.integer(base::as.Date(todate) - base::as.Date(fromdate) + 1) %% batchsize

    # Create a row for each batch
    if (batch.quotient >0) {
      list.of.dates <- base::data.frame(
        start = 1:batch.quotient * batchsize - batchsize+1,
        end = 1:batch.quotient * batchsize)
    } else {
      list.of.dates <- base::data.frame(start = base::integer(),
                                        end = base::integer())
    }

    # Append a row if there's any remainder (modulo)
    if(batch.modulo > 0) {
      list.of.dates <- base::rbind(list.of.dates,
                                   base::data.frame(
                                     start = batch.quotient * batchsize+1,
                                     end = batch.quotient * batchsize + batch.modulo))
    }

    # Create a sequence of all dates
    all.dates <- base::seq.Date(base::as.Date(fromdate), base::as.Date(todate), 1)

    # Create an empty list of data frames
    results <- base::list()

    # Loop through all batch an run the API
    for (i in 1:base::nrow(list.of.dates)) {
      batch.fromdate <- all.dates[list.of.dates[i,]$start]
      batch.todate <- all.dates[list.of.dates[i,]$end]
      results[[i]] <- dataItemExplorer(
        dataitems = dataitems,
        fromdate = batch.fromdate,
        todate = batch.todate,
        datetype = datetype,
        latestflag = latestflag,
        applicableforflag = applicableforflag,
        batchsize = -1,
        apiurl = apiurl)
    }

    return(base::do.call(rbind, results))

  } else {
    # Non-batch mode

    # Creates SOAP XML request
    soap.request <- base::paste0('<?xml version="1.0" encoding="utf-8"?><soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope"><soap12:Body><GetPublicationDataWM xmlns="http://www.NationalGrid.com/MIPI/"><reqObject><LatestFlag>',latestflag,'</LatestFlag><ApplicableForFlag>',applicableforflag,'</ApplicableForFlag><ToDate>',todate,'</ToDate><FromDate>',fromdate,'</FromDate><DateType>',datetype,'</DateType><PublicationObjectNameList>',paste0('<string>',dataitems,'</string>', collapse = ''),'</PublicationObjectNameList></reqObject></GetPublicationDataWM></soap12:Body></soap12:Envelope>')

    # Initiates SOAP request via HTTP
    result <- httr::POST(apiurl,
                         body = soap.request,
                         httr::add_headers(.headers = c("Accept"="text/xml",
                                                        "Content-Type"="application/soap+xml; charset=utf-8")))

    # Throws an error if the HTTP status code is not 200
    base::stopifnot(result$status_code == 200)

    # Writes SOAP response into character
    soap.response <- httr::content(result, "text")

    # converts SOAP response into an XML object
    soap.response.doc <- XML::xmlTreeParse(soap.response, replaceEntities = TRUE , useInternalNodes = TRUE)

    # converts SOAP response into a list of objects
    objects <- (XML::xmlToList(soap.response.doc,simplify = TRUE))[[1]][[1]]

    list.of.data.frames <- base::list()

    for(i in 1:base::length(objects)) {
      # Porcessing data frames
      rows <- objects[[i]][["PublicationObjectData"]]

      list.of.rows <- base::data.frame()

      for(r in 1:length(rows)){
        # Processing individual row
        list.of.rows <- base::rbind(list.of.rows,as.data.frame(rows[r][[1]],stringsAsFactors = FALSE))
      }
      list.of.rows$PublicationObjectName <- objects[[i]][[1]]

      list.of.data.frames <- base::rbind(list.of.data.frames, as.data.frame(list.of.rows))
    }

    # Convert all columns to appropiate types before returning data frame
    result <- base::within(data = list.of.data.frames,
                           expr = {
                             ApplicableAt <- strptime(ApplicableAt,"%Y-%m-%dT%H:%M:%SZ","UTC")
                             ApplicableFor <- strptime(ApplicableFor,"%Y-%m-%dT%H:%M:%SZ","UTC")
                             Value <- as.numeric(Value)
                             GeneratedTimeStamp <- strptime(GeneratedTimeStamp,"%Y-%m-%dT%H:%M:%SZ","UTC")
                             QualityIndicator <- factor(QualityIndicator)
                             Substituted <- factor(QualityIndicator)
                             CreatedDate <- strptime(CreatedDate,"%Y-%m-%dT%H:%M:%SZ","UTC")
                             PublicationObjectName <- as.factor(PublicationObjectName)
                             })

    # Prefer using POSIXct rather than POSIXlt
    result$ApplicableAt <- as.POSIXct(result$ApplicableAt)
    result$ApplicableFor <- as.POSIXct(result$ApplicableFor)
    result$GeneratedTimeStamp <- as.POSIXct(result$GeneratedTimeStamp)
    result$CreatedDate <- as.POSIXct(result$CreatedDate)

    return (result)
  }
}
