#' @title BMRS Initial Demand Outturn API Service
#' @description This function connects to the Elexon's BMRS API to retrieve Initial Demand Outturn (INDO) and Initial Transmission System Demand Out-Turn (ITSDO) data. Internet connection must be available.
#' @details The function submits a request to the API. The response is in CSV format which function will parse internally and returns a R dataframe object. The data returned by this API is identical to the data displayed on BMRS dashboard \url{https://test2.bmreports.com/bmrs/?q=demand/initialdemandoutturn}.
#' @param fromdate A character object specifying the start date. Date is inclusive.
#' @param todate A character object specifying the end date. Date is inclusive.
#' @param apikey A character object specifying the API key. This is also known as scripting key on Elexon's website.
#' @param batchsize An interger value indicating the batch size of each API call. (Number of days included in one call)
#' @return A dataframe object containing API response data.
#' @examples
#' \dontrun{
#' # Invoke the API (requires internet connection at this step)
#' response <- bmrsIndoItsdo(fromdate = "2020-01-01",
#'                           todate =  "2020-01-10",
#'                           apikey = "your api key goes here")
#'
#' # Visualise the results on a chart
#' library(ggplot2)
#' ggplot(response, aes(x=publishtime, y=value,colour=recordtype)) +
#'   geom_line()
#' }
#' @author Timothy Wong, \email{timothy.wong@@hotmail.co.uk}
#' @references
#' \itemize{
#' \item BMRS API and Data Push User Guide\cr
#' \url{https://www.elexon.co.uk/documents/training-guidance/bsc-guidance-notes/bmrs-api-and-data-push-user-guide-2/}
#' \item Scripting Guide\cr
#' \url{https://www.elexonportal.co.uk/scripting?cachebust=5fo1t1pld7}
#' \item Initial Demand Outturn (INDO)\cr
#' \url{https://www.bmreports.com/bmrs/?q=help/glossary#INDO}
#' \item Initial Transmission System Demand Out-Turn (ITSDO)\cr
#' \url{https://www.bmreports.com/bmrs/?q=help/glossary#ITSDO}
#' }
#' @export
bmrsIndoItsdo <- function(fromdate,
                          todate,
                          apikey,
                          batchsize = 60,
                          apiurl = "https://api.bmreports.com/BMRS/INDOITSDO/v1") {

  startdates <- base::seq.Date(base::as.Date(fromdate),
                               base::as.Date(todate),
                               by = batchsize)

  my_result <- base::data.frame()

  for (i in base::seq_along(startdates)) {

    startdate <- startdates[i]

    # Call the API

    my_url <- base::sprintf("%s?APIKey=%s&FromDate=%s&ToDate=%s&ServiceType=csv",
                            apiurl, apikey, startdate, base::min(as.Date(todate), startdate+batchsize-1))

    results <- httr::GET(my_url)
    response <- httr::content(results,"text")

    my_data <- utils::read.csv(text = response,
                               skip = 1,
                               header = FALSE,
                               colClasses = c("character",
                                              "character",
                                              "integer",
                                              "character",
                                              "character",
                                              "numeric"),
                               col.names = c("recordtype",
                                             "settdate",
                                             "settperiod",
                                             "syszone",
                                             "publishtime",
                                             "value"))

    # Parse columns
    my_data$settdate <- base::as.Date(my_data$settdate, format="%Y%m%d")
    my_data$publishtime <- base::as.POSIXct(my_data$publishtime, tz = "Europe/London", format="%Y%m%d%H%M%S")

    # Bind dataframe
    my_result <- base::rbind(my_result, my_data[-base::nrow(my_data), ])

  }

  return(my_result)
}
