#' Authenticate to app.mybasis.com
#'
#' @param username	Your mybasis username
#' @param password  Your mybasis password
#' @export
#' @import XML
#' @import RCurl
basis.authenticate <- function(username,password) {
	options(RCurlOptions = list(
					capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
					verbose=TRUE,
					ssl.verifypeer = FALSE))

	cookie = 'cookiefile.txt'   
	c =  getCurlHandle ( cookiefile = cookie,
			cookiejar = cookie,
			useragent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
			followlocation=TRUE
	)

	ans = postForm("https://app.mybasis.com/login", 
			username = username, 
			password = password,
			curl=c, style='POST')
	return(c) # Return the curl handle
}


#' Request JSON data for a particular date range
#'
#' @param ch			The curleHandle returned by basis.authenticate()
#' @param state_date	Date from which to retrieve results (as YYYY-mm-dd)
#' @param end_date		Date to which to retrieve results (as YYYY-mm-dd)
#' @param interval		Interval between data points (in seconds, defaults to 1 minute)
#' @export
#' @import XML
#' @import RCurl
#' @return character
basis.json.data <- function(ch,start_date, end_date,interval=60) {
	url <- paste0("https://app.mybasis.com/api/v1/chart/me?",
			"summary=TRUE",
			"&interval=60",
			"&units=ms",
	            '&start_date=',start_date,
	            '&start_date=',end_date,
	            '&start_offset=0',
	            '&end_offset=0',
	            '&heartrate=true',
	            '&steps=true',
	            '&calories=true',
	            '&gsr=true',
	            '&skin_temp=true',
	            '&air_temp=true',
	            '&bodystates=true')

	res.json <- getURL(url,curl=c)
	return(res.json)
}


#' Convert JSON data to a data frame
#'
#' @param json			JSON data from a call to basis.json()
#' @export
#' @import rjson
#' @return data.frame
basis.data.frame <- function(json) {
	res <- fromJSON(json)
	report.start <- as.POSIXct(res$starttime,origin="1970-01-01")
	report.end   <- as.POSIXct(res$endtime,origin="1970-01-01")

	filter.nulls <- function(x) {ifelse(is.null(x),NA,x)}

	df <- data.frame(
		Timestamp = report.start + seq(0,res$endtime-res$starttime,60),
		Heart=sapply(res$metrics$heartrate$values, filter.nulls),
		Steps=sapply(res$metrics$steps$values, filter.nulls),
		Calories=sapply(res$metrics$calories$values, filter.nulls),
		GSR=sapply(res$metrics$gsr$values, filter.nulls),
		SkinTemp=sapply(res$metrics$skin_temp$values, filter.nulls),
		AirTemp=sapply(res$metrics$air_temp$values, filter.nulls)
	)
	return(df)
}
