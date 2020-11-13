# Function to get percipitation data from the API
getYearlyData <- function(stationid, year) {
  # Basic API request
  baseUrl <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/data"
  startDate <- paste(year, "01", "01", sep="-")
  endDate <- paste(year, "12", "31", sep="-")
  request <- GET(baseUrl, query=list(stationid=stationid,
                                     datasetid="GHCND",
                                     startdate=startDate,
                                     enddate=endDate,
                                     datatypeid="PRCP",
                                     limit="366"),
                 add_headers(token=authToken))
  json <- content(request, as="text")
  # Check to see if we've been rate-limited
  if("status" %in% names(json)) {
    Sys.sleep(1)
    request <- GET(baseUrl, query=list(stationid=stationid,
                                       datasetid="GHCND",
                                       startdate=startDate,
                                       enddate=endDate,
                                       datatypeid="PRCP",
                                       limit="366"),
                   add_headers(token=authToken))
    json <- content(request)
  }
  if(validate(json)) {
    json <- fromJSON(json)
    if(length(json) != 0) {
      results <- json$results %>% select(-datatype) %>% separate(date, into=c("date","time"), sep="T") %>% select(-time) %>%
        separate(attributes, into=c("measurement","quality","source","time"), sep=",") %>% select(-source, -time)
    } else {
      # Dummy dataframe in case there is an error
      results <- data.frame(date = character(), station = character(), measurement = character(), quality = character(), value = numeric())
    }
    return(results)
  }
}

# Make dummy dataframe to begin storing data
allData <- data.frame(date = character(), station = character(), measurement = character(), quality = character(), value = numeric())

# For each station in the dataset, fetch data for all complete years 1895-2015
getAllData <- function(stations) {
  for(station in 1:nrow(stations)) {
    # Determine starting and ending years of complete data
    start <- max(as.numeric(stations[station, "startYear"]), 1895)
    end <- min(as.numeric(stations[station, "endYear"]), 2015)
    startDate <- paste(as.character(start), "01", "01", sep="-")
    endDate <- paste(as.character(end), "12", "31", sep="-")
    if(stations[station, "mindate"] >= as.Date(startDate, "%Y-%m-%d"))
      start <- start + 1
    if(stations[station, "maxdate"] <= as.Date(endDate, "%Y-%m-%d"))
      end <- end - 1
    # Make API calls and append to dataset
    for(year in start:end){
      print(paste(stations[station,"id"],year))
      data <- memoizedCall(getYearlyData, stations[station, "id"], year)
      allData <<- allData %>% bind_rows(data)
    }
  }
}