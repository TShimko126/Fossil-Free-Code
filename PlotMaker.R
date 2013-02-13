##Check/install necessary packages
if("tm" %in% rownames(installed.packages()) == FALSE){
  install.packages("tm")
}
if("stringr" %in% rownames(installed.packages()) == FALSE){
  install.packages("stringr")
}
if("ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}
if("maps" %in% rownames(installed.packages()) == FALSE){
  install.packages("maps")
}
if("zipcode" %in% rownames(installed.packages()) == FALSE){
  install.packages("zipcode")
}
if("mapproj" %in% rownames(installed.packages()) == FALSE){
  install.packages("mapproj")
}
if("ggmap" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggmap")
}

##Load packages
library(tm)
library(stringr)
library(ggplot2)
library(maps)
library(zipcode)
library(mapproj)
library(ggmap)

##Select and read file
file = file.choose()
data = readLines(file)

##Function to extract zipcodes
getZips = function(){
  x = grep("\\([0-9]+)", x = data)
  y = str_extract_all(string = data, "\\([0-9]+)")
  y = unlist(y)
  y = y[(length(y) != 0)]
  y = gsub(pattern = "\\(", replace = "", x = y)
  y = gsub(patter = ")", replace = "", x = y)
  y
}

##Function to extract signatures per date
getDates = function(){
  x = grep("[A-Z][a-z][a-z]\\. [0-9][0-9]?, 201[2-9]", x = data)
  y = str_extract_all(string = data, "[A-Z][a-z][a-z]\\. [0-9][0-9]?, 201[2-9]")
  y = unlist(y)
  y = y[(length(y) != 0)]
  y = gsub("\\.","",x = y)
  y = gsub(",","",x = y)
  y = as.Date(y, format = "%b %d %Y")
  y
}

##Function to format the dates and signature counts into a data frame
formatSigs = function(){
  sigTime = getDates()
  sigTimeData = as.data.frame(table(sigTime))
  names(sigTimeData) = c("Day", "Frequency")
  sigTimeData$Day = as.character(sigTimeData$Day)
  x = as.numeric(Sys.Date()-sigTime[1])
  sigTime1 = seq(sigTime[1], Sys.Date(), 1)
  sigTot = as.data.frame(cbind(as.character(sigTime1), 0))
  names(sigTot) = c("Day", "Frequency")
  sigs = merge(sigTot, sigTimeData, by = "Day", all.x = TRUE)
  sigs$Frequency.x = NULL
  names(sigs) = c("Day", "Frequency")
  sigs$Frequency[is.na(sigs$Frequency)] = 0
  sigs$Day = as.Date(sigs$Day)
  sigs
}

##Prompts user for plot type and returns plot of user's choosing (either national or signature)
plotType = function() {
  type = readline(prompt = "What plot would you like? (national/signature): ")
  if (type == "national") {
    data(zipcode)
    zips = getZips()
    count = as.data.frame(table(zips))
    zipData = zipcode[(zipcode$zip %in% zips),]
    zipData = cbind(zipData, count$Freq)
    names(zipData) = c("zip","city","state","latitude","longitude","freq")
    map_dat = map_data("state")
    plot_national = ggplot(data = map_dat, aes(x = long, y = lat)) + geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "gray", colour = "black", lwd = .5) + geom_point(data = zipData, aes(x = longitude, y = latitude, size = freq, colour = state), alpha = .5) + scale_size_continuous(range = c(4, 10)) + theme_bw()
    plot_national
  }
  else if (type == "signature"){
    sigs = formatSigs()
    cDate = as.character(Sys.time())
    plot_sigs = ggplot(data = sigs, aes(x = Day, y = cumsum(Frequency))) + geom_line(colour = "orange", lwd = 2) + geom_point(color = "black") + labs(x = "Date", y = "Total Signatures") + ggtitle(paste0("Petition Signatures Over Time\nCurent as of: ",cDate))
    plot_sigs
  } 
  else {
    stop("That is not a valid plot type.")
  }
}