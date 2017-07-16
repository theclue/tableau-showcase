require(plyr)
require(dplyr)
require(openxlsx)

papers$class <- "paper"
earthquakes$class <- "earthquake"

countries[which(countries$countryName == "New Zealand"),]$latitude <- -43
countries[which(countries$countryName == "New Zealand"),]$longitude <- 172.6


earthquakes$year <- substr(as.character(earthquakes$time), 1, 4)

papers[which(papers$Country == "Russian Federation"),]$Country <- "Russia"


tableau.data <- rbind.fill(
  merge(x = papers[,c("year", "Documents", "Citations", "class", "Country")],
        y = countries[,c("countryName", "continentName", "latitude", "longitude", "population")],
        by.x = "Country",
        by.y = "countryName"),
  earthquakes[,c("time", "year", "latitude", "longitude", "depth", "mag", "id", "place", "type", "class")])

write.xlsx(tableau.data,
           file = "./inst/data/earthquakes.papers.xlsx",
           rowNames = FALSE,
           save.Workbook = TRUE)

eq.p <- createWorkbook("Earthquakes & Papers")

addWorksheet(eq.p, "Earthquakes", gridLines = TRUE)
addWorksheet(eq.p, "Papers", gridLines = TRUE)
writeData(eq.p, sheet = "Papers", merge(x = papers[,c("year", "Documents", "Citations", "class", "Country")],
                                 y = countries[,c("countryName", "continentName", "latitude", "longitude", "population")],
                                 by.x = "Country",
                                 by.y = "countryName"))
writeData(eq.p, sheet = "Earthquakes", earthquakes[,c("time", "year", "latitude", "longitude", "depth", "mag", "id", "place", "type", "class")])

saveWorkbook(eq.p,
             file = "./inst/data/earthquakes.papers.split.xlsx",
             overwrite = TRUE)
