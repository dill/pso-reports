### what about using python???

library(rPython)
library(stringr)


#filename <- "4369-7.pdf"


parse_detections <- function(filename){
  # using witchcraft here:
  # https://cbrownley.wordpress.com/2016/06/26/parsing-pdfs-in-python-with-tika/
  # from @sharkey
  python.exec("from tika import parser")
  python.assign("filename", filename)
  python.exec("parsedPDF = parser.from_file(filename)")
  tbl_dat <- python.get("parsedPDF[\"content\"]")


  ## parse that
  # two new lines == a real new line?
  tbl_dat <- unlist(str_split(tbl_dat, "\\n\\n"))
  # single new line, convert to space
  tbl_dat <- str_replace_all(tbl_dat, "\\n", " ")

  # strip all the whitespace out to avoid parsing issues from Python
  strpped <- str_replace_all(tbl_dat, " ", "")

  # remove stuff before the title
  drop_ind <- which(str_detect(strpped, "RPSPROTECTEDSPECIESRECORDINGFORM"))
  tbl_dat <- tbl_dat[(drop_ind+1):length(tbl_dat)]

  ## first page of detections starts with "DETECTION  Date" get rid of that
  #tbl_dat[1] <- str_replace(tbl_dat[1], "DETECTION", "")
  #tbl_dat[1] <- str_trim(tbl_dat[1])


  # remove after last line of table "Other notes or comments"
  strpped <- str_replace_all(tbl_dat, " ", "")
  drop_ind <- which(str_detect(strpped, "Othernotesorcomments"))
  tbl_dat <- tbl_dat[1:(drop_ind+1)]

  catted <- str_replace_all(tbl_dat, " ", "")

  bits <- c(
  "Date",
  "ProjectNumber",
  "TimeatFirstDetection(UTC)",
  "TimeatLastDetection(UTC)",
  "VisualDetectionNumber",
  "AcousticDetectionNumber",
  "RegulatoryReferenceNumber",
  "VesselName",
  "SurveyType",
  "Observer(s)",
  "Detectionwasmade",
  "Detectionwasfirstmade",
  "LatitudeofVessel",
  "LongitudeofVessel",
  "HeadingofVessel(degrees)",
  "WaterDepth(metres)",
  "SwellHeight(metres)",
  "BeaufortScale",
  "Precipitation",
  "Visibility(km)",
  "CloudCover(%)",
  "GlareSeverityandDirection",
  "CommonName",
  "ScientificName",
  "CertaintyofIdentification",
  "TotalNumber",
  "NumberofAdults",
  "NumberofJuveniles",
  "Bearingtoanimal(s)whenfirstdetected(degrees)",
  "Rangetoanimal(s)whenfirstdetected(metres)",
  "Description(includefeaturessuchasoverallsize;shapeofhead;colourandpattern;size,shape,andpositionofdorsalfin;height,direction,andshapeofblow;etc.)",
  "VisualSightingand/orAcousticDetectionDetails(notebehavior,especiallychangesinrelationtosourceactivityanddistancefromsourcearray)",
  "DirectionofTravel/FirstApproach(relativetovessel)",
  "InitialCompassHeading(degrees)",
  "FinalCompassHeading(degrees)",
  "Closestdistanceofanimalstoairguns/source(metres)",
  "Closestdistanceofanimalstovessel(metres)",
  "Sourceactivityatinitialdetection",
  "Sourceactivityatfinaldetection",
  "TimeatClosestApproach(UTC)",
  "DistanceDuringSoftStart(m)",
  "FirstClosestLast",
  "Sourcemitigationaction(s)required",
  "Strikeavoidancemaneuversrequired",
  "Duplicatedetection?Ifyes,provideduplicatedetectionnumber",
  "Totaldurationanimalsobservedinexclusionzone(HH:MM)",
  "Totaldurationofproductionlossduetomitigationaction(HH:MM)",
  "Othernotesorcomments")

  catted <- paste0(catted, collapse="")
  catted <- str_replace(catted, "DETECTION", "")


  # str_replace_all fails me here, so this is goofy...
  for(bit in bits){
    catted <- sub(bit, "--SPLIT--", catted, fixed=TRUE)
  }

  # split it
  catted <- unlist(str_split(catted, "--SPLIT--"))

  # dispose of first entry
  catted <- catted[-1]

  # replace "n/a" with NA
  catted <- str_replace_all(catted, "n/a", "NA")
  #catted <- str_replace_all(catted, "", "NA")

  names(catted) <- bits

  return(catted)
}

## ./splitr 4369.pdf
aa <- list()
aa[[1]] <- parse_detections("pages/4369-6.pdf")
aa[[2]] <- parse_detections("pages/4369-7.pdf")
aa[[3]] <- parse_detections("pages/4369-8.pdf")

