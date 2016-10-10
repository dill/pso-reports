### take the "data" in the PDF reports https://www.data.bsee.gov/homepg/data_center/other/WebStore/pilist.asp?appid=5 and make then into "real" data

library(rPython)
library(stringr)

# need to install the tika library in Python
#   pip install --user tika
# should do that

# need to split PDFs into pages

# at the moment this only decodes the "DETECTION" pages, not effort
## ./splitr 4369.pdf


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
  drop_ind <- which(str_detect(strpped,
                               regex("RPSPROTECTEDSPECIESRECORDINGFORM",
                                     ignore_case=TRUE)))
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
  catted_save <- catted

  # str_replace_all fails me here, so this is goofy...
  for(bit in bits){
    catted <- sub(bit, "--SPLIT--", catted, fixed=TRUE)
  }

  # split it
  catted <- unlist(str_split(catted, "--SPLIT--"))

  # check that the "n/a" didn't get bundled into the wrong field
  # fields can't have both an entry and an n/a (surely?!)
  ct2 <- c()
  for(ii in seq_along(catted)){
    if(str_detect(catted[ii], "n/a") & !str_detect(catted[ii], "^n/a$")){
      repl <- catted[ii]
      repl <- str_replace(repl, "n/a", " n/a ")
      repl <- str_replace(repl, "\\s+", " ")
      repl <- str_trim(repl)
      repl <- unlist(str_split(repl, " "))
      ct2 <- c(ct2, repl)
    }else{
      ct2 <- c(ct2, catted[ii])
    }
  }

  # dispose of first entry
  catted <- ct2[-1]

  # replace "n/a" with NA
  catted <- str_replace_all(catted, "n/a", "NA")
  #catted <- str_replace_all(catted, "", "NA")

  # expand out the distance firstclosestlast thing, as it always gets
  # concatenated
  whbits <- which(bits == "FirstClosestLast")
  bits <- c(bits[1:(whbits-2)],
            "DistanceDuringSoftStart(m)First",
            "DistanceDuringSoftStart(m)Closest",
            "DistanceDuringSoftStart(m)Last",
            bits[(whbits+1):length(bits)])

  # sometimes the description field gets missed out?
  if(!str_detect(catted_save, "Description(includefeaturessuchasoverallsize;shapeofhead;colourandpattern;size,shape,andpositionofdorsalfin;height,direction,andshapeofblow;etc.)")){
    ind <- which(bits=="Description(includefeaturessuchasoverallsize;shapeofhead;colourandpattern;size,shape,andpositionofdorsalfin;height,direction,andshapeofblow;etc.)")
    catted <- c(catted[1:(ind-1)], NA, catted[ind:length(catted)])
  }

  names(catted) <- bits

  return(catted)
}

ll <- lapply(list.files("pages", "*.pdf", full.names=TRUE), parse_detections)

#for(ff in list.files("pages", "*.pdf", full.names=TRUE)){
#  cat(ff, "\n")
#  ll <- parse_detections(ff)
#}



