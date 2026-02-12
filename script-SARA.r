
# PROCESSING AURORA DATA TO DARWIN CORE STANDARDS:
#
#
# Both EMODnet Biology and OBIS (and GBIF) rely on Darwin Core Archive (DwC-A), the standard for publishing biodiversity data.
# 
# Data structure: in most cases, we will use three tables.
#   - Event (core) table: to store sample and/or observation information (time, location, depth, event hierarchy).
#   - Occurrence table: to store occurrence details (taxonomy, identification, organismID).
#   - Extended Measurements or Facts (eMoF) table: to store sampling information and additional biological and/or abiotic measurements.
#
# Field nomenclature: the field names of each of the 3 tables have to follow the Darwin Core terminology. 
#
# Content: the content, or the data itself, has to follow certain standards: 
#   - date-related fields have to be ISO 8601 compliant
#   - latitude and longitude must be in decimal degrees and referenced to the WGS84 datum
#   - eventID and occurrenceID need to be stable, unique identifiers used to distinguish between separate events and occurrence
#   - taxonomic information should include Life Science Identifiers (LSID)
#   - other parameters (eMoF table) should be standardized through BODC-NERC controlled vocabulary
# 

#
# Clarificar na submissão:
#   - information about the abundances per square meter and life stage is present both in the occurrences table and in the eMoF table
#   - information about the sampling protocol is present both in the event and eMoF tables
#   - habitat information: detailed info present only in the event table
#   - 435 potential duplicate records: correspond to different morphospecies in the same event (image) with the same density 
#     (cf., TIMER_2019_10_01_at_22_57_52_CP4A6642.JPG)
#
# Em falta:
#   - confirmation for coordinateUncertaintyInMeters
#   - confirmation for depth_uncertainty
#   - confirmation for verbatimIdentification from Sofia's/Zé's catalog 
#

# working directory:
setwd("~/AURORA/")

# packages:
library(tidyverse)
library(stringr)
library(worrms)
library(mregions2)
library(leaflet)
library(leaflet.extras2)
library(obistools)
library(EMODnetBiocheck)
library(sf)
library(countrycode)


# raw dataset (with the verified taxonomic IDs):
raw.data <- read.csv(file = "datasets/AURORA_dataset/data_for_processing/aurora_good_quality_data_densitydata_id_corrected.csv", 
                     check.names = FALSE)

# DwC fields required and strongly recommended for OBIS or EMODnet Biology (some also required for GBIF):
dwc.fields <- read.csv("datasets/dwc_terms.csv")

# required fields:
dwc.required <- dwc.fields %>% 
  filter(OBIS == "required" | EMODnet == "required" | GBIF == "required" | OBIS.manual == "required" |
           OBIS == "optional (required for imaging data)") # required only for imaging data

# required fields for our data structure: event core table, occurrence extension table, emof table
dwc.required <- dwc.required %>% 
  filter(Table == "Event" | Table == "Occurrence" | Table == "eMoF" ) %>% pull(Term)


# INITIAL CHECKS ON DATA: ------------------------------------------------------

## Transform to tidy structure ----

# fix species columns by moving to a single column variable
df.long <- raw.data %>%
  pivot_longer(names_to = "Taxa or morphotype",
               values_to = "Densities", 3:38) %>%
  relocate("Taxa or morphotype","Densities", .after = annotation_area_sqm)

# remove observation with 0 in density (only presence data)
df.long <- subset(df.long, Densities != 0)

# fix columns names (to WordWord_unit)
# initial columns will be capitalized, while DwC mapped columns will start with lowercase
colnames(df.long)

colnames(df.long) <- colnames(df.long) %>%
  str_replace_all("_", " ") %>%              # remove _ between words
  str_to_title() %>%                         # capitalize all words
  str_replace_all(" ", "") %>%               # remove space between capitalized words
  str_replace_all("Id", "ID") %>%            # correct words that should be all caps
  str_replace_all("Tpi", "TPI") %>% 
  str_replace_all("\\(Dd\\)", "_dd") %>%     # correct units to lowercase and after a _
  str_replace_all("\\(M\\)", "_m") %>%    
  str_replace_all("Sqm", "_sqm") %>%
  str_replace_all("\\s*\\([^\\)]+\\)", "")   # remove extra info within parenthesis

colnames(df.long)


## Make a map from your data ----
## to ensure the coordinates are valid and within your expected range:

pal <- colorFactor(palette = heat.colors(length(unique(df.long$DiveID))), domain = df.long$DiveID)
mrp_view("eez") %>%
  addCircleMarkers(data = df.long,lng = ~Longitude_dd, lat = ~Latitude_dd, label = ~DiveID, radius = 5, 
                   color = pal(df.long$DiveID), stroke = FALSE, fillOpacity = 1)


## Run basic statistics on each column of numeric data ----
## (min, max, mean, std. dev., etc.) to identify potential issues:
summary(df.long)
df.long$`Altitude/ImageQuality` <- as.character(df.long$`Altitude/ImageQuality`)

df.long %>% summarise(across(where(is.numeric), 
                             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE),
                                  min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE), na = ~sum(is.na(.x))),
                             .names = "{.fn}_{.col}")) %>%
  pivot_longer(everything(), names_to = c("statistic", "variable"), names_pattern = "^([^_]*)_(.*)$",  
               values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value)


## Look at unique values of columns containing ----
## string entries to identify potential issues (eg. spelling): 
df.long %>% select(-c(`Date/Time`, Time)) %>%
  keep(is.character) %>% 
  map(unique)

df.long$SubstrateType[df.long$SubstrateType == "muddy sediments"] <- "Muddy sediments"
df.long$ClassificationOnDeadTalusPresent[df.long$ClassificationOnDeadTalusPresent == "few"] <- "Few"
df.long$ClassificationOnDeadTalusPresent[df.long$ClassificationOnDeadTalusPresent == "In termedian"] <- "Intermedian"

df.long$TaxaOrMorphotype <- gsub("[[:blank:]]$", "", df.long$TaxaOrMorphotype)


# remove Usable and `Lebensspuren/Burrows`, ClassificationOnDeadTalusPresent
df.long <- select(df.long, -c(Usable,
                              SubstrateTypePerGeneralCategory,
                              TPI, # exclude TPI (?)
                              `Lebensspuren/Burrows`,  ClassificationOnDeadTalusPresent)) # confirmar que é para retirar (?)


# MAPPING = translate column headers to DwC terms ------------------------------
#
# To make the mapping step easier, we recommend starting a mapping document/spreadsheet 
# List out all of your column headers in one column and document the appropriate DwC terms in a second column
#
names(df.long)

# rename data columns to match Darwin Core terms
mapping.fields <- c("verbatimIdentification" = "TaxaOrMorphotype",
                    "eventDate" = "Date", 
                    "eventTime" = "Time",
                    "decimalLongitude" = "Longitude_dd", 
                    "decimalLatitude" = "Latitude_dd",
                    "verbatimDepth" = "DepthWater_m",
                    "identifiedBy" = "Annotator",
                    "occurrenceRemarks" = "NotesOnObservations")

df.mapped <- rename(df.long, all_of(mapping.fields))

names(df.mapped)

## Identify columns with measurements ----

abiotic <- c("AnnotationArea_sqm", "Slope" 
             # "TPI" # exclude TPI (?)
             ) 
biotic <- c("Densities")
measurements <- data.frame(measurements = c(abiotic, biotic),
                           association = c(rep("event", length(abiotic)),
                                           rep("occurrence", length(biotic))))

## habitat ----
df.mapped <- df.mapped %>%
  mutate(habitat = case_when(
    ClassificationOnAccumulationOfSpicule != "" ~ paste0(df.mapped$SubstrateType, " | Accumulation of Spicule: ",
                                                         df.mapped$ClassificationOnAccumulationOfSpicule),
    .default = df.mapped$SubstrateType)) %>%
  select(-c("SubstrateType", "ClassificationOnAccumulationOfSpicule"))

# habitat information: could not find controlled vocabs to include the detailed habitat in the emof
#
# add to the list of measurements
# measurements <- rbind(measurements, data.frame(measurements = "SubstrateType", association = "event"))
#
# there is also no controlled vocabulary for the General Category 
# "Marine Habitat Classification for Britain and Ireland Version 15.03" - http://vocab.nerc.ac.uk/collection/M21/current/
# doesn't match for our depths: Arctic upper abyssal: Depth Band	2000 - 3100 m - there is no Artic lower abyssal (only Atlantic)
#
         
## Annotation area to sampleSize ----
df.mapped$sampleSizeValue <- df.mapped$AnnotationArea_sqm
df.mapped$sampleSizeUnit <- "Square metres" # controlled vocabulary (SDN:P06::UMSQ)

## Densities to organismQuantity ----
df.mapped$organismQuantity <- df.mapped$Densities
df.mapped$organismQuantityType <-  "Abundance of biological entity specified elsewhere per unit area of the bed (number per square metre)" # controlled vocabulary (SDN:P01::SDBIOL02)

## DistanceGroup, Altitude/ImageQuality, Overlap, and AnnotationArea to eventRemarks ----

df.mapped <- df.mapped %>% 
  mutate(eventRemarks = paste0("Distance Group: ", df.mapped$DistanceGroup, " | ",
                               "Image Quality: ", df.mapped$`Altitude/ImageQuality`, " | ",
                               df.mapped$Overlap, " | ",
                               "Annotation Area")) %>%
  select(-c(DistanceGroup, `Altitude/ImageQuality`, Overlap, AnnotationArea))


# Check required fields missing
check_fields(df.mapped)


# CREATE UNIQUE IDENTIFIERS ----------------------------------------------------

## eventID and parentEventID ---- 
#
# repeating the parentEventID in the child event (use : as delimiter) can make the 
# structure of the dataset easier to understand
#

# images as events X(Cruise:DiveID:ImageUniqueID)X and dives as parentEvents of images (Cruise:DiveID)
# in this case, use the same ID as in https://doi.pangaea.de/10.1594/PANGAEA.943364 (i.e., Image_Filename)
df.mapped <- df.mapped %>% 
  rename(eventID = ImageFilename) %>%
  mutate(eventType = "StillImage", 
         .before = ImageUniqueID) %>%                                                     
  mutate(parentEventID = paste0(df.mapped$Cruise, ":",                          
                                df.mapped$DiveID),                             
         .after = eventID)

# dives as events (Cruise:DiveID) and cruise as parentEvent of dives (Cruise)
df.dives <- unique(df.mapped[, c("Cruise", "parentEventID", "eventDate")]) %>%
  rename(eventID = parentEventID,                                               
         parentEventID = Cruise) %>%
  mutate(eventType = "Dive")      

df.dives <- df.dives %>%
  group_by(eventID) %>%
  summarise(eventDate = paste(sort(eventDate), collapse = "/"),
            across(-eventDate, ~ paste(unique(.x), collapse = ", "))) %>%
  ungroup()

# cruise as events (Cruise)                                                                      
df.cruise <- unique(df.mapped[, c("Cruise")]) %>%
  rename(eventID = Cruise) %>%
  mutate(eventDate = paste0(min(df.mapped$eventDate), "/", max(df.mapped$eventDate)),
         eventType = "Cruise")                                 

# merge all parentEvents df
parent.events <- dplyr::bind_rows(df.cruise, df.dives)


## occurrenceID ----
## an identifier for an occurrence record that should be persistent and globally unique
# 
# can be the combination of the institutionCode, the collectionCode, and the catalogNumber 
# (or autonumber/idenificationID in the absence of a catalogNumber)
#

df.mapped$identificationID <- formatC(c(1:nrow(df.mapped)), width=4, format = "d",flag = "0")

df.mapped <- df.mapped %>%
  group_by(eventID) %>%
  mutate(identificationID = row_number())%>%
  ungroup()


df.mapped <- df.mapped %>% 
  mutate(occurrenceID = paste0(df.mapped$Cruise,":",
                               df.mapped$DiveID,":",
                               df.mapped$ImageUniqueID,"_",
                               df.mapped$identificationID)) %>%
  select(-c("identificationID"))

# QC: check for the uniqueness of occurrenceID
which(duplicated(df.mapped$occurrenceID)) # should be zero


# TAXONOMY ---------------------------------------------------------------------

## Clean identification information ----
# 
# the field scientificName should only contain the scientific name of the taxon
# other information should be split to other columns (e.g., identificationQualifier and scientificNameAuthor)
# information related to the sex, lifestage, or size of the specimen should be stored in the eMoF table
#

### verbatimIdentification field ----
### important to keep original identification
df.mapped <- df.mapped %>%
  mutate(initial.taxa = verbatimIdentification,
         .after = verbatimIdentification)

unique(df.mapped$initial.taxa)

### identificationQualifier field ----
df.mapped <- df.mapped %>%                                         
  mutate(identificationQualifier = if_else(str_detect(initial.taxa, "\\bindet"), "indet", ""), 
         initial.taxa = str_remove(initial.taxa, "\\s*indet")) %>%
  relocate(identificationQualifier, .after = initial.taxa)

df.mapped$identificationQualifier <- "indet" # all IDs are indet in this case

### lifeStage field ----
df.mapped <- df.mapped %>%
  mutate(lifeStage = if_else(str_detect(initial.taxa, "\\(juveniles\\)"), "juveniles", ""), 
         initial.taxa = str_remove(initial.taxa, "\\(juveniles\\)"))

# add to the list of measurements
measurements <- rbind(measurements, data.frame(measurements = "lifeStage", association = "occurrence"))

### other cleaning ----
df.mapped$initial.taxa <- sub("^([A-Za-z]+).*", "\\1", df.mapped$initial.taxa)  # removes msp
df.mapped$initial.taxa <- gsub("\\.", "", df.mapped$initial.taxa)               # removes .
df.mapped$initial.taxa[df.mapped$initial.taxa == "Animal"] <- "Animalia"

unique(df.mapped$initial.taxa)

# obistools::match_taxa(df.mapped$initial.taxa)
# dá um mismatch a mais do que o worrms::wm_records_taxamatch() - Asconema (?)

## Match with worrms package ----

# our cleaned taxa                                                              
initial.taxa <- na.omit(unique(df.mapped$initial.taxa))                      
initial.taxa <- as.data.frame(initial.taxa)
initial.taxa <- rownames_to_column(initial.taxa, var = "source")

# match to worms
taxmatch <- wm_records_taxamatch(initial.taxa$initial.taxa)                     
df.taxmatch <- bind_rows(taxmatch, .id = "source")

# join our initial taxa with worms' matches
df.taxmatch <- full_join(initial.taxa, df.taxmatch, by = "source")              

### potential ambiguous matches ----
#
# identify cases when:
#   - there is more than 1 worms' record for our initial.taxa
#   - the match_type is not "exact"
#   - the status is not "accepted"
#
non.matches <- unique(
  rbind(
    filter(df.taxmatch, n() > 1, .by = source),
    filter(df.taxmatch, (match_type != "exact") | (status != "accepted"))))     

# remove ambiguous matches from the main dataset
df.taxmatch <- setdiff(df.taxmatch, non.matches)

### identify the right classification
non.matches <- non.matches %>% group_split(source)
for (source in c(0:as.integer(length(non.matches)))) {
  View(non.matches[[source]])
}

### select the right row for each ambiguous match!

non.matches[[1]] <- non.matches[[1]][1,]                                         
non.matches[[2]] <- non.matches[[2]][1,]
# non.matches[[3]] <- non.matches[[3]][1,]
# etc
non.matches <- bind_rows(non.matches)

#
# Note: you should not blindly choose the occurring accepted name as the correct name,
#
#   - it’s important to use the LSID of the match for the field scientificNameID,
#     and not the LSID of the scientificName_accepted
#   - you do not add the authority, nor any other DwC classification fields (kingdom, class, taxonRank)
#     that are returned to you by the Taxon Match Tool. These classification fields should only be filled
#     out if the same information was also provided in the original source dataset.
#

### join corrected matches (non.matches) with the exact matches (df.taxmatch)
df.taxmatch <- rbind(df.taxmatch, non.matches)

### add taxonomic info to the main dataframe ----

# select relevant fields to DwC
# required: scientificName, scientificNameID, kingdom
# recommended: taxonRank,scientificNameAuthorship, genus, order
df.taxmatch <- df.taxmatch %>% 
  select("initial.taxa","scientificname","authority", "lsid", "rank", "kingdom", "order", "genus") %>%
  rename("scientificName" = "scientificname",
         "scientificNameID" = "lsid",
         "scientificNameAuthorship" = "authority",
         "taxonRank" = "rank")

# add fields to main dataframe
df.mapped <- df.mapped %>% 
  left_join(df.taxmatch, by= "initial.taxa") %>%
  select(-c(initial.taxa))

### QC: Any missing taxonomic matches? 
length(which(is.na(df.mapped$scientificName)))


# LOCATION FIELDS --------------------------------------------------------------

summary(df.mapped$decimalLatitude)
summary(df.mapped$decimalLongitude)

## geodeticDatum ----
df.mapped$geodeticDatum <- "WGS84"

## coordinateUncertaintyInMeters ---- 
df.mapped$coordinateUncertaintyInMeters <- 50 # (?)

## countryCode ---- 
## Recommended best practice is to use an ISO 3166-1-alpha-2 country code, 
## or ZZ (for an unknown location or a location unassignable to a single country code), 
## or XZ (for the high seas beyond national jurisdictions)

eez_sf <- mrp_get("eez") %>% st_make_valid()

points_sf <- df.mapped %>%
  select(c(occurrenceID, decimalLongitude, decimalLatitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),crs = 4326)  # WGS84

points_with_eez <- st_join(points_sf, eez_sf)
points_with_eez <- select(points_with_eez, c(occurrenceID, iso_sov1))
points_with_eez$iso_sov1 <- countrycode(points_with_eez$iso_sov1, origin = "iso3c", destination = "iso2c")

df.mapped <- df.mapped %>%
  left_join(st_drop_geometry(points_with_eez), by = "occurrenceID") %>%
  rename("countryCode" = "iso_sov1")

## locationID ----
## can be used to store an identifier for monitoring stations        
df.mapped$locationID <- df.mapped$ImageUniqueID

## locality ----                                                                
## can be used to store a textual location or a description of where the sample was taken
df.mapped <- rename(df.mapped, "locality" = "HydrothermalVentField")                               

## footprintWKT ----
## fill in in cases where it is not possible to provide an exact position for the sampling event
#
# The event can be stored as
#   - a linestring:
#       - footprintWKT should be formatted as "LINESTRING ([start-longitude] [start-latitude], [end-longitude] [end-latitude])"
#       - decimalLatitude and decimalLongitude can then be filled out with the centre point of the linestring 
#       - coordinateUncertaintyInMeters will then represent the distance from the centre to the start or end coordinates
#   - a bounding box polygon:
#       - footprintWKT should be formatted as "POLYGON (([longitude point 1] [latitude point 1], [longitude point 2] [latitude point 2], ..., [longitude point x] [latitude point x], [longitude point 1] [latitude point 1]))"
#       - if only the sampling general area is known:
#           1. add the name of the region in the field locality
#           2. look up this region in 'Marine Regions' - it gives all information to build a bounding box polygon
#
# obistools::calculate_centroid() calculates a centroid and radius for WKT strings. 
# This is useful for populating decimalLongitude, decimalLatitude and coordinateUncertaintyInMeters
#
                   
## depth ----
depth_uncertainty <- 0  # (?)

df.mapped <- df.mapped %>%
  mutate(minimumDepthInMeters = verbatimDepth - depth_uncertainty,
         maximumDepthInMeters = verbatimDepth + depth_uncertainty,
         .after = verbatimDepth)

### QC
report <- obistools::check_depth(df.mapped, report=T,
                                 depthmargin = depth_uncertainty) # How much can the given depth deviate from the bathymetry in the rasters (in meters)
report

# FORMAT DATES AND TIME --------------------------------------------------------
#
# all information stored in eventDate should use the ISO 8601 standard
#
# times can have a time zone indicator at the end. If this is not the case, 
# then the time is assumed to be local time (Z = UTC)
#
df.mapped$eventDate <- format(as.Date(df.mapped$eventDate, format = "%Y-%m-%d"), # current format
                              "%Y-%m-%d") # ISO 8601

df.mapped$eventTime <- format(strptime(df.mapped$eventTime, "%H:%M:%S"), # current format
                              "%H:%M:%S") # ISO 8601
df.mapped$eventTime <- paste0(df.mapped$eventTime, "Z")

# # or:
# df.mapped$eventDate <- format(as.Date(df.mapped$`Date/Time`, format = "%Y-%m-%dT%H:%M:%S"), # current format
#                               "%Y-%m-%dT%H:%M:%S") # ISO 8601
# df.mapped$eventDate <- paste0(df.mapped$eventDate, "Z") 

# QC: are all dates valid? 
obistools::check_eventdate(df.mapped) # should be zero

# MISSING FIELDS ---------------------------------------------------------------

## Required fields missing ----
## for OBIS
obistools::check_fields(df.mapped)
## for OBIS, EMODnet and GBIF
setdiff(c(dwc.required), names(df.mapped))

### occurrenceStatus ----
df.mapped$occurrenceStatus <- "present"

### basisOfRecord ----
df.mapped$basisOfRecord <- "MachineObservation"

### datasetName ----
### the same as the title of your data set
# 
# the dataset title should be as descriptive and complete as possible
# EMODnet Biology recommends titles to contain information about the taxonomic, geographic, and temporal coverage
#
# collectionCode should be an abbreviation of that data set title
# if you already have a globally unique occurrenceID, you can choose to only fill in the datasetName
#
df.mapped$datasetName <- "Megafauna composition in Aurora seamount and hydrothermal vent field (High Arctic) identified during the 'HACON expedition 2019' with the RV Kronprins Haakon (cruise no. 2019708)"

### institutionCode ----
### the code of the legal entity for which your data were collected
df.mapped$institutionCode <- "University of Aveiro" 

### identificationReferences ----
### required only for imaging data
# df.mapped$identificationReferences <- "" # there's not a single reference in this case - multiple references are used per id

### identificationVerificationStatus ----
### https://hiscom.github.io/hispid/vocabulary/identification_verification_status.xml
### verification required; verified; verified by specialist; unverifiable
df.mapped$identificationVerificationStatus <- "verified"

### samplingProtocol ----
### The names of, references to, or descriptions of the methods or protocols used during an Event (http://rs.tdwg.org/dwc/terms/samplingProtocol)
df.mapped$samplingProtocol <- "Seafloor images collected during the 'HACON expedition' with the RV Kronprins Haakon (cruise no. 2019708) (HK19) to the AURORA seamount, high Arctic, 19/09/2019 - 16/10/2019, https://doi.org/10.1594/PANGAEA.943364 "

# add to the list of measurements to be added to the eMoF table
# is common to all the child events, can be associated only to the higher parentEvent (Cruise)
measurements <- rbind(measurements, data.frame(measurements = "samplingProtocol", association = "parentEvent"))

# 
# https://doi.org/10.1594/PANGAEA.943364
#
# seafloor imaging collected by Ocean Floor Observation and Bathymetry System (OFOBS)
# towed camera platform integrating additional acoustical devices
# high resolution photo-camera (iSiTEC, CANON EOS 5D Mark III) and a high-definition video-camera (iSiTEC, Sony FCB-H11)
# during deployments, the OFOBS is lowered to ~1.5 m above the seafloor then towed by the ship / ice drift at speeds of up to 0.8 kn
# every 20 seconds a 26 megapixel still image of the seafloor is taken by the device
#

### sampling instrument ----
# if missing, EMODnet report shows the message "No sampling instrument present" in the general issues

df.mapped$Device <- "Ocean Floor Observation and Bathymetry System (OFOBS)"

# add to the list of measurements to be added to the eMoF table
# is common to all the child events, can be associated only to the higher parentEvent (Cruise)
measurements <- rbind(measurements, data.frame(measurements = "Device", association = "parentEvent")) 


### Are any required fields missing?
#
# measurement fields are expected to be missing at this point
#
obistools::check_fields(df.mapped)
setdiff(c(dwc.required), names(df.mapped))


## Remaining fields not mapped to DwC ----

setdiff(names(df.mapped)[grep("^[A-Z]", names(df.mapped))], measurements$measurements)

# remove if it is redundant information
df.mapped <- select(df.mapped, -c("ImageUniqueID", "Cruise", "DiveID", "Date/Time"))


## Additional fields ----

### associatedReferences ----
# A list (concatenated and separated) of identifiers (publication, bibliographic reference, global unique identifier, URI) of literature associated with the dwc:Occurrence.
df.mapped$associatedReferences <- "https://ria.ua.pt/handle/10773/41132" 


# STRUCTURING DATA IN SEPARATE TABLES ------------------------------------------

## Event table ----
#
# NOTE: At this stage, it is helpful to include abiotic measurements (if available) and 
#       other information related to the sampling event (e.g. samplingProtocol)
#

# select fields for event table
e.fields <- intersect(names(df.mapped),
                      c(dwc.fields$Term[dwc.fields$Table == "Event"],
                        setdiff(obistools::event_fields(),                      # fields for an event core table 
                                dwc.fields$Term[dwc.fields$Table == "Event"]))) # that are missing in the obis checklist table)

# add measurement columns associated to events and parentEvents that will go to the emof table
e.fields <- c(e.fields, 
              measurements$measurements[measurements$association == "event"],
              measurements$measurements[measurements$association == "parentEvent"])

# create event table
event.table <- select(df.mapped, all_of(e.fields))

# add parentEvents
event.table <- dplyr::bind_rows(parent.events, event.table)

# remove duplicates (different occurrences within the same event)
event.table <- event.table[!duplicated(event.table), ]

# fix NA values
event.table$datasetName <- event.table$datasetName[nrow(event.table)]
event.table$institutionCode <- event.table$institutionCode[nrow(event.table)]
event.table$samplingProtocol <- event.table$samplingProtocol[nrow(event.table)]
event.table$Device <- event.table$Device[nrow(event.table)]

# QC: Does the number of rows in the event table match the number of unique events in the mapped df?
nrow(event.table) == (length(unique(df.mapped$eventID)) + nrow(parent.events)) # should be TRUE

# QC: Are all eventID unique?
which(duplicated(event.table$eventID)) # should be  zero

# QC: Are eventID and parentEventID present in the event table?
#     Do all parentEventIDs have a corresponding eventID?
obistools::check_eventids(event.table) # should be zero


## Occurrence table ----
#
# NOTE: At this stage, it is helpful to include the measurements or fields that contain 
#       biological measurements related to the Occurrences (e.g. biomass)
#

# select fields for occurrence table
o.fields <- intersect(names(df.mapped),
                      c(dwc.fields$Term[dwc.fields$Table == "Occurrence"],
                      setdiff(setdiff(obistools::occurrence_fields(), obistools::event_fields()), # only fields for an occurrence extension table (not core)
                              dwc.fields$Term[dwc.fields$Table == "Occurrence"])))                # that are missing in the obis checklist table

# add measurement columns associated to occurrences that will go to the emof table
o.fields <- c(o.fields, measurements$measurements[measurements$association == "occurrence"])

# create occurrence table
occurrence.table <- select(df.mapped, all_of(o.fields))

# QC: Does the number of rows in the occurrence table match the number of occurrence IDs in the mapped df?
nrow(occurrence.table) == length(unique(df.mapped$occurrenceID)) # should be TRUE

# QC: Do all eventIDs in an extension have matching eventIDs in the core table?
obistools::check_extension_eventids(event.table, occurrence.table) # should be zero


## eMoF table ----

# required fields
dwc.fields %>% filter(Table == "eMoF") %>% 
  filter(OBIS == "required" | EMODnet == "required" | GBIF == "required") %>% pull(Term)

#### measurements associated to the parentEvent Cruise ----
emof.pe <- event.table %>% 
  select(c(eventID,
           measurements$measurements[measurements$association == "parentEvent"])) %>%
  filter(eventID == "HACON2019") # select only the higher event (i.e., the cruise HACON2019)

# convert to long format
emof.pe <- emof.pe %>% 
  pivot_longer(all_of(c(measurements$measurements[measurements$association == "parentEvent"])),
               names_to = "measurementType",
               values_to = "measurementValue",
               values_transform = as.character) %>%
  na.omit() %>% filter(measurementValue != "") # Can we remove NA values in slope and TPI (?)


#### measurements associated to events ----
emof.e <- select(event.table, c(eventID,
                                measurements$measurements[measurements$association == "event"]))

# remove rows with NAs for all measurements
emof.e <- emof.e %>%
  filter(!if_all((measurements$measurements[measurements$association == "event"]), 
                                           is.na))

# convert to long format
emof.e <- emof.e %>% 
  pivot_longer(all_of(c(measurements$measurements[measurements$association == "event"])),
               names_to = "measurementType",
               values_to = "measurementValue",
               values_transform = as.character) %>%
  na.omit() %>% filter(measurementValue != "") # Can we remove NA values in slope and TPI (?)
  

#### measurements associated to occurrences ----
emof.o <- select(occurrence.table, c(eventID, occurrenceID,
                                     measurements$measurements[measurements$association == "occurrence"]))

# convert to long format
emof.o <- emof.o %>% 
  pivot_longer(all_of(measurements$measurements[measurements$association == "occurrence"]), 
               names_to = "measurementType",
               values_to = "measurementValue",
               values_transform = as.character) %>%
  filter(measurementValue != "") # Can we remove NA values in lifeStage (?)


#### merge all dataframes and add units ----
emof.e <- emof.e %>% mutate(occurrenceID = "") %>% relocate(occurrenceID, .after = eventID) 
emof.pe <- emof.pe %>% mutate(occurrenceID = "") %>% relocate(occurrenceID, .after = eventID)
emof.table <- rbind(emof.pe, emof.e)
emof.table <- rbind(emof.table, emof.o)

# QC: should be zero
which(duplicated(emof.table))

# QC: Do all eventIDs in an extension have matching eventIDs in the core table?
obistools::check_extension_eventids(event.table, emof.table) # should be zero

# QC: Are there NAs or blanks?
filter(emof.table, measurementValue == "") # should be zero
sum(is.na(emof.table$measurementValue)) # should be zero


# add measurement units (to annotation area, slope, and densities)
unique(emof.table$measurementType)

emof.table <- emof.table %>% mutate(measurementUnit = 
                                      case_when(measurementType == "AnnotationArea_sqm" ~ "sqm",
                                                measurementType == "Slope" ~ "degrees",
                                                measurementType == "Densities" ~ "ind per sqm",
                                                .default = ""))


### controlled vocabularies ----
### https://vocab.nerc.ac.uk/search_nvs/
#
# The most often used collections within OBIS-ENV format are:
#   - measurementTypes: P01
#   - measurementValues: S11; S10; L05; L22; C17
#   - measurementUnits: P06
#
# NOTE: although measurementType, measurementValue, and measurementUnit are free text fields, 
#       it is recommended to fill them in with the "preferred label" given by the BODC parameter
#

#### measurementType and measurementTypeID ----
#### preferred label and machine-readable label (URI)                          
unique(emof.table$measurementType)
emof.table <- emof.table %>% mutate(measurementType = 
                                    case_when(measurementType == "AnnotationArea_sqm" ~ "Area of sample {sampled area} from the bed by image analysis", #SDN:P01::WASPXX01
                                              measurementType == "Slope" ~ "ground_slope_angle", #SDN:P07::GZCFTK5R
                                              measurementType == "Densities" ~ "Abundance of biological entity specified elsewhere per unit area of the bed", #SDN:P01::SDBIOL02
                                              measurementType == "lifeStage" ~ "Development stage of biological entity specified elsewhere", #SDN:P01::LSTAGE01
                                              measurementType == "samplingProtocol" ~ "Sampling protocol", #SDN:P01::SAMPPROT
                                              measurementType == "Device" ~ "Name of sampling instrument", #SDN:P01::NMSPINST
                                              .default = measurementType))

emof.table <- emof.table %>% mutate(measurementTypeID = 
                                      case_when(measurementType == "Area of sample {sampled area} from the bed by image analysis" ~ "http://vocab.nerc.ac.uk/collection/P01/current/WASPXX01/",
                                                measurementType == "ground_slope_angle" ~ "http://vocab.nerc.ac.uk/collection/P07/current/GZCFTK5R/",
                                                measurementType == "Abundance of biological entity specified elsewhere per unit area of the bed" ~ "http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL02/",
                                                measurementType == "Development stage of biological entity specified elsewhere" ~ "http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/",
                                                measurementType == "Sampling protocol" ~ "http://vocab.nerc.ac.uk/collection/P01/current/SAMPPROT/",
                                                measurementType == "Name of sampling instrument" ~ "http://vocab.nerc.ac.uk/collection/P01/current/NMSPINST/",
                                                .default = ""))

#### measurementValue and measurementValueID ----
#### preferred label and machine-readable label (URI)
emof.table <- emof.table %>% mutate(measurementValue =
                                    case_when(measurementValue == "juveniles" ~ "juvenile",
                                              .default = measurementValue))

emof.table <- emof.table %>% mutate(measurementValueID = 
                                    case_when(measurementValue == "juvenile" ~ "http://vocab.nerc.ac.uk/collection/S11/current/S1127/",
                                              .default = ""))

#### measurementUnit and measurementUnitID ----
#### preferred label and machine-readable label (URI)
emof.table <- emof.table %>% mutate(measurementUnit = 
                                      case_when(measurementUnit == "sqm" ~ "Square metres", #SDN:P06::UMSQ
                                                measurementUnit == "degrees" ~ "Degrees", #SDN:P06::UAAA
                                                measurementUnit == "ind per sqm" ~ "Number per square metre", #SDN:P06::UPMS
                                                measurementType == "TPI" ~ "Dimensionless", #SDN:P06::UUUU, # include/exclude TPI?
                                                measurementType == "Development stage of biological entity specified elsewhere" ~ "Not applicable", #SDN:P06::XXXX
                                                measurementType == "Sampling protocol" ~ "Not applicable", #SDN:P06::XXXX
                                                measurementType == "Name of sampling instrument" ~ "Not applicable", #SDN:P06::XXXX
                                                .default = measurementUnit))

emof.table <- emof.table %>% mutate(measurementUnitID = 
                                    case_when(measurementUnit == "Square metres" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UMSQ/",
                                              measurementUnit == "Degrees" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UAAA/",
                                              measurementUnit == "Number per square metre" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPMS/",
                                              measurementUnit == "Dimensionless" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UUUU/",
                                              measurementUnit == "Not applicable" ~ "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/",
                                              .default = ""))


## QC: are all fields attributed to a table?

setdiff(names(df.mapped),
        c(names(event.table), names(occurrence.table), names(emof.table))) # should be zero


# FINAL CHECKS -----------------------------------------------------------------

## Remove non-mapped fields ---- 
# 
# NOTE: any terms related to measurements, either biotic or abiotic, will be
#       included in the extendedMeasurementOrFact table. Measurements can remain in 
#       the Occurrence table as long as they can be mapped to the appropriate DwC term 
#       (e.g. DwC:Occurrence:sex). Not every data aggregator outside of OBIS indexes the 
#       eMoF table, so otherwise this information may be lost.
#       https://manual.obis.org/format_occurrence.html#how-to-format-occurrence-tables
#

# remove measurement columns from event table if they are in the emof.table (fields not mapped)
names(event.table)[grep("^[A-Z]", names(event.table))]
event.table <- select(event.table, -c(names(event.table)[grep("^[A-Z]", names(event.table))]))
# remove measurement columns from occurrence table if they are in the emof.table (fields not mapped)
names(occurrence.table)[grep("^[A-Z]", names(occurrence.table))]
occurrence.table <- select(occurrence.table, -c(names(occurrence.table)[grep("^[A-Z]", names(occurrence.table))]))
# remove non-mapped columns in emof table if any
names(emof.table)[grep("^[A-Z]", names(emof.table))]
emof.table <- select(emof.table, -c(names(emof.table)[grep("^[A-Z]", names(emof.table))]))


## Reorder fields in DwC tables ----

### event table
dwc.fields %>% 
  filter(Term %in% names(event.table), Table == "Event") %>% 
  arrange(Class)

col_order <- c("eventID", "parentEventID", "eventType", "eventDate", "eventTime",
               "sampleSizeValue", "sampleSizeUnit", "eventRemarks",
               "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", "geodeticDatum", "locationID",
               "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "habitat",
               "locality", "countryCode",
               "datasetName", "institutionCode", "samplingProtocol")

event.table <- event.table[, col_order]

### occurrence table
dwc.fields %>% 
  filter(Term %in% names(occurrence.table), Table == "Occurrence") %>% 
  arrange(Class) %>% pull(Term)

col_order <- c("eventID", "occurrenceID", "basisOfRecord",  
               "verbatimIdentification", "scientificName", "scientificNameID", "scientificNameAuthorship",
               "taxonRank", "kingdom", "order", "genus", "occurrenceStatus", 
               "identificationQualifier", "lifeStage", "organismQuantity", "organismQuantityType",
               "identifiedBy", "occurrenceRemarks", "identificationVerificationStatus",
               "datasetName", "institutionCode", "associatedReferences")

occurrence.table <- occurrence.table[, col_order]

### emof table
dwc.fields %>% 
  filter(Term %in% names(emof.table), Table == "eMoF") %>% 
  arrange(Class) %>% pull(Term)

col_order <- c("eventID", "occurrenceID",
               "measurementType", "measurementTypeID",
               "measurementValue", "measurementValueID", 
               "measurementUnit", "measurementUnitID")

emof.table <- emof.table[, col_order]


## Check required fields per table ----

required.e <- dwc.fields %>% filter(Table == "Event") %>%
  filter(OBIS == "required" | EMODnet == "required" | GBIF == "required") %>% pull(Term)

setdiff(required.e, names(event.table)) # should be zero


required.o <- dwc.fields %>% filter(Table == "Occurrence") %>%
  filter(OBIS == "required" | EMODnet == "required" | GBIF == "required") %>% pull(Term)

setdiff(required.o, names(occurrence.table)) # should be zero


required.emof <- dwc.fields %>% filter(Table == "eMoF") %>%
  filter(OBIS == "required" | EMODnet == "required" | GBIF == "required") %>% pull(Term)

setdiff(required.emof, names(emof.table)) # should be zero


## EMODnet Report {EMODnetBiocheck} ----

emodnet.report <- checkdataset(Event = event.table, 
                               Occurrence = occurrence.table, 
                               eMoF = emof.table)


## Save DwC tables ----

# write.csv(event.table, 'datasets/AURORA_dataset/DwC_files/density/event.csv', row.names = FALSE, na= "")
# write.csv(occurrence.table, 'datasets/AURORA_dataset/DwC_files/density/occurrences.csv', row.names = FALSE, na= "")
# write.csv(emof.table, 'datasets/AURORA_dataset/DwC_files/density/emof.csv', row.names = FALSE, na= "")

