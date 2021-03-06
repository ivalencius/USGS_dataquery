SSC_Retrieval
================
Ilan Valencius
2022-06-16

# Use: “For retrieving USGS suspended sediment concentration and discharge data along a river transect”

## Import Packages

``` r
library(dataRetrieval)
library(sp)
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.3.2, PROJ 7.2.1; sf_use_s2() is TRUE

``` r
library(maptools)
```

    ## Checking rgeos availability: TRUE
    ## Please note that 'maptools' will be retired by the end of 2023,
    ## plan transition at your earliest convenience;
    ## some functionality will be moved to 'sp'.

``` r
library(magrittr)
library(maps)
library(rgdal)
```

    ## Please note that rgdal will be retired by the end of 2023,
    ## plan transition to sf/stars/terra functions using GDAL and PROJ
    ## at your earliest convenience.
    ## 
    ## rgdal: version: 1.5-32, (SVN revision 1176)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.3.2, released 2021/09/01
    ## Path to GDAL shared files: C:/Users/valencig/AppData/Local/Programs/R/R-4.2.0/library/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: C:/Users/valencig/AppData/Local/Programs/R/R-4.2.0/library/rgdal/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.4-7
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.

``` r
library(ggplot2)
library(usmap)
library(stringr)
library(dataRetrieval)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(patchwork)
library(dams)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(nhdplusTools)
```

    ## USGS Support Package: https://owi.usgs.gov/R/packages.html#support

``` r
library(riverdist)
library(GISTools)
```

    ## Loading required package: RColorBrewer

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:patchwork':
    ## 
    ##     area

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## Loading required package: rgeos

    ## rgeos version: 0.5-9, (SVN revision 684)
    ##  GEOS runtime version: 3.9.1-CAPI-1.14.2 
    ##  Please note that rgeos will be retired by the end of 2023,
    ## plan transition to sf functions using GEOS at your earliest convenience.
    ##  GEOS using OverlayNG
    ##  Linking to sp version: 1.5-0 
    ##  Polygon checking: TRUE

    ## 
    ## Attaching package: 'GISTools'

    ## The following object is masked from 'package:maps':
    ## 
    ##     map.scale

``` r
library(rgeos)
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     extract

``` r
library(qpcR)
```

    ## Loading required package: minpack.lm

    ## Loading required package: rgl

    ## 
    ## Attaching package: 'rgl'

    ## The following object is masked from 'package:rgeos':
    ## 
    ##     triangulate

    ## Loading required package: robustbase

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

## Set up imports

-   00060: discharge, cubic feet per second
-   80154: Suspended sediment concentration, milligrams per liter
-   Dates should be in in YYYY-MM-DD format

``` r
wd_root <- 'D:/valencig/Thesis/USGS_dataquery'
centerline_kml <- "D:/valencig/Thesis/Data/chattahoochee_shape/chattahoochee_centerline.kml"

# Location of dams saved as kml files in dam_read_dir
# dam_names <- paste0(c("Buford Dam",
#                       "Morgan Falls",
#                       "Langdale Dam",
#                       "Eagle and Phoenix Dam",
#                       "West Point Dam",
#                       "Riverview Dam",
#                       "Bartletts Ferry Dam",
#                       "Goat Rock Dam",
#                       "Oliver Dam",
#                       "North Highlands Dam",
#                       "Walter F. George Dam",
#                       "George W Andrew Lock and Dam",
#                       "Jim Woodruff Dam"), ".kml")
# dam_read_dir <- "D:/valencig/Thesis/Data/chattahoochee_shape/dams/"

# When you want station info
startDate <- "1990-01-01"
endDate <- "1995-01-01"

## Query parameters for station data ##

# Daily value parameters
dv_parameters <- c("00060")
# Water quality parameters
wq_parameters <- c("80154") #(SSC, mgL)
# for WQP portal, characteristic name is hardcoded in
# Stat Codes
statCd <- c("00003") # stat code: mean

## If using station numbers from file ##

# 8 digit station numbers of interest, column name must be 'site_no'
station_file <- "D:/valencig/Thesis/USGS_dataquery/imports/station_numbers.csv"
usgs_prefix <- FALSE # do station numbers have 'USGS-' in title

## If getting station data from transect ##
upstream_distance_km <- 686

## Dam info ## 
use_dam_file <- TRUE # if set to false will autoquery data from NID website
dam_file <- 'D:/valencig/Thesis/USGS_dataquery/imports/nation.csv'

## Predicted SSC from landsat ##
landsat_csv <- 'D:/valencig/Thesis/chattahoochee-dams/chattahoochee-dams-exports/SSC_pred.csv'

## Don't touch this code ##

# helper function -> pads a character string with leading 0s (for USGS codes)
pad0 <- function(x) {
  while (nchar(x) < 8) {
    x <- paste0("0", x)
  }
  return(x)
}

centerline <- as.data.frame(maptools::getKMLcoordinates(centerline_kml, 
                                                        ignoreAltitude = TRUE))
names(centerline) <- c('lon','lat')
# Convert centerline to sf for plotting
centerline_sf <- st_as_sf(centerline, coords=c("lon","lat")) %>% st_set_crs(4326)

wd_root <- paste0(wd_root, '/', startDate, '_', endDate)
if(!dir.exists(wd_root)){
    dir.create(wd_root)
}
```

## Plot centerline transect

``` r
# plot_dam <- function(dam_name){
#   dam <- as.data.frame(t(as.data.frame(maptools::getKMLcoordinates(paste0(dam_read_dir, dam_name),
#                                                         ignoreAltitude = TRUE))))
#   names(dam) <- c('lon','lat')
#   dam_transform <- usmap_transform(dam)
#   geom_point(data = dam_transform,
#              aes(x = x, y = y),
#              color = "green",
#              shape=2)
# }
# 
# centerline_transform <- usmap_transform(centerline)
# 
# # Create Dam Map
# dam_map = plot_usmap(fill = "black", alpha = 0.5, color = "white", size = 1,
#            include= c("GA","FL", "AL"), labels = TRUE) +
#   labs(title = "Chattahooche Centerline", subtitle = "Buford Dam to Brothers River") +
#   geom_path(data = centerline_transform,
#             aes(x = x, y = y),
#             color = "red",
#             size = 1) +
#   lapply(dam_names, plot_dam)
# 
# dam_map
# 
# # Save Map
# ggsave(dam_map,
#        filename = 'exports/rivertransect.png',
#        width = 5, height = 8)
```

## Get station info and plot station locations (from file)

``` r
# Function to remove empty columns of query
# rm_empty <- function(data){
#   empty_columns <- colSums(is.na(data) | data == "") == nrow(data)
#   return(data[, !empty_columns])
# }
# stations <- read.csv(station_file,colClasses=c("character")) # read as character to keep leading zeros
# if (usgs_prefix) {
#   # remove USGS-prefix
#   station_nums <- unlist(strsplit(stations$site_no, split='-'))[c(FALSE, TRUE)]
# } else {
#   station_nums <- stations$site_no
# }
# station_nums <- unlist(lapply(station_nums, pad0))
# station_data <- readNWISsite(station_nums)
# station_data <- rm_empty(station_data)

# To export information on stations to csv
# write.csv(station_data, "exports/valencius_chattahoochee_usgs_stations.csv", row.names=FALSE)
# 
# station_transform = usmap_transform(station_data, 
#                                       input_names = c("dec_long_va", "dec_lat_va"))
# 
# station_map <- plot_usmap(fill = "black", alpha = 0.5, color = "white", size = 1, 
#            include= c("GA","FL", "AL"), labels = TRUE) +
#   labs(title = "USGS Station Locations") +
#   geom_path(data = centerline_transform,
#             aes(x = x, y = y),
#             color = "red",
#             size = 1) +
#   geom_point(data = station_transform,
#               aes(x = x, y = y),
#               color = "yellow",
#              shape=4)
# 
# station_map

# Save Map
# ggsave(station_map,
#        filename = 'exports/valencius_stationlocs.png',
#        width = 5, height = 8)
```

## Query each station (from file)

``` r
## Get daily discharge data ##
# dv <- readNWISdv(station_nums, dv_parameters, startDate, endDate, statCd = statCd)
# dv_filtered <- data.frame(dv["site_no"], 
#                           dv["Date"],
#                           dv["X_00060_00003"]) # X_00060_00003 will need to change depending on your stat codes
## Get water quality data ##
# wq <- readWQPqw(paste0("USGS-", station_nums), wq_parameters, startDate, endDate)

#stations <- read.table(station_file, header = TRUE, colClasses = "character")

## Get daily discharge data ##
# names(dv_filtered) <- c("site_no", "date", "discharge")

# Strip 'USGS-' identifier from station numbers
# wq["MonitoringLocationIdentifier"] <- lapply(list(wq[["MonitoringLocationIdentifier"]]), substring,
#                                                first = 6)
# wq_filtered <- data.frame(wq["MonitoringLocationIdentifier"],
#                             wq["ActivityStartDate"],
#                             wq["ResultMeasureValue"])
# names(wq_filtered) <- c("site_no","date", "sscmgL")
## To export station data to csv ##
# write.csv(dv_filtered, "exports/stations_from_file/valencius_discharge.csv", row.names=FALSE)
# write.csv(wq_filtered, "exports/stations_from_file/valencius_ssc.csv", row.names=FALSE)
```

## Automatically extract SSC and Q from stations

-   **Note:** this may take a while

``` r
## Run if you have previously saved data from {r get basin} ##
# save_dir <- paste0(wd_root, '/variables/')
# load(file=paste0(save_dir, 'riverdata.Rdata'))
# load(file=paste0(save_dir, 'nwis_stations.Rdata'))
# load(file=paste0(save_dir, 'wqp_stations.Rdata'))
# load(file=paste0(save_dir, 'SSC_full.Rdata'))
# load(file=paste0(save_dir, 'Q_full.Rdata'))
```

``` r
line2network <- function (sp = NA, path = ".", layer = NA, tolerance = 100, 
  reproject = NULL, supplyprojection = NULL) 
{
  if (suppressWarnings(is.na(sp))) {
    sp <- suppressWarnings(rgdal::readOGR(dsn = path, layer = layer, 
      verbose = F))
  }
  if (class(sp) != "SpatialLinesDataFrame") {
    stop("Specified shapefile is not a linear feature.")
  }
  if (is.na(sp@proj4string@projargs) & !is.null(supplyprojection)) {
    sp@proj4string@projargs <- supplyprojection
  }
  if (is.na(sp@proj4string@projargs)) {
    stop("Shapefile projection information is missing.  Use supplyprojection= to specify a Proj.4 projection to use.  If the input shapefile is in WGS84 geographic (long-lat) coordinates, this will be +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 (in double-quotes).  If so, it must also be reprojected using reproject=.")
  }
  proj4 <- strsplit(sp@proj4string@projargs, split = " ")
  projected <- sp::is.projected(sp)
  if (is.null(reproject) & !projected) 
    stop("Distances can only be computed from a projected coordinate system.  Use reproject= to specify a Proj.4 projection to use.")
  if (!is.null(reproject)) {
    sp <- sp::spTransform(sp, sp::CRS(reproject))
    proj4 <- strsplit(sp@proj4string@projargs, split = " ")
  }
  units <- "unknown"
  for (i in 1:length(proj4[[1]])) {
    if (proj4[[1]][i] != "") {
      proj4arg <- strsplit(proj4[[1]][i], split = "=")
      if (proj4arg[[1]][1] == "+units") {
        units <- proj4arg[[1]][2]
        cat("\n", "Units:", proj4arg[[1]][2], "\n")
      }
    }
  }
  if (length(sp@lines) > 1) {
    sp_line <- NA
    sp_seg <- NA
    lines <- list()
    j <- 1
    for (i in 1:length(sp@lines)) {
      for (k in 1:length(sp@lines[i][[1]]@Lines)) {
        lines[[j]] <- sp@lines[i][[1]]@Lines[[k]]@coords
        sp_line[j] <- i
        sp_seg[j] <- k
        j <- j + 1
      }
    }
  }
  if (length(sp@lines) == 1) {
    lines <- sp@lines[1][[1]]@Lines
    length <- length(lines)
    lines.new <- list()
    for (i in 1:length) {
      lines.new[[i]] <- lines[[i]]@coords
    }
    lines <- lines.new
    sp_line <- rep(1, length)
    sp_seg <- 1:length
  }
  length <- length(lines)
  rivID <- 1:length
  lineID <- data.frame(rivID, sp_line, sp_seg)
  connections <- calculateconnections(lines = lines, tolerance = tolerance)
  if (any(connections %in% 5:6)) 
    braided <- TRUE
  lengths <- rep(NA, length)
  for (i in 1:length) {
    lengths[i] <- pdisttot(lines[[i]])
  }
  names <- rep(NA, length)
  mouth.seg <- NA
  mouth.vert <- NA
  mouth <- list(mouth.seg, mouth.vert)
  names(mouth) <- c("mouth.seg", "mouth.vert")
  sequenced <- FALSE
  braided <- NA
  cumuldist <- list()
  for (i in 1:length) {
    xy <- lines[[i]]
    n <- dim(xy)[1]
    cumuldist[[i]] <- c(0, cumsum(sqrt(((xy[1:(n - 1), 1] - 
      xy[2:n, 1])^2) + ((xy[1:(n - 1), 2] - xy[2:n, 2])^2))))
  }
  out.names <- c("sp", "lineID", "lines", "connections", "lengths", 
    "names", "mouth", "sequenced", "tolerance", "units", 
    "braided", "cumuldist")
  out <- list(sp, lineID, lines, connections, lengths, names, 
    mouth, sequenced, tolerance, units, braided, cumuldist)
  names(out) <- out.names
  class(out) <- "rivernetwork"
  length1 <- length(out$lengths)
  length2 <- length(out$lengths)
  if (length2 < length1) 
    cat("\n", "Removed", length1 - length2, "duplicate segments.", 
      "\n")
  length3 <- length(out$lengths)
  if (length3 < length2) 
    cat("\n", "Removed", length2 - length3, "segments with lengths shorter than the connectivity tolerance.", 
      "\n")
  return(out)
}
```

``` r
save_dir <- paste0(wd_root,'/variables/')
if(!dir.exists(save_dir)){
    dir.create(save_dir)
}
if(!dir.exists(paste0(wd_root,'/exports/'))){
    dir.create(paste0(wd_root,'/exports/'))
}
## Determine Watershed Geometries ##
summarize.nldi = function(input){
  data.frame(name = names(input), 
             class = sapply(input, class)[1], 
             row.names = NULL) %>% 
    mutate(feature_count = ifelse(class == "sf", sapply(input, nrow), 
                                  sapply(input, length)))
}
# Get last lat, long pair from centerline
start_loc <- tail(centerline, 1)
start_loc <- c(start_loc[[1]], start_loc[[2]]) 
riverdata <- findNLDI(location = start_loc,
              nav = c("UT", "UM"),
              find = c("nwis", "WQP","basin", "flowlines"),
              distance_km = upstream_distance_km)

summarize.nldi(riverdata)
```

    ##           name class feature_count
    ## 1       origin    sf             1
    ## 2        basin    sf             1
    ## 3  UT_nwissite    sf          1602
    ## 4       UT_WQP    sf          4077
    ## 5 UT_flowlines    sf         24358
    ## 6  UM_nwissite    sf           261
    ## 7       UM_WQP    sf           631
    ## 8 UM_flowlines    sf           546

``` r
## Merge Site Names ##
# get all WQP sites
mc_names <- paste0("USGS-",lapply(riverdata$UM_WQP$comid, pad0))
ut_names <- paste0("USGS-",lapply(riverdata$UT_WQP$comid, pad0))
# merge with nwis sites
mc_names <- unique(append(mc_names, riverdata$UM_nwissite$identifier))
ut_names <- unique(append(ut_names, riverdata$UT_nwissite$identifier))
# Remove mislabelled sites outside of the watershed (remove from ut and mc)
mc_names <- mc_names[- match("USGS-02297310",mc_names)]
mc_names <- mc_names[- match("USGS-02297272",mc_names)]
mc_names <- mc_names[- match("USGS-03298550",mc_names)]
ut_names <- ut_names[- match("USGS-02297310",ut_names)]
ut_names <- ut_names[- match("USGS-02297272",ut_names)]
ut_names <- ut_names[- match("USGS-03298550",ut_names)]
ut_names <- ut_names[- match("USGS-02296750",ut_names)]

## Get Dicharge Data ##riverdata$UM_nwissite$identifier
Q_mc <- data.table(readNWISdv(gsub("USGS-", "", mc_names), 
                parameterCd = dv_parameters, 
                startDate = startDate,
                endDate = endDate) %>% renameNWISColumns())[,":=" (
                  site_no = paste0("USGS-", site_no),
                  date = Date,
                  flow = Flow,
                  portal = 'NWIS',
                  channel = 'MAIN STEM'
                )][,.(site_no, date, flow, portal, channel)]
Q_ut <- data.table(readNWISdv(gsub("USGS-", "", ut_names), 
                parameterCd = dv_parameters, 
                startDate = startDate,
                endDate = endDate) %>% renameNWISColumns())[,":=" (
                  site_no = paste0("USGS-", site_no),
                  date = Date,
                  flow = Flow,
                  portal = 'NWIS',
                  channel = 'TRIBUTARY'
                )][,.(site_no, date, flow, portal, channel)]

# Remove main channel sites from ut sites
Q_ut <- anti_join(Q_ut, Q_mc, by="site_no")

## Get SSC data ##
SSC_wqp_mc <- data.table(readWQPdata(siteNumbers=mc_names, 
                parameterCd = wq_parameters,
                startDate=startDate,
                endDate=endDate
                ))[,":=" (
                  site_no = MonitoringLocationIdentifier,
                  date = ActivityStartDate,
                  sscmgmL = ResultMeasureValue,
                  portal = 'WQP',
                  channel = 'MAIN STEM'
                )][,.(site_no, date, sscmgmL, portal, channel)]

SSC_wqp_ut <- data.table(readWQPdata(siteNumbers=ut_names, 
                parameterCd = wq_parameters,
                startDate = startDate,
                endDate = endDate
                ))[,":=" (
                  site_no = MonitoringLocationIdentifier,
                  date = ActivityStartDate,
                  sscmgmL = ResultMeasureValue,
                  portal = 'WQP',
                  channel = 'TRIBUTARY'
                )][,.(site_no, date, sscmgmL, portal, channel)]
# Remove main channel sites from ut sites
SSC_wqp_ut <- anti_join(SSC_wqp_ut, SSC_wqp_mc, by="site_no")

## Combine upper tributaries and main channel ##
Q_full <- rbind(Q_mc, Q_ut)
SSC_full <- rbind(SSC_wqp_mc, SSC_wqp_ut)

## Identify Stations of Interest ##
# NWIS sites
mc_nwis_names <- unique(Q_mc$site_no)
ut_nwis_names <- unique(Q_ut$site_no)

mc_nwis <- data.table(whatNWISsites(site=gsub("USGS-", "",mc_nwis_names)))[,":="(
    portal = "NWIS",
    site_no = paste0("USGS-", site_no),
    lat = dec_lat_va,
    lon = dec_long_va,
    channel="MAIN STEM"
  )][,.(portal, site_no, station_nm,site_tp_cd, lat, lon, channel)]
ut_nwis <- data.table(whatNWISsites(site=gsub("USGS-", "",ut_nwis_names)))[,":="(
    portal = "NWIS",
    site_no = paste0("USGS-", site_no),
    lat = dec_lat_va,
    lon = dec_long_va,
    channel="TRIBUTARY"
  )][,.(portal, site_no, station_nm,site_tp_cd, lat, lon, channel)]

# WQP sites
mc_wqp_names <- unique(SSC_wqp_mc$site_no)
ut_wqp_names <- unique(SSC_wqp_ut$site_no)

mc_wqp <- data.table(whatWQPsites(siteid=mc_wqp_names))[,":="(
    portal = "WQP",
    site_no = MonitoringLocationIdentifier,
    station_nm = MonitoringLocationName,
    site_code = MonitoringLocationTypeName,
    drainage_area_km2 = DrainageAreaMeasure.MeasureValue*(2.58999/1.00000073), # sq mi -> sq km
    lat = LatitudeMeasure,
    lon = LongitudeMeasure,
    elevation_m = VerticalMeasure.MeasureValue *0.3048, # ft -> m
    channel="MAIN STEM"
  )][,.(portal, site_no, station_nm, site_code, lat, lon, drainage_area_km2, elevation_m, channel)]
ut_wqp <- data.table(whatWQPsites(siteid=ut_wqp_names))[,":="(
    portal = "WQP",
    site_no = MonitoringLocationIdentifier,
    station_nm = MonitoringLocationName,
    site_code = MonitoringLocationTypeName,
    drainage_area_km2 = DrainageAreaMeasure.MeasureValue*(2.58999/1.00000073), # sq mi -> sq km
    lat = LatitudeMeasure,
    lon = LongitudeMeasure,
    elevation_m = VerticalMeasure.MeasureValue *0.3048, # ft -> m
    channel="TRIBUTARY"
  )][,.(portal, site_no, station_nm, site_code, lat, lon, drainage_area_km2, elevation_m, channel)]  
## Map gauge location to river distance ##
# trace(line2network, edit=T) --> comment out following two lines
# suppressMessages(out <- removemicrosegs(out))
# suppressMessages(out <- removeduplicates(out))
S1 <- as(riverdata$UM_flowlines, 'Spatial')
nldi_proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
AKalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154
    +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
river_center <- line2network(S1, reproject=AKalbers)
```

    ## 
    ##  Units: m

``` r
# Apply projection to mc nwis sites then convert into meter projection
mc_nwis_spatial <- mc_nwis
mc_nwis_spatial$lat <- as.numeric(mc_nwis_spatial$lat)
mc_nwis_spatial$lon <- as.numeric(mc_nwis_spatial$lon)
coordinates(mc_nwis_spatial) <- ~lon + lat
proj4string(mc_nwis_spatial) <- '+init=epsg:4326'
mc_nwis_spatial <- spTransform(mc_nwis_spatial, CRS(AKalbers))
mc_nwis_spatial <- as.data.frame(mc_nwis_spatial)
nwis_riv <- xy2segvert(x=mc_nwis_spatial$lon, y=mc_nwis_spatial$lat, rivers=river_center)
# Do the same for mc wqp sites
mc_wqp_spatial <- mc_wqp
mc_wqp_spatial$lat <- as.numeric(mc_wqp_spatial$lat)
mc_wqp_spatial$lon <- as.numeric(mc_wqp_spatial$lon)
coordinates(mc_wqp_spatial) <- ~lon + lat
proj4string(mc_wqp_spatial) <- '+init=epsg:4326'
mc_wqp_spatial <- spTransform(mc_wqp_spatial, CRS(AKalbers))
mc_wqp_spatial <- as.data.frame(mc_wqp_spatial)
wqp_riv <- xy2segvert(x=mc_wqp_spatial$lon, y=mc_wqp_spatial$lat, rivers=river_center)

# Determine distance from start of channel for nwis sites
distances_nwis <- list()
for (i in 1:length(nwis_riv$vert)){
  # Distance to mouth of river
  dist_m <- riverdistance(startseg = nwis_riv$seg[i], startvert = nwis_riv$vert[i],
                            endvert = 1, endseg = 1, rivers=river_center)
  # Convert from degrees to km
  distances_nwis <- append(distances_nwis, upstream_distance_km-dist_m/1000)
}
nwis_riv$dist_downstream_km <- distances_nwis

# Determine distance from start of channel for wqp sites
distances_wqp <- list()
for (i in 1:length(wqp_riv$vert)){
  # Distance to mouth of river
  dist_m <- riverdistance(startseg = wqp_riv$seg[i], startvert = wqp_riv$vert[i],
                            endvert = 1, endseg = 1, rivers=river_center)
  # Convert from degrees to km
  distances_wqp <- append(distances_wqp, upstream_distance_km-dist_m/1000)
}
wqp_riv$dist_downstream_km <- distances_wqp

## Merge station info ##
mc_nwis$dist2river_m <- nwis_riv$snapdist
mc_nwis$dist_downstream_km <- nwis_riv$dist_downstream_km
mc_wqp$dist2river_m <- wqp_riv$snapdist
mc_wqp$dist_downstream_km <- wqp_riv$dist_downstream_km

nwis_stations <- rbind(mc_nwis, ut_nwis, fill=TRUE)
wqp_stations <- rbind(mc_wqp, ut_wqp, fill=TRUE)

## Map stationd distances to Q and SSC measurements ##
Q_full <- left_join(Q_full, nwis_stations)
```

    ## Joining, by = c("site_no", "portal", "channel")

``` r
SSC_full <- left_join(SSC_full, wqp_stations)
```

    ## Joining, by = c("site_no", "portal", "channel")

``` r
## Convert variable data tables to upper case ##
nwis_stations <- mutate_all(nwis_stations, toupper)
wqp_stations <- mutate_all(wqp_stations, toupper)
Q_full <- mutate_all(Q_full, toupper)
SSC_full <- mutate_all(SSC_full, toupper)

## Save variables to save time ##
save(riverdata, file=paste0(save_dir, 'riverdata.Rdata'))
save(nwis_stations, file=paste0(save_dir, 'nwis_stations.Rdata'))
save(wqp_stations, file=paste0(save_dir, 'wqp_stations.Rdata'))
save(SSC_full, file=paste0(save_dir, 'SSC_full.Rdata'))
save(Q_full, file=paste0(save_dir, 'Q_full.Rdata'))

## Export data to csv ##
write.csv(Q_full, paste0(wd_root,"/exports/Q_full.csv"), row.names=FALSE)
write.csv(SSC_full, paste0(wd_root,"/exports/SSC_full.csv"), row.names=FALSE)
write.csv(nwis_stations, paste0(wd_root, '/exports/Q_stations.csv'), row.names=FALSE)
write.csv(wqp_stations, paste0(wd_root, '/exports/SSC_stations.csv'), row.names=FALSE)
```

## Determine locations of all dams in the watershed

-   Dam data cane be queried automatically or downloaded in csv form
    from the NID Website \[<https://nid.usace.army.mil/#/downloads>\]
-   **Note:** If you download the nation.csv which contains all NID
    data, you must remove the first row which contains the last date the
    file was modified (R uses the first row as column names)

``` r
## Issue --> not all dams stored in NID database
## If importing from file ##
if (use_dam_file) {
  nid_dams <- read.csv(dam_file)
} else {
  ## CURRENTLY NOT WORKING
  nid_dams <- get_nid()
}

# Remove entries which are missing lat, lon
nid_dams <- na.omit(nid_dams, cols=c('Longitude','Latitude'))
# Convert to sf object
nid_dams_sf <- st_as_sf(nid_dams, coords = c("Longitude", "Latitude")) %>% st_set_crs(4326)

# Get dams in the basin
basin_dams <- st_intersection(nid_dams_sf,riverdata$basin$geometry)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

# Start Making Plots

## Compare Centerline KML and NLDI

``` r
centerline_compare <-
  ggplot() + 
    geom_sf(data = riverdata$basin) + 
    geom_sf(data = centerline_sf, col = "red", alpha = .5, size = 1)+
    theme_void() +
    labs(caption = "Input extent") +
  ggplot() +
    geom_sf(data = riverdata$basin, col = NA) + 
    geom_sf(data = riverdata$UM_flowlines, col = "blue",size = 1) +
    theme_void() +
    labs(title = "Centerlines",
         caption = "NLDI extent") + 
    theme(legend.position = "none",
           plot.title = element_text(face = "bold", hjust = .5))

centerline_compare
```

![](ssc_retrieval_files/figure-gfm/NLDI%20sanity%20check-1.png)<!-- -->

``` r
# Save Map
ggsave(centerline_compare, 
       filename = paste0(wd_root, '/exports/centerline_compare.png'),
       width = 5, height = 8)
```

## Plot all NWIS stations (time independent)

``` r
## Plot Station Data ##
nwis_station_plot <- ggplot() + 
  ## Basin and Channels ##
  geom_sf(data = riverdata$basin) + 
  geom_sf(data = riverdata$UM_flowlines, col = "blue") + 
  geom_sf(data = riverdata$UT_flowlines, col = "blue", alpha = .5, lwd=0.25) + 
  theme_bw() +
  coord_sf(label_graticule = 'NESW') +
  
  geom_sf(data = riverdata$UM_nwissite, shape = 21, 
          fill = NA, stroke = 0.75, size = 1.5, color="red") + # Shape aes: USGS gage, set with scale_shape_manual
  geom_sf(data = riverdata$UT_nwissite, shape = 21, 
          fill = NA, stroke = 0.25, size = 0.5, color="red") + # Shape aes: USGS gage, set with scale_shape_manual 
  scale_size_continuous(range = c(1,7)) +
  scale_color_brewer(palette = "Set1") +
  #theme_void() +
  labs(title = "USGS Station Locations") + 
  theme(legend.position = "none",
         plot.title = element_text(face = "bold", hjust = .5))
```

    ## Coordinate system already present. Adding new coordinate system, which will replace the existing one.

``` r
nwis_station_plot
```

![](ssc_retrieval_files/figure-gfm/NWIS%20stations-1.png)<!-- -->

``` r
# Save Map
ggsave(nwis_station_plot,
       filename = paste0(wd_root, '/exports/usgs_stations.png'),
       width = 5, height = 8)
```

## Plot locations of USGS stations which measure discharge (in time period)

``` r
## Plot Station Data ##
nwis_plot <- nwis_stations%>%st_as_sf(coords=c('lon','lat')) %>% st_set_crs(4326)

discharge_locs <- ggplot() + 
  ## Basin and Channels ##
  geom_sf(data = riverdata$basin, show.legend = F) + 
  geom_sf(data = riverdata$UM_flowlines, col = "blue", show.legend = F) + 
  geom_sf(data = riverdata$UT_flowlines, col = "blue", alpha = .5, lwd=0.25, show.legend = F) + 
  theme_bw() +
  scale_size_continuous(range = c(1,7)) +
  
  ## Upper main channel#
  geom_sf(data = nwis_plot, aes(color=channel, shape=channel, size=channel),  fill = NA, stroke = 0.75) + 
  #scale_size_continuous(range = c(1,7)) +
  #scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c('green', 'red')) +
  scale_shape_manual(values = c(21, 1)) +
  scale_size_manual(values = c(3, 1)) +
  #theme_void() +
  coord_sf(label_graticule = 'NESW') +
  labs(title = "USGS Station Locations Measuring Discharge",
       caption = paste0(startDate, ' to ', endDate))
```

    ## Scale for 'size' is already present. Adding another scale for 'size', which
    ## will replace the existing scale.

``` r
  #theme(legend.position = "none",
  #       plot.title = element_text(face = "bold", hjust = .5))

discharge_locs
```

![](ssc_retrieval_files/figure-gfm/discharge%20stations-1.png)<!-- -->

``` r
# Save Map
ggsave(discharge_locs, 
       filename = paste0(wd_root, '/exports/discharge_stations.png'),
       width = 5, height = 8)
```

## Plot locations of SSC sampling (in time period)

``` r
## Plot Station Data ##
wqp_plot<-wqp_stations%>%st_as_sf(coords=c('lon','lat')) %>% st_set_crs(4326)

ssc_locs <- ggplot() + 
  ## Basin and Channels ##
  geom_sf(data = riverdata$basin, show.legend = F) + 
  geom_sf(data = riverdata$UM_flowlines, col = "blue", show.legend = F) + 
  geom_sf(data = riverdata$UT_flowlines, col = "blue", alpha = .5, lwd=0.25, show.legend = F) + 
  theme_bw() +
  scale_size_continuous(range = c(1,7)) +
  
  ## Upper main channel#
  geom_sf(data = wqp_plot, aes(color=channel, shape=channel, size=channel),  fill = NA, stroke = 0.75) + 
  #scale_size_continuous(range = c(1,7)) +
  #scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c('green', 'red')) +
  scale_shape_manual(values = c(21, 1)) +
  scale_size_manual(values = c(3, 1)) +
  #theme_void() +
  coord_sf(label_graticule = 'NESW') +
  labs(title = "USGS Station Locations Measuring SSC",
       caption = paste0(startDate, ' to ', endDate))
```

    ## Scale for 'size' is already present. Adding another scale for 'size', which
    ## will replace the existing scale.

``` r
  #theme(legend.position = "none",
  #       plot.title = element_text(face = "bold", hjust = .5))
ssc_locs
```

![](ssc_retrieval_files/figure-gfm/ssc%20stations-1.png)<!-- -->

``` r
# Save Map
ggsave(ssc_locs, 
       filename = paste0(wd_root, '/exports/ssc_sites.png'),
       width = 5, height = 8)
```

## Plot dam locations and flowlines

``` r
dam_flowline_map <- ggplot() + 
  ## Basin and Channels ##
  geom_sf(data = riverdata$basin) + 
  geom_sf(data = riverdata$UM_flowlines, col = "blue") + 
  geom_sf(data = riverdata$UT_flowlines, col = "blue", alpha = .5, lwd=0.25) + 
  theme_bw() +
  coord_sf(label_graticule = 'NESW') +
  geom_sf(data = basin_dams, 
          # Set size by storage capacity
          aes(size = Volume..Cubic.Yards.),
          color = 'red',
          shape = 2,
          fill = NA, stroke = 1) +# No fill, stroke (same as lwd for lines) is equal to 1 
  scale_size_continuous(range = c(0.5,3)) +
  labs(title = "NID Dam Locations")
```

    ## Coordinate system already present. Adding new coordinate system, which will replace the existing one.

``` r
dam_flowline_map
```

![](ssc_retrieval_files/figure-gfm/dam%20locations-1.png)<!-- -->

``` r
# Save Map
ggsave(dam_flowline_map, 
       filename = paste0(wd_root, '/exports/dam_flowline_map.png'),
       width = 5, height = 8)
```

## Import landsata data and harmonize with USGS data

``` r
landsat_data <- data.table(read.csv(landsat_csv))[,':='(
  lat=Latitude,
  lon=Longitude
)][,.(SSC_mgL, month, year, distance_km, lat, lon)]

## Filter data to time of interest stamp ##
# Remember dates are in YYYY-MM-DD format
landsat_data_filter <- landsat_data %>% 
  # Filter by year
  filter(year<=as.numeric(substr(endDate, 1, 4))) %>%
  filter(year>=as.numeric(substr(startDate, 1, 4))) %>%
  # Filter by month for end years
  filter(!(year==as.numeric(substr(startDate, 1, 4)) & month<as.numeric(substr(startDate, 6, 7)))) %>%
  filter(!(year==as.numeric(substr(endDate, 1, 4)) & month>as.numeric(substr(endDate, 6, 7))))

### THEMES- From Evan Dethier ###
theme_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13)
  )

season_facet <- theme_facet + theme(
  #legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)

SSC_full$dist_downstream_km <- as.numeric(SSC_full$dist_downstream_km)
```

    ## Warning: NAs introduced by coercion

``` r
SSC_full$sscmgmL <- as.numeric(SSC_full$sscmgmL)

## Create Plots ##
ssc_landsat_plot <- ggplot(landsat_data_filter, aes(x = distance_km, y = SSC_mgL)) +
    stat_summary(geom = 'line', fun = 'mean') +
    season_facet +
    labs(title = 'SSC Concentration - Chattahoochee River',
         caption = paste0(startDate, ' to ', endDate),
         x = 'Distance Downstream (km)', 
         y = 'SSC (mg/L)')
ssc_landsat_plot
```

![](ssc_retrieval_files/figure-gfm/landsat%20import-1.png)<!-- -->

``` r
ssc_usgs_plot <- ggplot(data=drop_na(SSC_full), aes(x = dist_downstream_km, y = sscmgmL)) +
    geom_point() +
    stat_summary(
      geom = 'point',
      fun= 'mean',
      col = 'red',
      size = 3,
      shape = 24,
      fill = 'red') +
    season_facet +
    labs(title = 'SSC Concentration - Chattahoochee River',
         caption = paste0(startDate, ' to ', endDate),
         x = 'Distance Downstream (km)', 
         y = 'SSC (mg/L)')
ssc_usgs_plot
```

![](ssc_retrieval_files/figure-gfm/landsat%20import-2.png)<!-- -->

``` r
# Combine data into one dataframe for one plot
plot_data <- landsat_data_filter
usgs_dist <- drop_na(SSC_full)$dist_downstream_km
usgs_sscmgmL <- drop_na(SSC_full)$sscmgmL
plot_data <- qpcR:::cbind.na(plot_data, usgs_dist)
plot_data <- qpcR:::cbind.na(plot_data, usgs_sscmgmL)

ssc_combined <- 
    ggplot(plot_data, aes(x = distance_km, y = SSC_mgL)) +
      stat_summary(
        aes(x = distance_km, y = SSC_mgL, color='Landsat Derived Mean SSC'),
        geom = 'line', 
        fun = 'mean',
        inherit.aes = FALSE,
        show.legend=TRUE) +
      #geom_point(data = drop_na(plot_data), aes(x = usgs_dist, y = usgs_sscmgmL)) +
        stat_summary(
          data = plot_data,
          aes(x = usgs_dist, y = usgs_sscmgmL, color='1 standard Error'),
          geom = 'errorbar',
          fun.data= mean_se,
          fun.args = list(mult = 1), # mult = # of standard errors
          size = 0.5,
          width =  30,
          #color = 'green',
          inherit.aes = FALSE,
          show.legend=TRUE) +
        stat_summary(
          data = drop_na(plot_data),
          aes(x = usgs_dist, y = usgs_sscmgmL, color='USGS Derived Mean SSC'),
          geom = 'point',
          fun= 'mean',
          #size = 3,
          #shape=24,
          inherit.aes = FALSE,
          show.legend=TRUE,
          fill = 'purple') +
      season_facet +
      theme(
        legend.position = "bottom"
      ) +
      guides(color=guide_legend("Key")) +
      scale_color_manual(values = c("green", "red", "#5ab4ac")) +
      #scale_y_continuous(limits = c(0, 1000)) + # IGNORING SOME OUTLIERS
      labs(title = 'SSC Concentration - Chattahoochee River',
           caption = paste0(startDate, ' to ', endDate),
           x = 'Distance Downstream (km)', 
           y = 'SSC (mg/L)')

ssc_combined
```

    ## Warning: Removed 22930 rows containing non-finite values (stat_summary).

![](ssc_retrieval_files/figure-gfm/landsat%20import-3.png)<!-- -->

``` r
## Save combined plot ##
ggsave(ssc_combined, 
       filename = paste0(wd_root, '/exports/ssc_USGSxLandsat.png'),
       width = 8, height = 5)
```

    ## Warning: Removed 22930 rows containing non-finite values (stat_summary).
