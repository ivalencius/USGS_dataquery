# i. Set WD
#### SET DIRECTORIES ####

wd_root <- "~/hydrology_teaching"

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/dam-trapping-exercise/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_imports,"/dam-trapping-exercise-exports/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}

setwd(wd_imports)
# 1.Import and install packages
library(data.table)
library(ggplot2)
library(dataRetrieval)
library(lubridate)
library(sf)
library(readxl)
library(ggtext)

# For plotting labels:
fancy_scientific_modified <- function(l) { 
  # turn in to character string in scientific notation 
  if(abs(max(log10(l), na.rm = T) - min(log10(l), na.rm = T)) > 2 | 
     # min(l, na.rm = T) < 0.01 | 
     max(l, na.rm = T) > 1e5){ 
    l <- log10(l)
    label <- parse(text = paste("10^",as.character(l),sep = ""))
  }else{
    label <- parse(text = paste(as.character(l), sep = ""))
  }
  # print(label)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(label)
}

# 2. Import dam datasets
# Import dams for every USGS gage with SSC records
nid_dams_by_gage <- fread('/Users/evandethier/us-dam-trapping/usgs-dam-trapping-imports/NID_dams_by_USGS_station.csv',
                          colClasses = c('site_no' = 'character'))

# 3. Make vector of USGS station names
usgs_ssc_sites <- unique(nid_dams_by_gage$site_no)

# 4. Download SSC metadata for all USGS gages in dam dataset
for(i in 1:length(usgs_ssc_sites)){
  # Select site code
  site_no_sel <- usgs_ssc_sites[i]
  # Download metadata about the SSC code: '80154'
  nwis_ssc_data_sel <- data.table(whatNWISdata(siteNumber = site_no_sel, parameterCd = '80154'))
  # Select site code, station name, count number (how many SSC samples there are), start and end dates of sampling
  nwis_ssc_data_sel <- nwis_ssc_data_sel[,.(site_no, station_nm, data_type_cd, count_nu, begin_date, end_date)]
  # Write to a new data table
  if(i == 1){
    nwis_ssc_data <- nwis_ssc_data_sel
  }else{
    nwis_ssc_data <- rbind(nwis_ssc_data, nwis_ssc_data_sel)
  }
}

# 5. Show San Juan Archuleta rows
# Answer: 3,936 daily observations; 170 water quality observations

nwis_ssc_data[site_no == '09355500']

# 6. RCI > 5 dams
rci5_dams <- nid_dams_by_gage[RCI > 5]

# 7. Summarize SSC site metadata table
# Get total number of SSC measurements and monitoring record span for each site
nwis_ssc_data_summary <- nwis_ssc_data[, .(begin_date = min(begin_date, na.rm = T),
                                           end_date = max(end_date, na.rm = T),
                                           count_nu = sum(count_nu, na.rm = T)),
                                       by = site_no][
                                         ,':='(record_yrs = as.numeric(end_date - begin_date)/365)
                                       ]

# 8. Join dam and SSC site metadata tables
rci5_dams <- merge(rci5_dams, nwis_ssc_data_summary, by = 'site_no')

# 9. Subset to only include SSC records spanning dam emplacement
dam_spanning_sampling <- rci5_dams[year(begin_date) < YEAR_COMPLETED & year(end_date) > YEAR_COMPLETED]

# 10. Select station
site_no_sel <- '09355500'

# 11. Download watershed and flowlines
basin_sel <- findNLDI(nwis = site_no_sel, 
                      # 'nwis': USGS gages; 'basin': watershed boundary; 'flowlines': flowlines
                      find = c("nwis", "basin", "flowlines"),
                      nav = c('UM', 'UT'), # UM: upstream main stem; UT: upstream tributaries
                      distance_km = 1500) # How far upstream to look (in km)

plot(basin_sel$basin)

# 12. Make a map of the basin with flowlines and gage stations
watershed_sel_dams_flowlines_map <- ggplot() + 
  geom_sf(data = basin_sel$basin) + # Just regular black outline
  geom_sf(data = basin_sel$UM_flowlines, 
          color = 'blue', show.legend = F) + # Blue but not in legend. line width is default of 1
  geom_sf(data = basin_sel$UT_flowlines, 
          color = 'blue', alpha = 0.7, lwd = 0.25, show.legend = F) + # Blue but not in legend. thinner lines (lwd = 0.7)
  geom_sf(data = basin_sel$UM_nwissite, aes(color = 'USGS gage', shape = 'USGS gage'), 
          fill = NA, stroke = 1, size = 1.5) + # Shape aes: USGS gage, set with scale_shape_manual
  theme_bw() +
  scale_size_continuous(range = c(1,7)) + # Size set by aes in dams geom. Range is 1-7 (7 is pretty big)
  # Set color for all geoms, but only those without show.legend = F show up in the legend
  scale_color_manual(values = c('Main stem' = 'blue', 'Tributaries' = 'blue', 'USGS gage' = 'red', 'NID dam' = 'orange')) +
  # Set shape for gage and dam (geom_point geoms)
  scale_shape_manual(values = c('USGS gage' = 21, 'NID dam' = 22)) +
  # Labels: set color and shape both = 'Legend' combines color and shape into one legend header
  labs(
    title = casefold(basin_sel$origin$name),
    size = 'Reservoir capacity\n(MAf)',
    shape = 'Legend',
    color = 'Legend'
  )
  
# Save map
ggsave(watershed_sel_dams_flowlines_map, 
       filename = paste0(wd_exports, paste0('USGS_', site_no_sel, '_basin_dams_flowlines_map.png')),
       width = 8, height = 5)

# 13. All dams from National Inventory of Dams (NID)
# Import raw dam data
all_dams_import <- data.table(read_xlsx('/Users/evandethier/usgs-erosion/usgs-erosion-imports/NID2019_U.xlsx'))

# Clean dam data TO DO: ACTUALLY CLEAN DATA
all_dams_clean <- na.omit(all_dams_import[DAM_NAME != "SOO LOCKS" & NID_STORAGE != 8519000],
                          cols = c('LONGITUDE', 'LATITUDE'))

# 14. Set projection and convert the dam dataset to an sf object
proj_sel <- "+proj=longlat +datum=WGS84 +no_defs"
all_dams_sf <- st_as_sf(all_dams_clean, coords = c("LONGITUDE", "LATITUDE"), 
                        crs = proj_sel)

# 15. Find dams within the watershed
basin_sel_dams <- st_intersection(all_dams_sf,basin_sel$basin$geometry)

# 16. Add dams to the watershed map
watershed_sel_dams_flowlines_map <- watershed_sel_dams_flowlines_map +
  geom_sf(data = basin_sel_dams, 
          # Set size by storage capacity, shape and color are in aes so they show up in the legend
          aes(size = NID_STORAGE, color = 'NID dam', shape = 'NID dam'), 
          fill = NA, stroke = 1) # No fill, stroke (same as lwd for lines) is equal to 1

# 17. Download daily observations of:
# suspended sediment concentration and discharge data for selected site
site_data <- data.table(readNWISdv(siteNumbers = site_no_sel, 
                                   parameterCd = c('00060', '80154')))


# 18 Convert discharge in cfs to cms and rename columns
# Add Day of year, discharge (in cms), ssc (correct name), and sample year cols
site_data <- site_data[,':='(Q_cfs = X_00060_00003,
                             SSC_mgL = X_80154_00003)][
                               ,.(site_no, Date, Q_cfs, SSC_mgL)
                             ]

#### WATER QUALITY DOWNLOAD ####  
# 19. Download water quality measurements of suspended sediment concentration and discharge data
# These measurements aren't made daily, and are stored in a different USGS portal
# Daily discharge: '00060', Instantaneous discharge: '00061', gage height (ft): '00065', SSC: '80154'

site_data_discrete_import <- data.table(readWQPqw(
                                  siteNumbers = paste0('USGS-', site_no_sel), 
                                                  parameterCd = c('00060','00061', '80154')))

# 20. Select only the relevant columns from the huge dataset (there's lots of metadata)
site_data_discrete <- site_data_discrete_import[,.(ActivityStartDate, ResultMeasureValue, USGSPCode)]

# 21. Make a small data.table of parameter codes so we can convert the codes to sensible names
pCode_name <- data.table(USGSPCode = c('00060', '00061', '00065','80154'),
                         parameter = c('Q_cfs_daily','Q_cfs_inst','Stage_ft', 'SSC_mgL'))

# 22. Then merge with the parameter code lookup table
site_data_discrete <- merge(site_data_discrete, pCode_name, by = 'USGSPCode')

# 23. Change the date column name
site_data_discrete <- site_data_discrete[,':='(Date = ActivityStartDate)]

# 24. Cast the data into wide structure
site_data_discrete <- dcast.data.table(Date ~ parameter, value.var = 'ResultMeasureValue', 
                                       fun.aggregate = mean, data = site_data_discrete)

# 25. Get one value for discharge
site_data_discrete <- site_data_discrete[,':='(Q_cfs = ifelse(!is.nan(Q_cfs_inst), Q_cfs_inst, Q_cfs_daily))]

# 26. Select only Date, discharge, and SSC colums
site_data_discrete <- site_data_discrete[,.(Date, Q_cfs, SSC_mgL)]

# 27. Bind (by row, using names) daily and discrete data
site_data_all <- rbind(site_data[,.(Date, Q_cfs, SSC_mgL)], site_data_discrete, use.names = T)

# 28. Add new columns
site_data_all <- site_data_all[
  ,':='(doy = yday(Date),
        month = month(Date),
        year = year(Date),
        decade = year(Date) - year(Date)%%10,
        Q_cms = Q_cfs * 0.02831,
        Qss_Mt_yr = Q_cfs * 0.02831 * SSC_mgL * 3.10585 * 10^-5)][
        ,.(Date, doy, Q_cms, SSC_mgL, Qss_Mt_yr, year, month, decade)]

# 29.
# Dam data for the site (all dams, years, and names of dams)
dams_sel <- dam_spanning_sampling[site_no == site_no_sel]
dams_sel_years <- unique(dams_sel$YEAR_COMPLETED)
dams_sel_names <- unique(dams_sel, by = 'YEAR_COMPLETED')$DAM_NAME

# Break data by dam period
site_data_all$Period <- cut(site_data_all$year, 
                            breaks = c(0, dams_sel_years, Inf), 
                            labels = c('Pre-dam', dams_sel_names))

station_nm_sel <- nwis_ssc_data[site_no == site_no_sel][1]$station_nm

# 31. Discharge vs. SSL
discharge_vs_ssl <- ggplot(site_data_all[!is.na(Qss_Mt_yr) & Q_cms > 0 & Qss_Mt_yr > 0]) +
  geom_point(aes(x = Q_cms, y = Qss_Mt_yr, color = Period)) + # Points colored by dam period
  geom_smooth(method = 'lm', aes(x = Q_cms, y = Qss_Mt_yr, color = Period), # Linear fits by period
              se = F) + # Don’t include std. error
  theme_classic() +
  scale_x_log10(labels = fancy_scientific_modified) + # labels are nice with fancy_scientific_modified
  scale_y_log10(labels = fancy_scientific_modified) + # labels are nice with fancy_scientific_modified
  labs(
    x = 'Discharge (m<sup>3</sup>/s)', # This syntax requires the ggtext package
    y = 'Suspended sediment load (tons/yr)',
    color = 'Period',
    title = casefold(station_nm_sel),
    subtitle = 'Changing Q-Qss relationship'
  ) +
  theme(axis.title.x = element_markdown()) # Allows the superscript for x label

# Save map
ggsave(discharge_vs_ssl, 
       filename = paste0(wd_exports, paste0('USGS_', site_no_sel, '_Q_Qss_rating_curve_by_dam.png')),
       width = 8, height = 5)

# Predict SSL
# 32. Clean data for regression model input
clean_model_site_data <- site_data_all[!is.na(Qss_Mt_yr) & Q_cms > 0 & Qss_Mt_yr > 0]


# 33. Create rating curve model (by Pre- and Post-dam periods)
discharge_vs_ssl_lm <- lm(log10(Qss_Mt_yr) ~ log10(Q_cms) + Period, # Model form
                          data = clean_model_site_data) # Cleaned data for model

# 34. Make Qss estimates for days even without SSC measurements
clean_predict_site_data <- site_data_all[Q_cms > 0]
discharge_vs_ssl_predict <- predict(discharge_vs_ssl_lm, # Rating-curve model
                                    newdata = clean_predict_site_data) # Cleaned new data for model
# Add predicted data as new column to clean data
clean_predict_site_data$Qss_Mt_yr_model <- 10^discharge_vs_ssl_predict


Qss_by_period <- clean_predict_site_data[,.(Qss_Mt_yr_model = mean(Qss_Mt_yr_model, na.rm = T),
                                            Qss_Mt_yr_obs = mean(Qss_Mt_yr, na.rm = T)),
                                         by = .(Period)]

# 35. Summarize
# (Answers will vary based on site selected)
dam_trapping_efficiency <- (Qss_by_period$Qss_Mt_yr_model[1] - Qss_by_period$Qss_Mt_yr_model[2])/
                            Qss_by_period$Qss_Mt_yr_model[1]

# 36. Millions of tons trapped behind dam
# Millions of tons per year:
# Based on rating curve = 1.05 Mt/yr * 60 yrs = 63 Million tons
dam_Mt_trapped_per_yr <- Qss_by_period$Qss_Mt_yr_model[1] - Qss_by_period$Qss_Mt_yr_model[2]
dammed_years <- 2022 - dams_sel_years
# Based only on observations = 2.02 Mt/yr * 60 yrs = 121 Million tons
dam_Mt_obs_trapped_per_yr <- Qss_by_period$Qss_Mt_yr_obs[1] - Qss_by_period$Qss_Mt_yr_obs[2]

# 37. Dam trapping efficiency
# My answer for USGS site: 09355500:
# DTE = 0.976

dam_trapping_efficiency_obs <- (Qss_by_period$Qss_Mt_yr_obs[1] - Qss_by_period$Qss_Mt_yr_obs[2])/
                            Qss_by_period$Qss_Mt_yr_obs[1]

# Using observations only for USGS site 09355500:
# DTE = 0.987
