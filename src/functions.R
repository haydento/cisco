#library(data.table)
#' @title compile directory of vdat vrl files into single object
#' @description compiles detection data from vdat files extracted from vrl files.  Vdat command line program is used to create vdat files for each vrl.  Function compiles data from each vrl and changes column names to something sane.
#' @param pth path to directory that contains .csv files extracted from .vrl files with vdat software

#' @return returns a data.table
#' 
#' @examples
#' dtc <- compile_det(pth = "~/Documents/Lake_Huron_cisco/data/receivers/vrl_to_csv")

compile_det <- function(pth){
  fls <- pth
#  fls <- list.files(pth, pattern = "*.csv", full.names = TRUE)
  name_col <- names(fread(cmd = paste("grep -iw DET_DESC", fls[1])))
  int_trans <- paste("grep -iw", "DET", fls)
  trans <- lapply(int_trans, function(x) fread(cmd = x))
  trans <- rbindlist(trans)
  setnames(trans, paste0("V", 1:length(name_col)), name_col)
  set(trans, j = "Time", value = fasttime::fastPOSIXct(trans$Time, tz = "UTC"))
  trans <- trans[, c("Time", "Model", "Serial Number", "Full ID", "ID", "Signal Strength (dB)", "Noise (dB)", "Quality Score", "Decoder", "Sensor Value", "Sensor Unit")]
  setnames(trans, names(trans), c("detection_timestamp_utc", "rec_model", "rec_serial_no", "full_transmitter_id", "transmitter_id", "signal_strength", "noise", "quality_score", "decoder", "sensor_value", "sensor_unit"))
  return(trans)
}

#' @title cleans receiver deployment/recovery data
#' @description Extracts receiver deployment and recovery information from project spreadsheet and fixes column names and creates time columns.  Also adds current day/time information as receiver recovery if missing (i.e., receiver hasn't been recovered yet)
#' @param pth path to project spreadsheet that contains a "recs" sheet 
#' @return returns a data.table of cleaned data
#' 
#' @examples
#' recs <- clean_recs(pth = "~/Documents/Lake_Huron_cisco/dbase_submission/cisco_dbase.ods")


clean_recs <- function(pth){
  recs <- readODS::read_ods(pth, sheet = "recs", col_types = readr::cols(glatos_deploy_date_time = readr::col_character(), recover_date_time = readr::col_character()))
  setDT(recs)
  set(recs, j = "GLATOS_DEPLOY_DATE_TIME", value = as.POSIXct(recs$glatos_deploy_date_time, tz = "America/Detroit"))
  set(recs, j = "GLATOS_RECOVER_DATE_TIME", value = as.POSIXct(recs$RECOVER_DATE_TIME, tz = "America/Detroit"))
  set(recs, j = "Serial Number", value = as.numeric(recs$`Serial Number`))
  #recs <- recs[!is.na(RECOVER_DATE_TIME),]
  recs <- recs[, c("GLATOS_ARRAY", "STATION_NO", "SITE", "GLATOS_DEPLOY_DATE_TIME", "GLATOS_RECOVER_DATE_TIME", "DEPLOY_LAT", "DEPLOY_LONG", "RECEIVER", "INS_SERIAL_NO", "BOTTOM_DEPTH")]
  setnames(recs, names(recs), c("glatos_array", "station_no", "site", "glatos_deploy_date_time", "glatos_recover_date_time", "deploy_lat", "deploy_long", "receiver", "receiver_serial_no", "bottom_depth_m"))
  # next line is needed because I have not entered any recovery times from summer 2021 swaps yet.
  # Should not make much difference because likely won't have any detections of fish after receivers were recovered
  # the exception is if a fish was within detection range of boat after receiver was recovered and detected through the boat.
  recs[is.na(glatos_recover_date_time), glatos_recover_date_time := Sys.time()]
  recs[, glatos_array2 := glatos_array]
  recs[glatos_array == "OUT",  glatos_array2 := paste(glatos_array, station_no, sep = "_")]
  recs[glatos_array == "OFF",  glatos_array2 := paste(glatos_array, station_no, sep = "_")]

  return(recs)
  
}


#' @title cleans tagging data
#' @description Extracts tagging information from project spreadsheet, fixes column names, and sets  time columns.
#' @param pth path to project spreadsheet that contains a "tagging" sheet 
#' @return returns a data.table of cleaned data
#' 
#' @examples
#' tags <- clean_tags(pth = "~/Documents/Lake_Huron_cisco/dbase_submission/cisco_dbase.ods")


clean_tags <- function(pth = "~/Documents/Lake_Huron_cisco/dbase_submission/cisco_dbase.ods"){
  tags <- readODS::read_ods(pth, sheet = "tagging", col_types = readr::cols(RELEASE_DATE_TIME = readr::col_character(), DATE_OF_SURGERY = readr::col_character()))
  setDT(tags)
  set(tags, j = "RELEASE_DATE_TIME", value = as.POSIXct(tags$RELEASE_DATE_TIME, tz = "America/Detroit"))
  set(tags, j = "DATE_OF_SURGERY", value = as.Date(tags$DATE_OF_SURGERY))
  return(tags)
}



#' @title combine receiver location and tag information with detection information
#' @description uses data.table joins to combine receiver deployment and tagging information with detections.  This is similar to process used by glatos to create export data.
#' @param det data.table containing cleaned and prepped detections
#' @param recs data.table containing cleaned and prepped receiver deployment information
#' @param tagging data.table containing cleaned and prepped tagging information
#' @return returns a data.table of detection data that contains receiver deployment information (lat/lon) and fish biological information
#' @examples
#' 
#'

#' tar_load(compile_dtc)
#' tar_load(prep_recs)
#' tar_load(prep_tags)

#' add_locs_tags(det = compile_dtc, recs = prep_recs, tagging = prep_tags)


add_locs_tags <- function(det, recs, tagging){
  rec <- copy(recs)
  dtc <- copy(det)
  tg <- copy(tagging)

  foo <- rec[dtc, .(detection_timestamp_utc = i.detection_timestamp_utc,
                    rec_model = i.rec_model,
                    rec_serial_no = i.rec_serial_no,
                    full_transmitter_id = i.full_transmitter_id,
                    transmitter_id = i.transmitter_id,
                    signal_strength = i.signal_strength,
                    noise = i.noise,
                    quality_score = i.quality_score,
                    decoder = i.decoder,
                    sensor_value = i.sensor_value,
                    sensor_unit = i.sensor_unit,
                    glatos_array = x.glatos_array,
                    station_no = x.station_no,
                    site = x.site,
                    deploy_lat = x.deploy_lat,
                    deploy_long = x.deploy_long,
                    receiver = x.receiver,
                    receiver_serial_no = x.receiver_serial_no,
                    bottom_depth_m = x.bottom_depth_m),
             on = .(receiver_serial_no = rec_serial_no, glatos_deploy_date_time <= detection_timestamp_utc, glatos_recover_date_time >= detection_timestamp_utc), nomatch = NULL]

  out <- tg[foo, .(animal_id = x.ANIMAL_ID,
                   tag_model = x.TAG_MODEL,
                   tag_serial_no = x.TAG_SERIAL_NUMBER,
                   tag_id_code = x.TAG_ID_CODE,
                   full_transmitter_id = x.TRANSMITTER,
                   est_tag_life = x.EST_TAG_LIFE,
                   capture_location = x.CAPTURE_LOCATION,
                   capture_latitude = x.CAPTURE_LATITUDE,
                   capture_longitude = x.CAPTURE_LONGITUDE,
                   length = x.LENGTH,
                   sex = x.SEX,
                   release_location = x.RELEASE_LOCATION,
                   release_latitude = x.RELEASE_LATITUDE,
                   release_longitude = x.RELEASE_LONGITUDE,
                   release_date_time = x.RELEASE_DATE_TIME,
                   surgery_location = x.SURGERY_LOCATION,
                   date_of_surgery = x.DATE_OF_SURGERY,
                   surgery_latitude = x.SURGERY_LATITUDE,
                   surgery_longitude = x.SURGERY_LONGITUDE,
                   min_delay = x.MIN_DELAY,
                   max_delay = x.MAX_DELAY,
                   tag_power = x.TAG_POWER,
                   num_tag_id = x.NUM_TAG_ID,
                   sensor_type = x.SENSOR_TYPE,
                   sensor_units = x.UNITS,
                   sensor_slope = x.SLOPE,
                   sensor_intercept = x.INTERCEPT,
                   detection_timestamp_utc = i.detection_timestamp_utc,
                   rec_model = i.rec_model,
                   rec_serial_no = i.rec_serial_no,
                   full_transmitter_id = i.full_transmitter_id,
                   transmitter_id = i.transmitter_id,
                   signal_strength = i.signal_strength,
                   noise = i.noise,
                   quality_score = i.quality_score,
                   decoder = i.decoder,
                   sensor_value = i.sensor_value,
                   sensor_unit = i.sensor_unit,
                   glatos_array = i.glatos_array,
                   station_no = i.station_no,
                   site = i.site,
                   deploy_lat = i.deploy_lat,
                   deploy_long = i.deploy_long,
                   receiver = i.receiver,
                   receiver_serial_no = i.receiver_serial_no,
                   bottom_depth_m = i.bottom_depth_m),
            on = .(TRANSMITTER = full_transmitter_id), nomatch = NULL]

  out[is.na(sensor_value), c("sensor_type", "sensor_units", "sensor_slope", "sensor_intercept") := list(NA, NA, NA, NA)]
  out[, sensor_units := sub('\u00b0', "", out$sensor_units)]
  out[, sensor_real_value := (sensor_slope * sensor_value) + sensor_intercept] 
  
  return(out)
}


##########

#' @title Plot preliminary plots of depth sensor data
#' @param z data.table for plotting-detections
#' @param recs data.table of all recs
#' @param out_pth pth to save output plots
#' @examples
#' tar_load(clean_dtc)
#' z <- clean_dtc
#' out_pth = "output/pressure_temp.pdf"
#' tar_load(prep_recs)
#' recs <- prep_recs
#' depth_fig(z = z)

depth_fig <- function(z, recs, out_pth = "output/pressure_temp.pdf"){

  setkey(z, animal_id, detection_timestamp_utc)
  recs <- recs[, glatos_array2_f := as.factor(glatos_array2)]
  z[, glatos_array2_f := as.factor(glatos_array2)]

  x <- z[sensor_type == "P",]
  y <- z[sensor_type == "T",]

  fsh <- z[!is.na(sensor_units)]
  dtc_rng <- range(fsh$detection_timestamp_utc)
  rng_start <- round(dtc_rng[1], "month")
  rng_end <- round(dtc_rng[2], "month")
  
  fsh <- unique(fsh$animal_id)

  pdf(out_pth)
  par(mfrow = c(3,1))
  par(cex = 0.6)
  par(mar = c(4,4,4,0), oma = c(1,1,1,1))
  
  yr <- seq(as.POSIXct("2019-01-01", tz = "UTC"), as.POSIXct("2021-08-01", tz = "UTC"), by = "year")
  mon <- seq(as.POSIXct("2019-01-01", tz = "UTC"), as.POSIXct("2021-08-01", tz = "UTC"), by = "month")
  depth <- seq(from = 0, to = 100, by = 10)
  temp <- seq(from = 0, to = 30, by = 10)

  #for(i in 1:10){
  for(i in 1:length(fsh)){
    z.i <- z[animal_id == fsh[i],]
    x.i <- x[animal_id == fsh[i],]
    y.i <- y[animal_id == fsh[i],]
    
    smoother <- loess(x.i$sensor_real_value ~ as.numeric(x.i$detection_timestamp_utc), span = 0.25)
    tst <- data.table(detection_timestamp_utc = x.i$detection_timestamp_utc, smooth = predict(smoother))
        
    plot(x[animal_id == fsh[i],]$detection_timestamp_utc, x[animal_id == fsh[i],]$sensor_real_value, type = "l", axes = FALSE, ylim = c(50,0), ylab = "depth, m", xlab = "time", xlim = as.numeric(c(rng_start, rng_end)), main = x.i$animal_id[1])
    axis.POSIXct(1, at = yr, labels = TRUE, format = "%Y")
    axis.POSIXct(1, at = mon, labels = TRUE, format = "%m")
    axis(2, at = depth, las = 1)
    lines(tst$detection_timestamp_utc, tst$smooth, col = "red", lwd = 2)
    box()

     
     plot(y[animal_id == fsh[i],]$detection_timestamp_utc, y[animal_id == fsh[i],]$sensor_real_value, type = "l", axes = FALSE, ylim = c(0,30), ylab = "temp, C", xlab = "time", xlim = as.numeric(c(rng_start, rng_end)), main = NA)
     axis.POSIXct(1, at = yr, labels = TRUE, format = "%Y")
     axis.POSIXct(1, at = mon, labels = TRUE, format = "%m")
     axis(2, at = temp, las = 1)
     box()

    plot(z.i$detection_timestamp_utc, z.i$glatos_array2_f, axes = FALSE, ylab = NA, xlab = NA, pch = 16, xlim = as.numeric(c(rng_start, rng_end)))
    lines(z.i$detection_timestamp_utc, z.i$glatos_array2_f)
    axis.POSIXct(1, at = yr, labels = TRUE, format = "%Y")
    axis.POSIXct(1, at = mon, labels = TRUE, format = "%m")
    axis(2, at = seq(1,length(levels(z.i$glatos_array2_f)),1), labels = levels(z.i$glatos_array2_f),  las = 1)
    box()
  }

  dev.off()
  return(out_pth)  
}


#' @title revised min_lag for sensor tags
#' @description Similar to glatos::min_lag function but allows user to choose either tag serial number or transmitter_id for filter.
#' @param det A 'glatos_detections' object or a data frame containing detection data with "detection_timestamp_utc", "transmitter_codespace", "transmitter_id", and "receiver_sn" columns.
#' @param tag_serial_number Boolean.  If TRUE  then use transmitter_serial_number for filtering.
#' @examples
#' tar_load(dtc)
#' min_lag2(det = dtc, tag_serial_number = TRUE)


min_lag2 <- function(det, tag_serial_number = FALSE){
  dtc <- data.table::as.data.table(det)
  dtc[, `:=`(ord, 1:.N)]

  if(tag_serial_number == FALSE){
    data.table::setkey(dtc, transmitter_codespace, transmitter_id, 
                       receiver_sn, detection_timestamp_utc)
    dtc[, `:=`(min_lag, pmin(diff(c(NA, as.numeric(detection_timestamp_utc))), 
                             diff(c(as.numeric(detection_timestamp_utc), NA)), na.rm = TRUE)), 
        by = c("transmitter_codespace", "transmitter_id", "receiver_sn")][]
  }
  if(tag_serial_number == TRUE){
    data.table::setkey(dtc, tag_serial_no, rec_serial_no, detection_timestamp_utc)
    dtc[, `:=`(min_lag, pmin(diff(c(NA, as.numeric(detection_timestamp_utc))), 
                             diff(c(as.numeric(detection_timestamp_utc), NA)), na.rm = TRUE)), 
        by = c("tag_serial_no", "rec_serial_no")][]
  }
return(dtc)
}


#' @title identify potential false detections
#' @description adds column to identify potential false detections
#' @param x detection object
#' @param time_interval filter time interval
#' @param out_pth if show_plot = TRUE, output path written

#' @examples
#' tar_load(find_min_lag)
#' x <- find_min_lag
#' time_interval = 240*30
#' id_false(x = find_min_lag, time_interval = 240 * 30, out_pth = "output/false_detections.pdf")


id_false = function(x, time_interval, out_pth){

  pdf(out_pth)
  out <- false_detections(det = x, tf = time_interval, show_plot = TRUE)
  dev.off()
  out <- out[.(1), on = "passed_filter"]
  out[, glatos_array2 := glatos_array]
  out[glatos_array == "OUT",  glatos_array2 := paste(glatos_array, station_no, sep = "_")]
  out[glatos_array == "OFF",  glatos_array2 := paste(glatos_array, station_no, sep = "_")]
  return(out)
}


# start of function that may be useful for filtering dead fish.
## tar_load(clean_dtc)  

## rng <- range(clean_dtc$detection_timestamp_utc)
## rng_start <- round(rng[1], "month")
## rng_end <- round(rng[2], "month")
  
## tseq <- seq(rng_start, rng_end, by = "1 week")

## clean_dtc[, bin := tseq[findInterval(detection_timestamp_utc, tseq)]]
## foo <- clean_dtc[, uniqueN(site), by = .(bin, animal_id)]
## setkey(foo, bin)


#' @title identify possible dead fish using manual classification (based on detection history and depth)
#' @description labels detections from fish that died soon after release near a receiver
#' @param y detection table (data.table)
#' @examples
#' tar_load(clean_dtc)
#' y <- clean_dtc
#' manual_dead_id(clean_dtc

manual_dead_id <- function(y) {
  x <- data.table(animal_id = c("G_14943","G_14957", "G_14981", "G_15007", "G_8394", "G_8398", "G_8400", "G_8609", "G_8617", "G_8619", "G_8621", "G_8631", "G_8633", "G_8635", "G_8637", "G_8643", "G_8645", "G_8647", "G_8649", "G_8655", "G_8659", "G_8661", "G_8663", "G_8665", "G_8669", "G_8673", "G_8675", "G_8681", "G_8683", "G_8691", "G_8695", "G_8697", "G_8699", "G_8707", "G_8709", "G_8713", "G_8717", "G_8767", "H_15135", "H_8723", "H_8743", "H_8783"), status = rep("dead", 42))

  z <- copy(y)
  foo <- x[z, on = .(animal_id)]
  foo[is.na(status), status := "alive"]
  return(foo)
}

#' @title calculates mean depth/temperature for tagged cisco
#' @description calculates mean depth/temperature for each time bin.
#' @param x data.table of detection information
#' @param param switch- select temperature or pressure
#' @param t_interval set interval for bins used to calculate temp or pressure
#' @examples
#' tar_load(dead_id)
#' x <- dead_id
#' param <- "pressure"
#' t_interval = "1 day"
#' daily_sensor(x = x, param = "pressure", t_interval = "1 day")

daily_sensor <- function(x, param = "pressure", t_interval = "1 day") {
  y <- copy(x)
  if(param == "pressure"){
    y <- y[status == "alive" & tag_model == "V9TP-2x-069k-1" & sensor_type == "P",]
  }
  if(param == "temperature"){
    y <- y[status == "alive" & tag_model == "V9TP-2x-069k-1" & sensor_type == "T",]
  }

  rng <- range(y$detection_timestamp_utc)
  rng_start <- round(rng[1], "month")
  rng_end <- round(rng[2], "month")
  
  tseq <- seq(rng_start, rng_end, by = t_interval)

  y[, bin := tseq[findInterval(detection_timestamp_utc, tseq)]]
  out <- y[, .(mean_daily_depth = mean(sensor_real_value), num_daily_dtc = .N, mean_daily_bottom_depth_m = mean(bottom_depth_m, na.rm = TRUE)), by = .(bin, animal_id)]
 
  out[, month := as.numeric(format(bin, "%m"))][] 
  return(out)
}

#' @title plots depth information
#' @description plots depth preferences by cisco tagged with V9TP tags.
#' @param x input data.table containing daily summary data
#' @param out_pth output path for saving file
#' @param seasonal boolean; seasonal output (TRUE = 3 month intervals, FALSE = monthly intervals) 
#' @examples
#' tar_load(daily_mean_depth)
#' x <- daily_mean_depth
#' seasonal = TRUE
#' out_pth <- "output/seasonal.png"
#' cisco_depth_plot(x, out_pth, seasonal = TRUE)
cisco_depth_plot <- function(x, out_pth, seasonal = TRUE, ylim = c(30,0), ylab = "depth (m)", bottom_depth = FALSE){
  
  x[month %in% c(6,7,8), season := "summer"]
  x[month %in% c(9,10,11), season := "autumn"]
  x[month %in% c(12,1,2), season := "winter"]
  x[month %in% c(3,4,5), season := "spring"]

  x[, season_f := factor(season, levels = c("spring", "summer", "autumn", "winter"))]
  x[, month_f := factor(month, levels = as.character(1:12))]

  if(seasonal){
  png(out_pth)
  boxplot(mean_daily_depth ~ season_f, data = x, ylim = ylim, ylab = ylab, las = 1, notch = FALSE, pch = 16, col = "lightgrey", xlab = "season")
  dev.off()
  } else {
    
    png(out_pth)
    boxplot(mean_daily_depth ~ month_f, data = x, ylim = ylim, ylab = ylab, las = 1, pch = 16, col = "lightgrey", xlab = "month")
    dev.off()
  }    
  
  return(out_pth)

}

#' @title water and fish depth grouped boxplot
#' @description plots fish depth and water depth boxplots by month.
#' @param x detection object
#' @param pth output path for saving file
#' @examples
#' tar_load("dead_id")
#' x <- dead_id
#' pth <- "output/grouped_boxplot_depth.png"
#' grouped_boxplot_depth(x = dead_id, pth = "output/grouped_boxplot_depth.png")

grouped_boxplot_depth <- function(x = dead_id, pth){
  #foo <- detection_events(x, location_col = "glatos_array2", time_sep = Inf, condense = FALSE)
  #foo <- foo[arrive == 1,]
  x[, month := as.numeric(format(detection_timestamp_utc, "%m"))]
  x[, month_f := factor(month, levels = as.character(1:12))]
  x <- x[!is.na(sensor_real_value), c("month", "month_f", "sensor_real_value", "sensor_type", "bottom_depth_m")]
  x <- melt(x, id.vars = c("month", "month_f", "sensor_type"), measure.vars = c("sensor_real_value", "bottom_depth_m"))
  bar <- x[sensor_type == "P",]

  bar[, id := paste(month, variable, sep = "_")]
  bar[, id_f := factor(id, levels = c("1_sensor_real_value", "1_bottom_depth_m", "2_sensor_real_value", "2_bottom_depth_m", "3_sensor_real_value", "3_bottom_depth_m", "4_sensor_real_value", "4_bottom_depth_m", "5_sensor_real_value", "5_bottom_depth_m", "6_sensor_real_value", "6_bottom_depth_m", "7_sensor_real_value", "7_bottom_depth_m", "8_sensor_real_value", "8_bottom_depth_m", "9_sensor_real_value", "9_bottom_depth_m", "10_sensor_real_value", "10_bottom_depth_m", "11_sensor_real_value", "11_bottom_depth_m", "12_sensor_real_value", "12_bottom_depth_m"))] 


  cols <- rainbow(2, s=0.5)
  nam <- rep(c("J","F","M","A","M","J","J","A","S","O","N","D"), each = 2)

  png(filename = pth)
  boxplot(value ~ id_f, data = bar, ylim = c(70,0), col = cols, pch = 16, xlab = "month", ylab = "depth (m)", xaxs = FALSE, las =1, names = nam, at = c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17, 19:20, 22:23, 25:26, 28:29, 31:32, 34:35), outline = FALSE)
  legend("bottomright", fill = cols, legend = c("fish depth", "water depth"))
  dev.off()
  return(pth)
}


  #####################
#' @title temperature boxplot by month
#' @description plots fish experienced temperature by month.
#' @param x detection object
#' @param pth output path for saving file
#' @examples
#' tar_load("dead_id")
#' pth <- "output/fish_experienced_temperature.png"
#' x <- dead_id
#' temperature_boxplot(x = dead_id, pth = "output/fish_experienced_temperature.png")

temperature_boxplot <- function(x = dead_id, pth){

  ## foo <- detection_events(x, location_col = "glatos_array2", time_sep = Inf, condense = FALSE)
  ## foo <- foo[arrive == 1,]
  x[, month := as.numeric(format(detection_timestamp_utc, "%m"))]
  x[, month_f := factor(month, levels = as.character(1:12))]
  x <- x[!is.na(sensor_real_value), c("month", "month_f", "sensor_real_value", "sensor_type", "bottom_depth_m")]
  x <- x[sensor_type == "T",]

  cols <- rainbow(2, s=0.5)
  nam <- c("J","F","M","A","M","J","J","A","S","O","N","D")

  png(filename = pth)
  boxplot(sensor_real_value ~ month_f, data = x, ylim = c(0, 25), col = cols[1], pch = 16, xlab = "month", ylab = "temperature (m)", xaxs = FALSE, las =1, names = nam, at = 1:12, outline = FALSE)
  legend("topleft", fill = cols, legend = c("fish temperature"))
  dev.off()
  return(pth)
}

##############################
  
#' @examples
#' tar_load(prep_recs)
#' tar_load(background_raw)  
#' background <- background_raw
#' out_pth <- "output/rec_map.png"

make_site_map <- function(recs, background, out_pth){
  recs <- unique(recs, by = c("glatos_array", "station_no"))
  recs <- st_as_sf(recs, coords = c("deploy_long", "deploy_lat"))

  base_lay <- raster::raster(background)
  newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  out <- raster::projectExtent(base_lay, newproj)
  out1 <- raster::projectRaster(base_lay, out)

  new_extent <- raster::extent(c(-84.6, -84.0, 45.8, 46.2))
  foo <- raster::crop(out1, new_extent)
  bar <- raster::clamp(foo, lower = -135, upper = 1, useValues = FALSE)
  my.palette <- RColorBrewer::brewer.pal(n=9, name = "Blues")

  m <- leaflet(recs)
  m <- setView(m, lng = -84.35135, lat = 45.96761, zoom = 11)
  #m <- addTiles(m, urlTemplate = "http://tileservice.charts.noaa.gov/tiles/50000_1/{z}/{x}/{y}.png", group = "navigation chart")
  m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "satellite")
  #m <- addProviderTiles(m, providers$Esri.NatGeoWorldMap, group = "alt")
  m <- addCircleMarkers(m, data = recs, color = "red", radius = 12, stroke = 0, fillOpacity = 0.8)

  m <- addRasterImage(map = m, x = bar, opacity = 1, colors = my.palette)

  #mapshot(m, file = out_pth, remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"))
  htmlwidgets::saveWidget(m, selfcontained = TRUE, out_pth)

  return(out_pth)

}

###############################################################

# this function below is used to make animated bubble plot of fish detected over time.  Got from 2020_LHTC_talk
#' tar_load(prep_recs)
#' tar_load(dead_id)
#' x <- dead_id
#' recs <- prep_recs
#' location_col = "glatos_array2"
#' background_ylim = c(41.3, 49) 
#' background_xlim = c(-92.45, -75.87)
#' dir_pth = "output/gif"
#' gif_name = "bubble_ani.gif"
#' make_gif(x = dead_id, recs = prep_recs, location_col = "glatos_array2",  background_ylim = c(41.3, 49), background_xlim = c(-92.45, -75.87), dir_pth = "output/gif")


make_gif <- function(x = dead_id, recs = prep_recs, location_col = "glatos_array2",  background_ylim = c(41.3, 49), background_xlim = c(-92.45, -75.87), dir_pth = "output/gif", gif_name = "bubble_ani.gif"){

  dtc <- copy(x)
  recs <- unique(recs, by = c("glatos_array2", "glatos_deploy_date_time", "glatos_recover_date_time"))

  t_seq <- seq(from = as.POSIXct("2018-11-08", tz = "UTC"), to = as.POSIXct("2021-07-01", tz = "UTC"), by = "1 week")

  dtc[, t_bin := t_seq[findInterval(detection_timestamp_utc, t_seq)]]
  recs[, start := glatos_deploy_date_time]
  recs[, end := glatos_recover_date_time]
  foo <- data.table(start = t_seq, end = t_seq)
  setkey(foo, start, end)
  setkey(recs, start, end)
  recs_1 <- foverlaps(recs, foo)

  paths <- file.path(dir_pth, sprintf("frame_%04d.png", 1:length(t_seq)))
  
  # make plots
  for(i in 1:length(t_seq)){
    recs.i <- recs_1[start == t_seq[i],]
    dtc.i <- dtc[t_bin == t_seq[i],]

    custom_bubble_plot1(det = dtc.i, location_col = "glatos_array2", receiver_locs = recs.i, map = NULL, col_grad = c("white", "red"), symbol_radius = 1.5, background_ylim = c(45.8, 46.05), background_xlim = c(-84.5,-84.0), scale_loc = c(-84.02, 45.80, -84.0, 45.92), out_file = paths[i])
  }

  foo <- gifski(png_files = paths, gif_file = file.path(dir_pth, gif_name), width = 800, height = 600, delay = 0.5, loop = TRUE, progress = FALSE)
  
  return(foo)

}
















  
## custom_bubble <- function (det, location_col = "glatos_array", receiver_locs = NULL, 
##     map = NULL, out_file = NULL, background_ylim = c(41.3, 49), 
##     background_xlim = c(-92.45, -75.87), symbol_radius = 1, col_grad = c("white", 
##         "red"), scale_loc = NULL) 
## {
##   library(sp)

##     if (is.null(map)) 
##         map <- greatLakesPoly
##     if (!("POSIXct" %in% class(det$detection_timestamp_utc))) {
##         stop(paste0("Column detection_timestamp_utc in det data frame must be of\n                class 'POSIXct'."), 
##             call. = FALSE)
##     }
  
##   det_summ <- glatos::summarize_detections(det, location_col = location_col, 
##         receiver_locs = receiver_locs, summ_type = "location")
##     det_summ <- det_summ[order(det_summ$num_fish), ]
##     xlabs <- round(seq(from = background_xlim[1], to = background_xlim[2], 
##         length.out = 5), 2)
##     ylabs <- round(seq(from = background_ylim[1], to = background_ylim[2], 
##         length.out = 5), 2)
##     color <- c(colorRampPalette(col_grad)(101))
##     linear_x = geosphere::distMeeus(c(background_xlim[1], background_ylim[1]), 
##         c(background_xlim[2], background_ylim[1]))
##     linear_y = geosphere::distMeeus(c(background_xlim[1], background_ylim[1]), 
##         c(background_xlim[1], background_ylim[2]))
##     figRatio <- linear_y/linear_x
##     file_type <- ifelse(is.null(out_file), NA, tools::file_ext(out_file))
##     ext_supp <- c(NA, "png", "jpeg", "png", "bmp", "tiff")
##     if (!(tolower(file_type) %in% ext_supp)) 
##         stop(paste0("Image type '", file_type, "' is not supported."), 
##             call. = FALSE)
##     if (!is.na(file_type) & tolower(file_type) == "png") 
##         png(out_file, height = 1000 * figRatio, width = 1000, 
##             pointsize = 28)
##     if (!is.na(file_type) & tolower(file_type) == "jpeg") 
##         jpeg(out_file, height = 1000 * figRatio, width = 1000, 
##             pointsize = 28)
##     if (!is.na(file_type) & tolower(file_type) == "bmp") 
##         bmp(out_file, height = 1000 * figRatio, width = 1000, 
##             pointsize = 28)
##     if (!is.na(file_type) & tolower(file_type) == "tiff") 
##         tiff(out_file, height = 1000 * figRatio, width = 1000, 
##             pointsize = 28)
##     if (is.null(out_file)) {
##      dev.new(noRStudioGD = TRUE, height = 7 * figRatio, width = 7)
##      }

##     par(mar = c(1, 0, 0, 2), oma = c(3, 5, 3, 0))

##     plot(map, xlim = background_xlim, ylim = background_ylim, 
##         axes = T, xaxs = "i", lwd = 1.5, xaxt = "n", yaxt = "n", 
##         col = "White", bg = "WhiteSmoke")

##   symbols(det_summ$mean_lon, det_summ$mean_lat, circles = rep((background_xlim[2] - 
##         background_xlim[1]) * symbol_radius/100, length(det_summ$mean_lon)), 
##         add = T, inches = FALSE, bg = color[round(det_summ$num_fish/max(det_summ$num_fish) * 
##             100, 0) + 1], fg = "black", lwd = 3)

  
##     ## symbols(det_summ$mean_lon, det_summ$mean_lat, circles = rep((background_xlim[2] - 
##     ##     background_xlim[1]) * symbol_radius/100, length(det_summ$mean_lon)), 
##     ##     add = T, inches = FALSE, bg = color[round(det_summ$num_fish/23 * 
##     ##                                                 100, 0) + 1], fg = "black", lwd = 3)
##     ## mtext(dtc.i$t_bin[1], side = 3)
##     if (any(det_summ$num_fish == 0)) {
##         with(det_summ[det_summ$num_fish == 0, ], text(mean_lon, 
##             mean_lat, "X", cex = 0.6 * symbol_radius))
##     }
##     if (is.null(scale_loc)) {
##         scale_loc <- c(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 
##             0.02, par("usr")[3] + ((par("usr")[4] - par("usr")[3]) * 
##             0.25), par("usr")[1] + ((par("usr")[2] - par("usr")[1]) * 
##             0.03), par("usr")[4] - ((par("usr")[4] - par("usr")[3]) * 
##             0.25))
##     }
##     plotrix::color.legend(scale_loc[1], scale_loc[2], scale_loc[3], 
##         scale_loc[4], paste0(" ", round(seq(from = 1, to = 23, 
##             length.out = 6), 0)), color, gradient = "y", family = "sans", 
##         cex = 0.75, align = "rb")
##     axis(1, at = xlabs, labels = paste0(format(xlabs, 4), intToUtf8(176)), 
##         cex.axis = 1)
##     mtext("Longitude", side = 1, line = 2.5, cex = 1)
##     axis(2, at = ylabs, labels = paste0(format(ylabs, 4), intToUtf8(176)), 
##         cex.axis = 1, las = 1)
##     mtext("Latitude", side = 2, line = 4, cex = 1)
##     box()
##     if (!is.na(file_type)) 
##         dev.off()
##     if (!is.na(file_type)) 
##         message(paste0("Image files were written to the following directory:\n", 
##             getwd(), "\n"))
##     return(det_summ)
## }

## custom_bubble <- function (det, location_col = "glatos_array", receiver_locs = NULL, 
##     map = NULL, out_file = NULL, background_ylim = c(41.3, 49), 
##     background_xlim = c(-92.45, -75.87), symbol_radius = 1, col_grad = c("white", 
##         "red"), scale_loc = NULL) 
## custom_bubble(det = dead_id, location_col = "glatos_array2", receiver_locs = tst_recs)


custom_bubble_plot1 <- function (det, location_col = "glatos_array", receiver_locs = NULL, 
    map = NULL, out_file = NULL, background_ylim = c(41.3, 49), 
    background_xlim = c(-92.45, -75.87), symbol_radius = 1, col_grad = c("white", 
        "red"), scale_loc = NULL) 
{
    missingCols <- setdiff(c("animal_id", "detection_timestamp_utc", 
        "deploy_lat", "deploy_long", location_col), names(det))
    if (length(missingCols) > 0) {
        stop(paste0("det is missing the following ", "column(s):\n", 
            paste0("       '", missingCols, "'", collapse = "\n")), 
            call. = FALSE)
    }
    if (is.null(map)) 
        map <- greatLakesPoly
    if (!("POSIXct" %in% class(det$detection_timestamp_utc))) {
        stop(paste0("Column detection_timestamp_utc in det data frame must be of\n                class 'POSIXct'."), 
            call. = FALSE)
    }
    det_summ <- glatos::summarize_detections(det, location_col = location_col, 
        receiver_locs = receiver_locs, summ_type = "location")
    det_summ <- det_summ[order(det_summ$num_fish), ]
    xlabs <- round(seq(from = background_xlim[1], to = background_xlim[2], 
        length.out = 5), 2)
    ylabs <- round(seq(from = background_ylim[1], to = background_ylim[2], 
        length.out = 5), 2)
    color <- c(colorRampPalette(col_grad)(101))
    linear_x = geosphere::distMeeus(c(background_xlim[1], background_ylim[1]), 
        c(background_xlim[2], background_ylim[1]))
    linear_y = geosphere::distMeeus(c(background_xlim[1], background_ylim[1]), 
        c(background_xlim[1], background_ylim[2]))
    figRatio <- linear_y/linear_x
    file_type <- ifelse(is.null(out_file), NA, tools::file_ext(out_file))
    ext_supp <- c(NA, "png", "jpeg", "png", "bmp", "tiff")
    if (!(tolower(file_type) %in% ext_supp)) 
        stop(paste0("Image type '", file_type, "' is not supported."), 
            call. = FALSE)
    if (!is.na(file_type) & tolower(file_type) == "png") 
        png(out_file, height = 1000 * figRatio, width = 1000, 
            pointsize = 28)
    if (!is.na(file_type) & tolower(file_type) == "jpeg") 
        jpeg(out_file, height = 1000 * figRatio, width = 1000, 
            pointsize = 28)
    if (!is.na(file_type) & tolower(file_type) == "bmp") 
        bmp(out_file, height = 1000 * figRatio, width = 1000, 
            pointsize = 28)
    if (!is.na(file_type) & tolower(file_type) == "tiff") 
        tiff(out_file, height = 1000 * figRatio, width = 1000, 
            pointsize = 28)
    if (is.null(out_file)) {
        dev.new(noRStudioGD = TRUE, height = 7 * figRatio, width = 7)
    }
    par(mar = c(1, 0, 0, 2), oma = c(3, 5, 1, 0))
    plot(map, xlim = background_xlim, ylim = background_ylim, 
        axes = T, xaxs = "i", lwd = 1.5, xaxt = "n", yaxt = "n", 
        col = "White", bg = "WhiteSmoke")
    symbols(det_summ$mean_lon, det_summ$mean_lat, circles = rep((background_xlim[2] - 
        background_xlim[1]) * symbol_radius/100, length(det_summ$mean_lon)), 
        add = T, inches = FALSE, bg = color[round(det_summ$num_fish/max(det_summ$num_fish) * 
                                                    100, 0) + 1], fg = "black", lwd = 3)
    mtext(det$t_bin[1], side = 3)
    if (any(det_summ$num_fish == 0)) {
        with(det_summ[det_summ$num_fish == 0, ], text(mean_lon, 
            mean_lat, "X", cex = 0.6 * symbol_radius))
    }
    if (is.null(scale_loc)) {
        scale_loc <- c(par("usr")[1] + (par("usr")[2] - par("usr")[1]) * 
            0.02, par("usr")[3] + ((par("usr")[4] - par("usr")[3]) * 
            0.25), par("usr")[1] + ((par("usr")[2] - par("usr")[1]) * 
            0.03), par("usr")[4] - ((par("usr")[4] - par("usr")[3]) * 
            0.25))
    }
    plotrix::color.legend(scale_loc[1], scale_loc[2], scale_loc[3], 
        scale_loc[4], paste0(" ", round(seq(from = 1, to = max(det_summ$num_fish), 
            length.out = 6), 0)), color, gradient = "y", family = "sans", 
        cex = 0.75, align = "rb")
    axis(1, at = xlabs, labels = paste0(format(xlabs, 4), intToUtf8(176)), 
        cex.axis = 1)
    mtext("Longitude", side = 1, line = 2.5, cex = 1)
    axis(2, at = ylabs, labels = paste0(format(ylabs, 4), intToUtf8(176)), 
        cex.axis = 1, las = 1)
    mtext("Latitude", side = 2, line = 4, cex = 1)
    box()
    if (!is.na(file_type)) 
        dev.off()
    if (!is.na(file_type)) 
        message(paste0("Image files were written to the following directory:\n", 
            getwd(), "\n"))
    return(det_summ)
}


#' @title Plot preliminary abacus plots of all detections
#' @param z data.table for plotting-detections
#' @param recs data.table of all recs
#' @param out_pth pth to save output plots
#' @examples
#' tar_load(clean_dtc)
#' z <- clean_dtc
#' out_pth = "output/abacus.pdf"
#' tar_load(prep_recs)
#' recs <- prep_recs
#' abacus_fig(z = z)

abacus_fig <- function(z, recs, out_pth = "output/abacus.pdf"){

  setkey(z, date_of_surgery, animal_id, detection_timestamp_utc)
  recs[, glatos_array2_f := as.factor(glatos_array2)]
  z[, glatos_array2_f := as.factor(glatos_array2)]

  fsh <- unique(z$animal_id)

  dtc_rng <- range(z$detection_timestamp_utc)
  rng_start <- round(dtc_rng[1], "month")
  rng_end <- round(dtc_rng[2], "month")
  
  pdf(out_pth)
  par(cex = 0.6)
  par(mar = c(4,4,4,0), oma = c(1,1,1,1))
  
  yr <- seq(as.POSIXct("2018-01-01", tz = "UTC"), as.POSIXct("2022-01-01", tz = "UTC"), by = "year")
  mon <- seq(as.POSIXct("2018-01-01", tz = "UTC"), as.POSIXct("2022-01-01", tz = "UTC"), by = "month")

  #for(i in 1:10){
  for(i in 1:length(fsh)){
    z.i <- z[animal_id == fsh[i],]
        
    plot(z.i[animal_id == fsh[i],]$detection_timestamp_utc, z.i[animal_id == fsh[i],]$glatos_array2_f, type = "p", axes = FALSE, ylim = c(0, length(levels(z$glatos_array2_f))), ylab = "depth, m", xlab = "time", xlim = as.numeric(c(rng_start, rng_end)), main = z.i$animal_id[1], pch = 16, col = "black", cex = 2)
    axis.POSIXct(1, at = yr, labels = TRUE, format = "%Y")
    axis.POSIXct(1, at = mon, labels = FALSE, format = "%m")
    axis(2, at = 1:length(levels(z.i$glatos_array2_f)), labels = levels(z.i$glatos_array2_f), las = 1)
    lines(z.i[animal_id == fsh[i],]$detection_timestamp_utc, z.i[animal_id == fsh[i],]$glatos_array2_f , col = "red", lwd = 2)
    box()
  }
     
  dev.off()
  return(out_pth)  
}

#####################


#' @title extract vrls
#' @description function opens and creates csv files from each vrl file
#' @param in_pth path to directory that contains raw vrl files
#' @param out_dir  path to directory where extracted vrl files will be written as .csv files
#' @param vdat_pth  pth to directory that contains vdat command line program
#' @examples
#' extract_vrl(in_pth = "~/Desktop/lost_AR_data/vrl", out_dir = "~/Desktop/vrl_to_csv", vdat_pth = "/home/todd/tools")
#' in_pth = "~/Desktop/lost_AR_data/vrl"
#' out_dir = "~/Desktop/vrl_to_csv"
#' vdat_pth = "/home/todd/tools"

extract_vrl <- function(in_pth, out_dir, vdat_pth){
  tdir <- tempdir()
  tdir_arg <- sprintf("--output=%s", tdir)
  
  fls <- list.files(path = in_pth, pattern="*.vrl", full.names = TRUE)
  fls_arg <- paste(shQuote(fls), collapse= " ")
   
  vdat_pth <- file.path(path.expand(vdat_pth), "vdat")

  out_dir <- path.expand(out_dir)
  out_names <- sub(".vrl$", ".csv", x = basename(fls))
  temp_out <- file.path(tdir, out_names)
  final_files <- file.path(out_dir, out_names)

  system2(vdat_pth, c("convert", "--format=csv.fathom", "--timec=default", tdir_arg, fls_arg))

  file.copy(temp_out, out_dir, overwrite = TRUE)
  file.remove(temp_out)
  
  return(final_files)
}




 

