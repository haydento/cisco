library(targets)
library(tarchetypes)

source("src/functions.R")

tar_option_set(packages = c("data.table", "glatos", "sf", "raster", "leaflet", "mapview", "sp", "gifski"))

list(
  tar_target(raw_detections,
             "~/Documents/Lake_Huron_cisco/data/receivers/vrl_to_csv",
             format = "file"
             ),
  tar_target(project_data,
             "~/Documents/Lake_Huron_cisco/dbase_submission/cisco_dbase.ods",
             format = "file"
             ),
  tar_target(compile_dtc,
             compile_det(pth = raw_detections),
             format = "fst_dt"
             ),
  tar_target(prep_recs,
             clean_recs(pth = project_data),
             format = "fst_dt"
             ),
  tar_target(prep_tags,
             clean_tags(pth = project_data),
             format = "fst_dt"
             ),
  tar_target(dtc,
             add_locs_tags(det = compile_dtc, recs = prep_recs, tagging = prep_tags),
             format = "fst_dt"
             ),
  tar_target(all_depth,
             depth_fig(z = clean_dtc, recs = prep_recs, out_pth = "output/pressure_temp.pdf"),
             format = "file"
             ),
  tar_target(find_min_lag,
             min_lag2(det = dtc, tag_serial_number = TRUE),
             format = "fst_dt"
             ),
  tar_target(clean_dtc,
             id_false(find_min_lag, time_interval = 240 * 30, out_pth = "output/false_detections.pdf"),
             format = "fst_dt"
             ),
  tar_target(dead_id,
             manual_dead_id(y = clean_dtc),
             format = "fst_dt"
             ),
  tar_target(daily_mean_depth,
             daily_sensor(x = dead_id, param = "pressure", t_interval = "1 day"),
             format = "fst_dt"
             ),
  tar_target(daily_mean_temp,
             daily_sensor(x = dead_id, param = "temperature", t_interval = "1 day"),
             format = "fst_dt"
             ),
  tar_target(seasonal_temp_plot,
             cisco_depth_plot(x = daily_mean_temp, out_pth = "output/seasonal_temp.png", seasonal = TRUE, ylim = c(0,25), ylab = "temperature (C)"),
             format = "file"
             ),
  tar_target(monthly_temp_plot,
             cisco_depth_plot(x = daily_mean_temp, out_pth = "output/monthly_temp.png", seasonal = FALSE, ylim = c(0,25), ylab = "temperature (C)"),
             format = "file"
             ),
  tar_target(seasonal_depth_plot,
             cisco_depth_plot(x = daily_mean_depth, out_pth = "output/seasonal_depth.png", seasonal = TRUE),
             format = "file"
             ),
  tar_target(monthly_depth_plot,
             cisco_depth_plot(x = daily_mean_depth, out_pth = "output/monthly_depth.png", seasonal = FALSE),
             format = "file"
             ),
  tar_target(background_raw,
             "~/Documents/Lake_Huron_cisco/cisco_analyses/cisco_rpt_2021/data/huron_lld.tif",
             format = "file"
             ),
  tar_target(site_map,
             make_site_map(recs = prep_recs, background = background_raw, out_pth = "output/rec_map.html"),
             format = "file"
             ),
  tar_render(pres, path = "src/cisco_advisory.Rmd", output_dir = "output", output_file = "pres.html"),

  tar_target(video,
             make_gif(x = dead_id, recs = prep_recs, location = "glatos_array2", background_ylim = c(41.3, 49), background_xlim = c(-92.45, -75.87), dir_pth = "output/gif"),
             format = "file"
             ),

  tar_target(abacus,
             abacus_fig(z = clean_dtc, recs = prep_recs, out_pth = "output/abacus.pdf"),
             format = "file"
             )
)


             

             



     
