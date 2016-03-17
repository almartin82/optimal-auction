
proj_files <- list.files(path = proj_data_path)
num_projs <- length(proj_files)

#load the projections in
for (i in seq_along(proj_files)) {
  file_loc <- file.path(proj_data_path, proj_files[i])
  data <- readRDS(file_loc)
  assign(paste("proj", i, sep = ""), data)  
}
