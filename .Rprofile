RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)

load_data_table = function(inf) {
  inf_path = file.path(getwd(), 'data', inf)
  return(read.table(inf_path, sep='\t', head=T))
}
