RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)

trans.arcsine <- function(x) {
  x = x/sum(x)
  asin(sign(x) * sqrt(abs(x)))
}

preprocess_mat = function(infile) {
  df = load_data_table(infile)
  mat = t(df[,-1])
  colnames(mat) = df[,1]
  return(mat[,colnames(mat) != 'Total'])
}

preprocess_normalized_mat = function(infile) {
  t(apply(preprocess_mat(infile), 1, trans.arcsine))
}

load_data_table = function(inf) {
  inf_path = file.path(getwd(), 'data', inf)
  return(read.table(inf_path, sep='\t', head=T))
}
