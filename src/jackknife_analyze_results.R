source(normalizePath(getwd()), 'env.R')

load_result_table = function(inf) {
  inf_path = file.path(getwd(), 'results', inf)
  return(read.table(inf_path, sep='\t', head=T))
}

x = load_result_table("mean_importance_x.txt")
y = load_result_table("mean_importance_y.txt")
