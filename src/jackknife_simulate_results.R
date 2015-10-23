source(file.path("src", "jackknife.r"))

set.seed(1)

mapping_maria = load_data_table('mapping_maria_BL.txt')

x = data.frame(lapply(rep(22, 3), function(x) {abs(rnorm(x))}))
colnames(x) = 1:3
y = data.frame(lapply(rep(22, 5), function(x) {abs(rnorm(x))}))
colnames(y) = 1:5

run_jackknife_infiles(list(x, y), mapping_maria$Persistence, c('x', 'y'))
