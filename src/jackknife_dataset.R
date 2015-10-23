source(normalizePath(getwd()), 'env.R')
source(file.path("src", "jackknife.r"))

mapping_maria = load_data_table('mapping_maria_BL.txt')

names = c('genes', 'RDP_fam', 'RDP_gen', 'OTU')

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')

XX = sapply(infiles, preprocess_normalized_mat)

run_jackknife_infiles(XX, mapping_maria$Persistence, names)

