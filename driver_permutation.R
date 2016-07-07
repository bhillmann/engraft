infiles = c('RDP_family_AH_standarized.txt', 
            'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt', 'genes_CEFITH_standardized.txt')
source(file.path('src', 'rf_permutation_test.R'))
source(file.path('src', 'preprocess.R'))
source(file.path('src', 'cross_validation.R'))

mapping_maria = load_data_table('mapping_maria_BL.txt')

dependent = mapping_maria$Persistence

sink('output_perm.txt')

for (file in infiles) {
  print(file)
  results = permutation_test(prevalence(preprocess_mat(file)), dependent)
  print(mean(results$permutation_errs >= results$err))
}

sink()