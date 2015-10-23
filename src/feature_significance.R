source('env.R')
library(lmtest)

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')

log_transform = function(x) {
  constant = min(x[x > 0])/2.
  x = x + constant
  x = x/sum(x)
  log(x)
}

otu_mat = apply(preprocess_mat(infiles[4]), 1, log_transform)
genes_mat = apply(preprocess_mat(infiles[1]), 1, log_transform)

prot_000000000877 = genes_mat['prot_000000000877',]
otu_11 = otu_mat['OTU_11',]

mapping_maria = load_data_table('mapping_maria_BL.txt')

dependent = as.numeric(mapping_maria$Persistence)
lm_prot_000000000877 = lm(dependent~prot_000000000877)
lm_otu_11 = lm(dependent~otu_11)

summary(lm_prot_000000000877)
summary(lm_otu_11)

coxtest(lm_prot_000000000877, lm_otu_11)
jtest(lm_prot_000000000877, lm_otu_11)
