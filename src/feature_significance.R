library(pscl)
library(lmtest)

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')

preprocess_mat = function(infile) {
  df = load_data_table(infile)
  mat = t(df[,-1])
  colnames(mat) = df[,1]
  return(mat)
}

otu_mat = preprocess_mat(infiles[4])
genes_mat = preprocess_mat(infiles[1])


log_transform = function(x) {
  x = x[!is.na(x)]
  constant = min(x[x > 0])/2.
  x = x + constant
  x = x/sum(x)
  log(x)
}

prot_000000000877 = log_transform(genes_mat[,'prot_000000000877'])
otu_11 = log_transform(otu_mat[,'OTU_11'])

mapping_maria = load_data_table('mapping_maria_BL.txt')

dependent = as.numeric(mapping_maria$Persistence)
lm_prot_000000000877 = lm(dependent~prot_000000000877)
lm_otu_11 = lm(dependent~otu_11)

summary(lm_prot_000000000877)
summary(lm_otu_11)

abs(AICc(lm_prot_000000000877)-AICc(lm_otu_11))


coxtest(lm_prot_000000000877, lm_otu_11)
jtest(lm_prot_000000000877, lm_otu_11)
