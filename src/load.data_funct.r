source('env.R')
#normalize: usually set this to false when we have merged L1-L6 taxa files 
load.data<-function(mapfile = "map.txt", otufile="otu_table_mc2_w_tax_keep2_sorted_L6.txt",
			minOTUInSamples = .000, normalize=TRUE, use.env.var=TRUE)
{
	source(paste(Sys.getenv("RSCRIPTS"),"collapse-features.r",sep="/"))

	if(use.env.var==TRUE) {
		mapfile<- paste(Sys.getenv('WORKING_DIR'),'/data/', mapfile, sep='')		
		otufile<- paste(Sys.getenv('WORKING_DIR'),'/data/', otufile, sep='')		
	} 

	map <- read.table(mapfile,sep='\t',head=T,row=1,comment='')
	rownames(map) <- make.names(rownames(map)) # this is auto done for otu table, so do this here to make sure sample ids are exactly the same 
	
		
	line1 <-readLines(otufile,n=1)
	if(line1=="# Constructed from biom file") {
		otu0 <-read.table(otufile,sep='\t',head=T,row=1,comment='',quote="",skip=1)
	} else {
		otu0 <-read.table(otufile,sep='\t',head=T,row=1,comment='',quote="")
	}

	# pathway files from picrust usually have an additional last column with descriptions, just drop this for now
	KEGG <- NA
	if(colnames(otu0)[ncol(otu0)]=="KEGG_Pathways")  
	{
		KEGG <- otu0[,ncol(otu0),drop=FALSE]
		otu0 <- otu0[,-ncol(otu0)]
	}

	otu <- t(otu0)

	if(normalize==TRUE)
	{
	  
		otu <- sweep(otu, 1, rowSums(otu), '/')	
		otu <- otu[, colMeans(otu) > minOTUInSamples, drop=FALSE] # increase minOTUInSamples up to 25%
		otu <- asin(sqrt(otu))
	  ret <- collapse.by.correlation(otu, .95)
		otu <- otu[, ret$reps]
	}
	
	
	full <- merge(otu, map, by=0)
	rownames(full) <- full$Row.names
	full <- full[,-1] 
	
	kegg <- ""
	if(!anyNA(KEGG))
	{
		kegg <- KEGG[colnames(otu),,drop=FALSE]
			#	kegg <- KEGG[which(colnames(otu) %in% rownames(KEGG)),,drop=FALSE]
	}
	
	list(full, ncol(otu))
}
