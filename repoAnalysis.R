countMax = length(unique(repoAnalysis$V1))
count = as.integer(0)

for (repository in unique(repoAnalysis$V1)) {
	count = count + 1
	print(paste0(formatC(count*100/countMax,format="f",digits=2),"%"))

	a <- repoAnalysis[which(repoAnalysis$V1 == repository),]
	
	FOLDERPATH = paste("output",repository,sep="/")
	dir.create(FOLDERPATH, showWarnings = FALSE)
	
	for (declaration in unique(a$V4)) {
		ag <- a[which(a$V4 == declaration),]
		b <- aggregate(ag$V5, by=list(factor(ag$V2,levels=unique(ag$V2))), FUN=sum)

		b$Group.1 <- factor(b$Group.1)
    
		if( max(b$x) < 5 ) { next }

		WDT = length(b$x)/5
		if (WDT < 5) { WDT = 5 }

		FILEPATH = paste(FOLDERPATH, paste(declaration,"graph.png", sep = "_"), sep = "/")

		res <- try(png(FILEPATH, width=WDT, height=10, units="in", res=200))

		if(inherits(res, "try-error")){ next }

		par(mar=c(18, 1, 1, 1) + 1.1)

		PLOTNAME = paste(paste("'", repository, "':", sep = ""), declaration)
		x <- barplot(b$x, xaxt = "n", main = PLOTNAME)
		labs <- paste(names(table(b$Group.1)))
		text(cex=0.9, x=x+0.5, y=-0.5, labs, xpd=TRUE, srt=90, pos=2)

		dev.off()
	}

	if(length(dir(FOLDERPATH)) == 0) { unlink(FOLDERPATH, recursive=TRUE, force=TRUE) }
}
