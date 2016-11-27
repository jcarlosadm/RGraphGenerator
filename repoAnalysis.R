
ag <- aggregate(repoAnalysis$V5, by= list(repoAnalysis$V1, repoAnalysis$V2, 
                                    repoAnalysis$V4), FUN = sum)

for (repository in unique(ag$Group.1)) {
  a <-  ag[which(ag$Group.1 == repository), ]
  dir.create(paste("output",repository,sep="/"), showWarnings = FALSE)
  for (declaration in unique(a$Group.3)) {
    b <- a[which(a$Group.3 == declaration), ]
    b$Group.2 <- factor(b$Group.2)
    if( max(b$x) < 5 ) { next }
    WDT = length(b$x)/5
    HGT = max(b$x)/5
    if (WDT < 5) { WDT = 5 }
    if (HGT < 3){ HGT = 3 }
    png(paste("output", repository, paste(declaration,"graph.png", 
              sep = "_"), sep = "/"), width=WDT, height=7, units="in",
              res=200) ; par(mar=c(18, 1, 1, 1) + 1.1)
    x <- barplot(b$x, xaxt = "n", main = paste(
      paste("'", repository, "':", sep = ""), declaration))
    labs <- paste(names(table(b$Group.2)))
    text(cex=0.9, x=x+0.5, y=-0.5, labs, xpd=TRUE, srt=90, pos=2)
    dev.off()
  }
}
