
ag <- aggregate(repoAnalysis$V5, by= list(repoAnalysis$V1, repoAnalysis$V2, 
                                    repoAnalysis$V4), FUN = sum)

for (repository in unique(ag$Group.1)) {
  a <-  ag[which(ag$Group.1 == repository), ]
  dir.create(paste("output",repository,sep="/"), showWarnings = FALSE)
  for (declaration in unique(a$Group.3)) {
    b <- a[which(a$Group.3 == declaration), ]
    b$Group.2 <- factor(b$Group.2)
    png(paste("output", repository, paste(declaration,"graph.png", 
              sep = "_"), sep = "/")) ; par(mar=c(18, 1, 1, 1) + 1.1)
    x <- barplot(b$x, xaxt = "n", main = paste(
      paste("'", repository, "':", sep = ""), declaration))
    labs <- paste(names(table(b$Group.2)))
    text(cex=0.9, x=x, y=-0.5, labs, xpd=TRUE, srt=90, pos=2)
    dev.off()
  }
}
