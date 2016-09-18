require(stats)

norm  <- rnorm(9000,0,5)
norm2 <- rnorm(9000,0,3)

func <- function(x, a, b, c) {
  a*exp(-0.5*(x-b)^2/(c)^2)
}


pdf("samplePlots.pdf")
dN <- data.frame(norm,norm2)
hist(norm, xlab="Gaus 1", ylab="Counts", breaks=25, prob=TRUE, main="Fit Gaussian")
curve(dnorm(x, mean=mean(norm), sd=sd(norm)), add=TRUE, col="blue", lwd=2)

plot(dN, type="p", col="red", xlab="Gaus 1", ylab = "Gaus 2", asp=1)
title(main="2D Gaussian", col.main="black", font.main="4")
dev.off()
