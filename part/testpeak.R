rm(list=ls())
ThresholdingAlgo <- function(y,lag,threshold,influence) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  return(signals)
}
cB58 <- readFrameFromFITS("cB58_Lyman_break.fit")
spec <- readFrameFromFITS("data/spec-1353-53083-0579.fits")
y <- cB58$FLUX
#y <- spec$flux

lag       <- 100
threshold <- 2.5
influence <- 0

# Run algo with lag = 30, threshold = 5, influence = 0
result <- ThresholdingAlgo(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(y),y,type="l",ylab="",xlab="")
lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)
sum(result$signals==-1)  

