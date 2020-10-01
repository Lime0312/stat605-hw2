rm(list = ls())
library("FITSio")


cB58 <- readFrameFromFITS("cB58_Lyman_break.fit")
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
lag       <- 100
threshold <- 2
influence <- 0
target <- ThresholdingAlgo(cB58$FLUX,lag,threshold,influence)
ta = 0
for(i in 1:nrow(cB58)){
  if(target[i]==-1){
    ta=i
    break
  }
}
ta

  
#Read the file
filename = dir("data/")
file_load <- c()
for(i in 1:length(filename)){
  file_load[i] <- paste("data/",filename[i],sep = "")
  assign(filename[i], readFrameFromFITS(file_load[i]))
}

start = Sys.time()

#Preprocess data: test and_mask
distance <- c()

for(d in 1:length(filename)){
  data=get(filename[d])
  #plot(data$loglam,data$flux,type='l')
  
  #If too many and_mask != 0
  if (sum(data$and_mask != 0) > 0.5* nrow(data)){
    distance[d]=100
    next
  }
  
  
  #Deal with start and end
  i = nrow(data)
  while(i <= nrow(data)){
    if(data$and_mask[i]!=0) {
      i=i-1
    }else{
      break
    }
  }
  if(i < nrow(data)){
    data$and_mask[(i+1):nrow(data)] = 0
    data$flux[(i+1):nrow(data)] = data$flux[i]
  }
  
  j = 1
  while(j <= nrow(data)){
    if(data$and_mask[j]!=0) {
      j=j+1
    }else{
      break
    }
  }
  if(j > 1){
    data$and_mask[1:(j-1)] = 0
    data$flux[1:(j-1)] = data$flux[j]
  }
  
  #Linear Interpolation
  i = 1
  j = 1
  while(i < nrow(data)){
    while(data$and_mask[i] == 0 & i < nrow(data)){
      i=i+1
    }
    j=i+1
    if(j > nrow(data)){
      break
    }
    while(data$and_mask[j] != 0){
      j=j+1
    }
    for(t in i:j-1){
      data$flux[t]=seq(data$flux[i-1],data$flux[j],length.out = j-i+2)[t-i+2]
    }
    i=j+1
  }
  #plot(data$loglam,data$flux,type='l')
  #Calculate and find min distance
  result <- ThresholdingAlgo(data$flux,lag=100,threshold=2,influence=0)
  s=which(result==-1)
  dis_data <- c()
  for(i in 1: length(s)){
    if((s[i]+nrow(cB58)-ta) > nrow(data)){
      break
    }
    dis_data[i]=cor(cB58$FLUX , data$flux[(s[i]-ta+1):(s[i]+nrow(cB58)-ta)])
  }
  distance[d]= min(abs(dis_data-1))
}
end = Sys.time()

print(filename[order(distance,decreasing=F)[1:3]])
print(end-start)
