#Yukun Fang
#yfang67@wisc.edu

rm(list = ls())
library("FITSio")


cB58 <- readFrameFromFITS("cB58_Lyman_break.fit")
PeakAlgorithm <- function(y,lag,thr) {
  meanstand <- c()
  stdstand <- c()
  return.list = list()
  ystand <- y[0:lag]
  peak <- rep(0,length(y))
  meanstand[lag] <- mean(y[0:lag])
  stdstand[lag] <- sd(y[0:lag])
  i = lag + 1
  while(i <= length(y)){
    if ( thr*stdstand[i-1] > abs(y[i]-meanstand[i-1])) {
      peak[i] = 0
      ystand[i] = y[i]
    } 
    else {
      if (y[i] > log(thr)*meanstand[i-1]) {
        peak[i] = 0;
      } else {
        peak[i] = -1;
      }
      ystand[i] = ystand[i-1]
    }
    meanstand[i] = mean(ystand[(i-lag):i])
    stdstand[i] = sd(ystand[(i-lag):i])
    i=i+1
  }
  return.list$peak=peak
  return.list$meanstand=meanstand
  return.list$stdstand=stdstand
  return(return.list)
}
lag <- 100
thr <- 2
target <- PeakAlgorithm(cB58$FLUX,lag,thr)
ta = 0
for(i in 1:nrow(cB58)){
  if(target$peak[i]==-1){
    ta=i
    break
  }
}


#Read the file
filename = dir("data/")
file_load <- c()
for(i in 1:length(filename)){
  file_load[i] <- paste("data/",filename[i],sep = "")
  assign(filename[i], readFrameFromFITS(file_load[i]))
}

cB58$FLUX = cB58$FLUX-mean(cB58$FLUX)/(max(cB58$FLUX)-min(cB58$FLUX))

#Preprocess data: test and_mask
distance <- c()
index <- c()
for(d in 1:length(filename)){
  data=get(filename[d])
  #plot(data$loglam,data$flux,type='l')
  
  #If too many and_mask != 0
  if (sum(data$and_mask != 0) > 0.5* nrow(data)){
    distance[d]=3
    index[d]=FALSE
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
  data$flux= (data$flux-mean(data$flux))/(max(data$flux)-min(data$flux))

  #Calculate and find min distance
  result <- PeakAlgorithm(data$flux,lag=100,thr=2)
  s=which(result$peak==-1)
  dis_data <- c()
  for(i in 1: length(s)){
    if((s[i]+nrow(cB58)-ta) > nrow(data)){
      break
    }
    dis_data[i]=cor(cB58$FLUX , data$flux[(s[i]-ta+1):(s[i]+nrow(cB58)-ta)])
  }
  index[d] = s[which(abs(dis_data-1)==min(abs(dis_data-1)))]-100
  distance[d]= min(abs(dis_data-1))
}
hard <- matrix(cbind(distance[order(distance,decreasing=F)],
                     filename[order(distance,decreasing=F)],
                     as.numeric(index[order(distance,decreasing=F)])),
                     ncol = 3,
                     dimnames = list(c(1:100),
                     c("distance","spectrumID","i")))
write.csv(hard,"hw2.csv")
print(filename[order(distance,decreasing=F)[1:3]])


