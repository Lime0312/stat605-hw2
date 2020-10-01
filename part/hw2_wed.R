rm(list = ls())
library("FITSio")


#Read the file
cB58 <- readFrameFromFITS("cB58_Lyman_break.fit")
filename = dir("data/")
file_load <- c()
for(i in 1:length(filename)){
  file_load[i] <- paste("data/",filename[i],sep = "")
  assign(filename[i], readFrameFromFITS(file_load[i]))
}


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
  wei_dis_data <- c()
  i = 1
  j = 1
  while(i <= (nrow(data)-nrow(cB58)+1)){
    s = 0
    for(j in i:(i+nrow(cB58)-1)){
      s = s + data$ivar[j]*((data$flux[j]-cB58$FLUX[j-i+1])^2)
    }
    wei_dis_data[i%/%10+1]=(1/nrow(cB58))*sqrt(s)
    i=i+10
  }
  distance[d] = min(wei_dis_data)
}

print(filename[order(distance,decreasing=T)[1:3]])
