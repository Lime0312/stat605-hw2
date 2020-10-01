rm(list = ls())
start = Sys.time()
library("FITSio")


#Read the file
cB58 <- readFrameFromFITS("cB58_Lyman_break.fit")
filename = dir("data/")
file_load <- c()
for(i in 1:length(filename)){
  file_load[i] <- paste("data/",filename[i],sep = "")
  assign(filename[i], readFrameFromFITS(file_load[i]))
}


end = Sys.time()
print(end-start)


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
  i = 1
  j = 1
  cor_data <- c()
  for(i in 1:(nrow(data)-nrow(cB58)+1)){
    cor_data[i]=abs(cor(data$flux[i:(i+nrow(cB58)-1)],cB58$FLUX)-1)
  }
  distance[d] = min(cor_data)
}

print(filename[order(distance,decreasing=F)[1:3]])

