x <- c(1,9,18,4,3,2,5,64,3,34,4,54,3,3,2,5,9,10,10,7,6,34,3,2,5,3,4,3)
y <- c(1,4,5,6,3,2)
i=1
j=1
cor_data <- c()
for(i in 1:(length(x)-length(y)+1)){
  cor_data[i]=abs(cor(x[i:(i+length(y)-1)],y)-1)
}
which(cor_data==min(cor_data))
x = cars
x[-1,]
