x <- c(1,9,18,4,3,2,5,64,3,34,4,54,3,3,2,5,9,10,10,7,6,34,3,2,5,3,4,3)
y <- c(1,4,5,6,3,2)
z <- c(1,9,18,4,3,2,5,64,3,34,4,54,3,3,2,5,9,10,10,7,6,34,3,2,5,3,4,3)
i=2
j=1
wei_dis_data <- c(0)

for(i in 1:(length(x)-length(y)+1)){
  s = 0
  for(j in i:(i+length(y)-1)){
    s = s + z[j]*((x[j]-y[j-i+1])^2)
  }
  wei_dis_data[i]=(1/length(y))*sqrt(s)
}

wei_dis_data