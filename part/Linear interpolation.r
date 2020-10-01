x = c(1,1,0,0,5,0,10,0,0,0,1,1)
i = 1
j = 1
while(i < length(x)){
  while(x[i] != 0 & i < length(x)){
    i=i+1
  }
  j=i+1
  while(x[j] == 0){
    j=j+1
  }
  for(t in i:j-1){
    x[t]=seq(x[i-1],x[j],length.out = j-i+2)[t-i+2]
  }
  i=j+1
}
x

