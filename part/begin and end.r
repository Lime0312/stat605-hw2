x = c(1,1,1,0,0,0,0,1,1)
i = length(x)
while(i <= length(x)){
  if(x[i]!= 0) {
    i=i-1
  }else{
    break
  }
}
if(j < length(x)){
  x[(j+1):length(x)] = x[j]
}
x

j = 1
while(j <= length(x)){
  if(x[j]!= 0) {
    j=j+1
  }
  else{
    break
  }
}
if(j > 1){
  x[1:(j-1)] =x[j]
}
x
