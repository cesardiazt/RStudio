


esBisciesto<-function(a,b){
  x<-a
  annos<-integer(0)
  
  while(x<=b){
    if(x%%4==0){
      if((x%%100==0) & (x%%400==0)){
        annos<-c(annos,x)
      }
      else if(!(x%%100==0)){
        annos<-c(annos,x)
      }
    }
    x<-x+1
  }
  return(annos)
}


ejeX<-function(texto,vector){
  a<-paste(texto,vector)
  return(a)
}

esTriangulo<- function(a,b,c){
  
  if(((a+b)>c)&((a+c)>b)&((a+c)>b)){
    return(TRUE)
  } 
  else{
    return(FALSE)
  }
}

leibniz<-function(n){
  i<-0
  suma<-0
  while(i<=n){
    suma<-suma+((-1)^i)*(1/(2*i+1))
    i<-i+1
  }
  return(suma*4)
}
  

