library(magrittr)
a=1/2
b=1/3
c=1/6

Ea = (1+1/(1-a)
  +b*(1/c)*(a/(a+b))
  +c*(1/b)*(a/(a+c)))
Eb = (c*1/a + a*(1+(1/c)*a/(a+b)) )/(1-b)
Ec=(b*1/a + a*(1+(1/b)*a/(a+c)) )/(1-c)

a*Ea+b*Eb+c*Ec

gemCountSim<- function(){
vecCount = c(0,0,0)
while(TRUE){
 draw = rmultinom(1,1,c(1/2,1/3,1/6))%>%as.vector
 vecCount = vecCount + draw
 if(vecCount%>%min>0) return(vecCount[1])
}
}

replicate(1000,gemCountSim(),simplify="array")%>%mean

