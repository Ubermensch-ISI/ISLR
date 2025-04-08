require(animation)
require(ggplot2)
ani.options(nmax=50)
x=0
y=0
par(bg="blue")
p=0.5
eat_x=sample(-4:4,size=1)
eat_y=sample(-4:4,size=1)

for(i in 1:50)
{
  
 
   plot(x,y ,ylim=c(-5,5),col="red",xlim=c(-5,5),lwd=4)
   grid(NULL,NULL)
  
  
  
  
  
  points(eat_x,eat_y,col="green")
  
  
  
  m=matrix(c(1,-1,0,0,0,0,1,-1),nr=4)[as.integer(readline()),]
  x= m[1]+x
  y=m[2]+y
  
  if (x==eat_x & y==eat_y) 
  {
    text(0,0,paste("winner"))
    break()
  }
  ani.pause()
}











