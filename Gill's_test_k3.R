rm(list=ls())
fun1<-function(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3)
{
  g1=mu1+rexp(n1,rate=1/v1)
  g2=mu2+rexp(n2,rate=1/v2)
  g3=mu3+rexp(n3,rate=1/v3)
  y1=min(g1);y2=min(g2);y3=min(g3)
  S1=sum(g1-y1)/(n1-1);S2=sum(g2-y2)/(n2-1);S3=sum(g3-y3)/(n3-1)
  W21=(y2-y1)/(S2/n2);W2=max(W21,0)
  W31=(y3-y1)/(S3/n3);W32=(y3-y2)/(S3/n3)
  W3=max(W31,W32,0)
  q2=qf((1-0.05)^(1/2),2,2*n2-2);q3=qf((1-0.05)^(1/2),2,2*n3-2)
  a=0
  if(W2>q2 || W3>q3)
    a=a+1
  return(a)
}
fun4<-function(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3)
{
  c<-replicate(10000,fun1(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3))
  p=mean(c)
return(p)
}
##Size values of Table 6.1

p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=1,v2=1.5,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=0.5,v2=0.2,v3=0.8))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=2.5,v2=3,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=1,v2=2,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=7,n2=7,n3=7,mu1=1,mu2=1,mu3=1,v1=1,v2=1,v3=2))

p<-replicate(10,fun4(7,8,9,1,1,1,1,1,1))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,1,1.5,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,1,2,3))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,0.5,0.2,0.8))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,2.5,3,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,1,2,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(7,8,9,1,1,1,1,1,2))

p<-replicate(10,fun4(10,8,9,1,1,1,1,1,1))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,1,1.5,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,1,2,3))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,0.5,0.2,0.8))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,2.5,3,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,1,2,2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(10,8,9,1,1,1,1,1,2))


p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=1,v2=1.5,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=0.5,v2=0.2,v3=0.8))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=2.5,v2=3,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=1,v2=2,v3=2))
alpha<-mean(p);alpha
p<-replicate(10,fun4(n1=10,n2=80,n3=10,mu1=1,mu2=1,mu3=1,v1=1,v2=1,v3=2))


###Power values of Table 6.4

p<-replicate(2,fun4(8,10,12,0,0,0,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1,1.05,2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*1.2,1.05*1.2,2*1.2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*1.4,1.05*1.4,2*1.4,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*1.6,1.05*1.6,2*1.6,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*1.8,1.05*1.8,2*1.8,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*2,1.05*2,2*2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*2.2,1.05*2.2,2*2.2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*2.4,1.05*2.4,2*2.4,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*2.6,1.05*2.6,2*2.6,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*2.8,1.05*2.8,2*2.8,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*3,1.05*3,2*3,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*3.2,1.05*3.2,2*3.2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*3.4,1.05*3.4,2*3.4,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*3.6,1.05*3.6,2*3.6,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*3.8,1.05*3.8,2*3.8,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*4,1.05*4,2*4,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*4.2,1.05*4.2,2*4.2,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*4.4,1.05*4.4,2*4.4,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*4.6,1.05*4.6,2*4.6,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*4.8,1.05*4.8,2*4.8,2,4,6))
alpha<-mean(p);alpha
p<-replicate(2,fun4(8,10,12,1*5,1.05*5,2*5,2,4,6))
alpha<-mean(p);alpha




##Power values of Table 6.3


p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=1,mu2=1.02,mu3=1.02,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=1.2,mu2=1.02*1.2,mu3=1.02*1.2,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=1.4,mu2=1.02*1.4,mu3=1.02*1.4,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=1.6,mu2=1.02*1.6,mu3=1.02*1.6,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=1.8,mu2=1.02*1.8,mu3=1.02*1.8,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=2,mu2=1.02*2,mu3=1.02*2,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=2.2,mu2=1.02*2.2,mu3=1.02*2.2,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=2.4,mu2=1.02*2.4,mu3=1.02*2.4,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=2.6,mu2=1.02*2.6,mu3=1.02*2.6,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=2.8,mu2=1.02*2.8,mu3=1.02*2.8,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=3,mu2=1.02*3,mu3=1.02*3,v1=1,v2=1,v3=1))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=3.2,mu2=1.02*3.2,mu3=1.02*3.2,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=3.4,mu2=1.02*3.4,mu3=1.02*3.4,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=3.6,mu2=1.02*3.6,mu3=1.02*3.6,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=3.8,mu2=1.02*3.8,mu3=1.02*3.8,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=4,mu2=1.02*4,mu3=1.02*4,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=4.2,mu2=1.02*4.2,mu3=1.02*4.2,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=4.4,mu2=1.02*4.4,mu3=1.02*4.4,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=4.6,mu2=1.02*4.6,mu3=1.02*4.6,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=4.8,mu2=1.02*4.8,mu3=1.02*4.8,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha
p<-replicate(1,fun4(n1=70,n2=80,n3=100,mu1=5,mu2=1.02*5,mu3=1.02*5,v1=1,v2=2,v3=3))
alpha<-mean(p);alpha



###For getting critical points of Gill's test

rm(list=ls())
fun1<-function(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,v1,v2,v3,v4,v5)
{
  g1=mu1+rexp(n1,rate=1/v1)
  g2=mu2+rexp(n2,rate=1/v2)
  g3=mu3+rexp(n3,rate=1/v3)
  g4=mu4+rexp(n4,rate=1/v4)
  g5=mu5+rexp(n5,rate=1/v5)
  y1=min(g1);y2=min(g2);y3=min(g3);y4=min(g4);y5=min(g5)
  S1=sum(g1-y1)/(n1-1);S2=sum(g2-y2)/(n2-1);S3=sum(g3-y3)/(n3-1);S4=sum(g4-y4)/(n4-1)
  S5=sum(g5-y5)/(n5-1)
  W21=(y2-y1)/(S2/n2);W2=max(W21,0)
  W31=(y3-y1)/(S3/n3);W32=(y3-y2)/(S3/n3)
  W3=max(W31,W32,0)
  W41=(y4-y1)/(S4/n4);W42=(y4-y2)/(S4/n4);W43=(y4-y3)/(S4/n4)
  W4=max(W41,W42,W43,0)
  W51=(y5-y1)/(S5/n5);W52=(y5-y2)/(S5/n5);W53=(y5-y3)/(S5/n5);W54=(y5-y4)/(S5/n5)
  W5=max(W51,W52,W53,W54,0)
  q2=qf((1-0.05)^(1/3),2,2*n2-2);q3=qf((1-0.05)^(1/3),2,2*n3-2);q4=qf((1-0.05)^(1/3),2,2*n4-2)
  q5=qf((1-0.05)^(1/3),2,2*n5-2)
  a=0
  if(W2>q2 || W3>q3||W4>q4||W5>q5)
    a=a+1
  return(a)
}
fun2<-function(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,v1,v2,v3,v4,v5)
{
  c<-replicate(10000,fun1(n1,n2,n3,n4,n5,mu1,mu2,mu3,mu4,mu5,v1,v2,v3,v4,v5))
  p=mean(c)
  return(p)
}
c<-replicate(10,fun2(n1=50,n2=60,n3=70,n4=80,n5=90,mu1=1,mu2=1,mu3=1,mu4=1,mu5=1,
                     v1=3,v2=3,v3=3,v4=3,v5=3))
mean(c)
