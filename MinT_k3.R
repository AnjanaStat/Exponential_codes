rm(list=ls())
fun1<-function(n1,n2,n3,v1,v2,v3,v4)
{
  g1=rexp(n1,rate=1/v1)
  g2=rexp(n2,rate=1/v2)
  g3=rexp(n3,rate=1/v3)
  m1=min(g1);m2=min(g2);m3=min(g3)
  S1=sum(g1-m1)/(n1-1);S2=sum(g2-m2)/(n2-1)
  S3=sum(g3-m3)/(n3-1)
  V12=((n1-1)/n1^3)*S1^2+((n2-1)/n2^3)*S2^2
  V23=((n2-1)/n2^3)*S2^2+((n3-1)/n3^3)*S3^2
  T1=(m2-m1)/sqrt(V12);T2=(m3-m2)/sqrt(V23)
  #T=max(T1,T2,na.rm = FALSE)
  T=min(T1,T2,na.rm = FALSE)
  return(T)
}
fun2<-function(n1,n2,n3,v1,v2,v3)
{
  x<-replicate(1000,fun1(n1,n2,n3,v1,v2,v3))
  # arrange them in increasing order
  y<-sort(x,decreasing=FALSE)
  # gives the critical value
  c<-y[950]
  return(c)
}

fun3<-function(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3)
{
  g1=mu1+rexp(n1,rate=1/v1)
  g2=mu2+rexp(n2,rate=1/v2)
  g3=mu3+rexp(n3,rate=1/v3)
  m1=min(g1);m2=min(g2);m3=min(g3)
  S1=sum(g1-m1)/(n1-1);S2=sum(g2-m2)/(n2-1)
  S3=sum(g3-m3)/(n3-1)
  V12=((n1-1)/n1^3)*S1^2+((n2-1)/n2^3)*S2^2
  V23=((n2-1)/n2^3)*S2^2+((n3-1)/n3^3)*S3^2
  T1=(m2-m1)/sqrt(V12);T2=(m3-m2)/sqrt(V23)
  #T=max(T1,T2,na.rm = FALSE)
  T=min(T1,T2,na.rm = FALSE)
  out<-fun2(n1,n2,n3,S1,S2,S3)
  a=0
  if(T>out)
    a=a+1
  return(a)
}
fun4<-function(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3)
{
  out<-replicate(5000,fun3(n1,n2,n3,mu1,mu2,mu3,v1,v2,v3))
  alpha<-sum(out)/5000
  alpha
  return(alpha)
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
