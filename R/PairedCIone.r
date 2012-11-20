
PairedCIone<-function(n12, t, n21, confidence.level, alternative,precision,grid.one,grid.two){
pround=0
while(1/precision>=10^pround)
{pround=pround+1}

n<-n12+t+n21
datavectorL<-n12*(n+2)+t
datavectorU<-(n-n12-t)*(n+2)+t
output<-c()
output[1]<-round(2*n12/n+t/n-1,digits=6)
delta<-10^(-10)
alpha=1-confidence.level

f<-array(,dim=c((n+1)*(n+2)/2,7))
S<-array(,dim=c((n+1)*(n+2)/2,2))
N<-array(,dim=c((n+1)*(n+2)/2,3))
NC<-array(,dim=c((n+1)*(n+2)/2,3))
Ls<-array(,dim=c((n+1)*(n+2)/2,7))

num=0
for(i in 0:n)
for(j in 0:(n-i))
{
num=num+1
f[num,1:3]=c(i,j,n-i-j)
}
num

p1hat<-f[,1]/n
p0hat<-f[,3]/n
f[,7]=(p1hat-p0hat)


f<-f[order(-f[,7]),]

allvector<-f[,1]*(n+2)+f[,2]
allvector<-round(allvector)

allvectormove<-(f[,1]+1)*(n+3)+(f[,2]+1)
allvectormove<-round(allvectormove)

############### for the first table ############################

I1<-f[1,1]
I2<-f[1,2]
I3<-n-I1-I2
part1<-lfactorial(n)-lfactorial(I1)-lfactorial(I2)-lfactorial(I3)

prob<-function(delv)
{
delvalue<-delv
p0<-seq(delta,min(1-delvalue-delta,1+delvalue-delta),length=500)
part2<-I1%*%t(log((1+delvalue-p0)/2)) +(I2)%*%t(log(p0)) + (I3)%*%t(log((1-p0-delvalue)/2))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-max(sumofprob)
return(mansum)
}


imax=1-100*delta
imin=-1+delta
while(abs(imax-imin)>=0.00001)
{
mid<-(imax+imin)/2
probmid<-prob(mid)
if(probmid>=alpha)
{imax<-mid}else{imin<-mid}
}
f[1,4]=round(imin,digits=pround)
Ls[1,]<-f[1,]

partvector<-round(Ls[1,1]*(n+2)+Ls[1,2])
allvector<-setdiff(allvector,partvector)



################### from the second table  ################################

morepoint=1
kk=1
kk1=1
dimoftable<-dim(Ls)[1]


if(n12==n && t==0 && alternative=="Lower"){output[2]=Ls[1,4];output[3]=1;kk<-dimoftable}
if(n12==0 && t==0 && alternative=="Lower"){output[2]=-1;output[3]=1;kk<-dimoftable}
if(n12==n && t==0 && alternative=="Upper"){output[2]=-1;output[3]=1;kk<-dimoftable}  
if(n12==0 && t==0 && alternative=="Upper"){output[2]=-1;output[3]=-Ls[1,4];kk<-dimoftable}  

while(kk<=(dimoftable-2))
{
C<-Ls[(kk-morepoint+1):kk,1:2]
S[(kk-morepoint+1):kk,]<-C
DD<-S[1:kk,]
if(kk==1){DD[2]<-DD[2]-1}else{
DD[,2]<-DD[,2]-1}
A<-DD
DD<-S[1:kk,]
if(kk==1){DD[2]<-DD[2]+1;DD[1]<-DD[1]-1}else{
DD[,2]<-DD[,2]+1;DD[,1]<-DD[,1]-1}
B<-DD

N<-rbind(A,B)
N<-unique(N)


Nvector<-c()
Nvector<-(N[,1]+1)*(n+3)+N[,2]+1
Nvector<-Nvector[is.element(Nvector,allvectormove)]

SKvector<-(S[1:kk,1]+1)*(n+3)+S[1:kk,2]+1
Nvectortemp<-Nvector[!is.element(Nvector,SKvector)]
Ntemp<-array(,dim=c(length(Nvectortemp),2))
Ntemp[,2]<-Nvectortemp%%(n+3)
Ntemp[,1]<-(Nvectortemp-Ntemp[,2])/(n+3)-1
Ntemp[,2]<-Ntemp[,2]-1
N<-Ntemp


####################  NC ####


Nvector<-(N[,1]+1)*(n+3)+N[,2]+1
Nvector1<-(N[,1]+1)*(n+3)+N[,2]+2
Nvector2<-(N[,1]+2)*(n+3)+N[,2]+0
drop<-is.element(Nvector1,Nvector)*1+is.element(Nvector2,Nvector)*1
M<-cbind(N,drop)
MM<-M[which(M[,3]<0.5),]
NC<-MM


if(length(NC)<=3){lengthNC=1;NMN=array(c(100,100,100,100,100,100,100),dim=c(2,3));NMN[1,]<-NC;NC<-NMN}else{lengthNC=length(na.omit(NC[,1]))}
for(i in 1:lengthNC)
{
imax=1-100*delta
imin=-1+delta

Ls[kk+1,1:2]<-NC[i,1:2]
I1<-Ls[1:(kk+1),1]
I2<-Ls[1:(kk+1),2]
I3<-n-I1-I2

part1<-lfactorial(n)-lfactorial(I1)-lfactorial(I2)-lfactorial(I3)

if(kk>(dimoftable-2)/2)
{
partvector<-round(NC[i,1]*(n+2)+NC[i,2])
leftvector<-setdiff(allvector,partvector)
I2<-round(leftvector%%(n+2))
I1<-round((leftvector-leftvector%%(n+2))/(n+2))
I3<-n-I1-I2
part1<-lfactorial(n)-lfactorial(I1)-lfactorial(I2)-lfactorial(I3)
}



prob2step<-function(delv)
{
delvalue<-delv
p0<-seq(delta,min(1-delvalue-delta,1+delvalue-delta),length=grid.one)
part2<-I1%*%t(log((1+delvalue-p0)/2)) +(I2)%*%t(log(p0)) + (I3)%*%t(log((1-p0-delvalue)/2))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-max(sumofprob)
stepv<-(p0[grid.one]-p0[1])/grid.one
maxloc<-which(sumofprob==mansum)
lowerb<-max(p0[1],p0[maxloc]-stepv)+delta
upperb<-min(p0[grid.one],p0[maxloc]+stepv)-delta
p0<-seq(lowerb,upperb,length=grid.one)
part2<-I1%*%t(log((1+delvalue-p0)/2)) +(I2)%*%t(log(p0)) + (I3)%*%t(log((1-p0-delvalue)/2))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-max(sumofprob)
return(mansum)
}


prob2steplmin<-function(delv)
{
delvalue<-delv
p0<-seq(delta,min(1-delvalue-delta,1+delvalue-delta),length=grid.one)
part2<-I1%*%t(log((1+delvalue-p0)/2)) +(I2)%*%t(log(p0)) + (I3)%*%t(log((1-p0-delvalue)/2))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-min(sumofprob)
stepv<-(p0[grid.one]-p0[1])/grid.one
maxloc<-which(sumofprob==mansum)
lowerb<-max(p0[1],p0[maxloc]-stepv)+delta
upperb<-min(p0[grid.one],p0[maxloc]+stepv)-delta
p0<-seq(lowerb,upperb,length=grid.two)
part2<-I1%*%t(log((1+delvalue-p0)/2)) +(I2)%*%t(log(p0)) + (I3)%*%t(log((1-p0-delvalue)/2))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-min(sumofprob)
return(mansum)
}

imax=Ls[kk,4]
imin=-1+delta
if(i==1){NCmax<-imin}

if(kk<=(dimoftable-2)/2){
while(abs(imax-imin)>=0.1)
{
mid<-(imax+imin)/2
probmid<-prob2step(mid)
if(probmid>=alpha)
{imax<-mid}else{imin<-mid}
}
NC[i,3]<-round(imin,digits=2)
if(imax>=NCmax){
while(abs(imax-imin)>=precision)
{
mid<-(imax+imin)/2
probmid<-prob2step(mid)
if(probmid>=alpha)
{imax<-mid}else{imin<-mid}
}
NC[i,3]<-round(imin,digits=pround)
                               }
NCmax<-max(NCmax,imin)
         }else{
while(abs(imax-imin)>=0.1)
{
mid<-(imax+imin)/2
probmid<-prob2steplmin(mid)
if(probmid>=(1-alpha))
{imin<-mid}else{imax<-mid}
}
NC[i,3]<-round(imin,digits=2)
if(imax>=NCmax){
while(abs(imax-imin)>=precision)
{
mid<-(imax+imin)/2
probmid<-prob2steplmin(mid)
if(probmid>=(1-alpha))
{imin<-mid}else{imax<-mid}
}
NC[i,3]<-round(imin,digits=pround)
                               }
NCmax<-max(NCmax,imin)
                } 
}##  end of i loop



morepointLsest<-function(morekk)
{
imax=1-100*delta
imin=-1+delta

I1<-Ls[1:morekk,1]
I2<-Ls[1:morekk,2]

prob2step<-function(delv)
{
delvalue<-delv
if(delvalue<0){p0<-seq(-delvalue+delta,1-delta,length=grid.one)}else{
p0<-seq(delta,1-delvalue-delta,length=grid.one)}
part1<-lchoose(n,I1) + I1%*%t(log(p0+delvalue)) + (n-I1)%*%t(log(1-p0-delvalue))
part2<-lchoose(m,I2) + I2%*%t(log(p0)) + (m-I2)%*%t(log(1-p0))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-max(sumofprob)
stepv<-(p0[grid.one]-p0[1])/grid.one
maxloc<-which(sumofprob==mansum)
lowerb<-max(p0[1],p0[maxloc]-stepv)+delta
upperb<-min(p0[grid.one],p0[maxloc]+stepv)-delta
p0<-seq(lowerb,upperb,length=grid.two)
part1<-lchoose(n,I1) + I1%*%t(log(p0+delvalue)) + (n-I1)%*%t(log(1-p0-delvalue))
part2<-lchoose(m,I2) + I2%*%t(log(p0)) + (m-I2)%*%t(log(1-p0))
sum<-part1+part2
expsum<-exp(sum)
sumofprob<-colSums(expsum)
mansum<-max(sumofprob)
return(mansum)
}

imax=Ls[kk,4]
imin=-1+delta
if(i==1){NCmax<-imin}

while(abs(imax-imin)>=precision)
{
mid<-(imax+imin)/2
probmid<-prob2step(mid)
if(probmid>=alpha)
{imax<-mid}else{imin<-mid}
}
NCres<-round(imin,digits=pround)
return(NCres)
	}## end of function morepointLsest



if(i>=2)
{NCnomiss<-NC[1:dim(na.omit(NC))[1],]
NCnomiss<-NCnomiss[order(-NCnomiss[,3]),]
morepoint<-sum(NCnomiss[,3]>=NCnomiss[1,3]-delta)
if(morepoint>=2){
Ls[(kk+1):(kk+morepoint),1:2]<-NCnomiss[1:morepoint,1:2]
Ls[(kk+1):(kk+morepoint),4]<-NCnomiss[1:morepoint,3]
for(iq in 1:morepoint)
{partvector<-round(Ls[kk+iq,1]*(n+2)+Ls[kk+iq,2])
allvector<-setdiff(allvector,partvector)}
kk=kk+morepoint
             }else{Ls[kk+1,1:2]<-NCnomiss[1,1:2];Ls[kk+1,4]<-NCnomiss[1,3]
                   partvector<-round(Ls[kk+1,1]*(n+2)+Ls[kk+1,2])
                   allvector<-setdiff(allvector,partvector);kk=kk+1
                   }
}else{NCnomiss<-NC
Ls[kk+1,1:2]<-NCnomiss[1,1:2]
Ls[kk+1,4]<-NCnomiss[1,3]
partvector<-round(Ls[kk+1,1]*(n+2)+Ls[kk+1,2])
allvector<-setdiff(allvector,partvector)
kk=kk+1
}

if(alternative=="Lower"){
if((is.element(datavectorL,allvector)*1)==0){
   for(jj in (kk1+1):kk){
        if(Ls[jj,1]==n12 && Ls[jj,2]==t){output[2]=Ls[jj,4]}
                         }
if(kk>=(kk1+2)){output[2]<-morepointLsest(kk)} 
	output[3]=1;kk<-dimoftable
}
	                    }else{ 
if((is.element(datavectorU,allvector)*1)==0){
   for(jj in (kk1+1):kk){
        if(Ls[jj,1]==(n-n12-t) && Ls[jj,2]==t){output[3]=-Ls[jj,4]}
                         }
if(kk>=(kk1+2-delta)){output[3]<--morepointLsest(kk)}                       
output[2]=-1;kk<-dimoftable
	}
                            }
kk1=kk
}
return(output)
}

