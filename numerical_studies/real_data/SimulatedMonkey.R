library(MASS)

set.seed(125)

#RCT data
#MatchSet: 6 matched sets, each with one treated + two controls
#z: treatment indicator
#parity: exact match. Treated: one 2, one 3, two 4, two 5.
#age: diff less than 0.3. Treated: two 6, two 7, two 8.
#weight0: difference less than 1. Treated: range 5.5-8.7, mean 7.9, standard deviation 1.2.
#weight3: weight3=-2.02-0.49z-0.60parity+0.90age+0.84weight0+error(sd 0.5)
#weight6: weight6=7.37-0.27z+0.82parity-0.78age+0.43weight0+error(sd 0.5)
mut=c(7.0,3.8,7.9)
cmt=cbind(c(0.9,1.0,0.6),c(1.0,1.4,1.1),c(0.6,1.1,1.4))
lb=c(6,2,5.5)
ub=c(8,5,8.7)
Xt=mvrnorm(n=6, mu=mut, Sigma=cmt)
for (k in 1:3) Xt[,k]=pmin(ub[k],pmax(lb[k],Xt[,k]))
Xt[,2]=round(Xt[,2])
drt_sim=data.frame(MatchSet=rep(1:6,each=3),
                   z=rep(c(1,0,0),6),
                   parity=rep(Xt[,2],each=3),
                   age=c(Xt[,1],Xt[,1]+runif(6,-0.3,0.3),Xt[,1]+runif(6,-0.3,0.3)),
                   weight0=c(Xt[,3],Xt[,3]+runif(6,-1,1),Xt[,3]+runif(6,-1,1)))
attach(drt_sim)
drt_sim$weight3=-2.02-0.49*z-0.60*parity+0.90*age+0.84*weight0+rnorm(18,sd=0.5)
drt_sim$weight6=7.37-0.27*z+0.82*parity-0.78*age+0.43*weight0+rnorm(18,sd=0.5)
detach(drt_sim)
head(drt_sim)


#OS data
#sample size 591
#z: logit(P(z=1))=-1.39+0.30*age-0.26*parity-0.08*weight0
#parity: nonnegative integer (range 0-13), mean 1.9, standard deviation 2.0. 
#age: positive real number (range 2.5-18.5), mean 6.6, standard deviation 2.9
#weight0: positive real number (range 4.0-14.4), mean 7.3, standard deviation 1.7.
#weight3: weight3=1.58-0.28z-0.02parity-0.03age+0.89weight0+error(sd 0.8)
#weight6: weight6=1.86-0.13z-0.04parity-0.05age+0.87weight0+error(sd 0.8)
n=591
mu=c(6.6,1.9,7.3)
cm=cbind(c(8.2,5.1,2.8),c(5.1,4.2,1.8),c(2.8,1.8,3.0))
lb=c(2.5,0,4.0)
ub=c(18.5,13,14.4)
X=mvrnorm(n=n, mu=mu, Sigma=cm)
for (k in 1:3) X[,k]=pmin(ub[k],pmax(lb[k],X[,k]))
X[,2]=round(X[,2])
dos_sim=data.frame(parity=X[,2],
                   age=X[,1],
                   weight0=X[,3])
X=cbind(rep(1,n),X)
beta=matrix(c(-1.4,0.3,-0.3,-0.1),ncol=1)
ps=1/(1+exp(-X%*%beta))
dos_sim$z=rbinom(n,1,ps)
attach(dos_sim)
dos_sim$weight3=1.58-0.28*z-0.02*parity-0.03*age+0.89*weight0+rnorm(n,sd=0.8)
dos_sim$weight6=1.86-0.13*z-0.04*parity-0.05*age+0.87*weight0+rnorm(n,sd=0.8)
detach(dos_sim)
head(dos_sim)


###design and analysis
#### Matching
drt=drt_sim[,c('z','age','parity','weight0','weight3','weight6')]
dos=dos_sim
ps.model=glm(z~age+parity+weight0,data=dos,family=binomial)
ps=predict(ps.model,type='response')
summary(ps)
zrt=drt$z
zos=dos$z
oos=(round(dos$age)<=8) & (round(dos$age)>=6) & (dos$parity<=5) & (dos$parity>=2) & 
  (dos$weight0<=8.7) & (dos$weight0>=5.5)
vars = c('age','parity','weight0')

set.seed(10)
res = bridging2(drt,dos,zrt,zos,oos,vars,vars.extra = list('age','parity','weight0'),weight.extra = c(0.1,0.5,0),method='randomForest')
match=res$match
dfull=res$dfull
match=match[dfull[match[,1],'imagineix']==0,]


#### Table 2
## Check match
btboverlap=covbalance(.data=dfull, groupvar='groupix', imaginevar='imagineix',overlapvar='overlap',
                      matches = match,vars=vars,poolsd=NULL,
                      select=NULL,copy=res$copy,overlap=T)
btboverlap
# $meanbefore
# 1        2        3
# age     7.197788 7.192393 7.026277
# parity  2.526316 3.833333 2.759036
# weight0 7.042665 7.850394 7.392684
# 
# $meanafter
# 1        2        3
# age     7.197788 6.985250 7.069892
# parity  2.526316 2.842105 2.526316
# weight0 7.042665 7.461583 7.422054
# 
# $std_diff
# $std_diff$`1-2`
# std_diff_before std_diff_after
# age         0.002035673     0.08019932
# parity     -0.756801212    -0.18285130
# weight0    -0.503381048    -0.26107167
# 
# $std_diff$`1-3`
# std_diff_before std_diff_after
# age          0.06471812     0.04826025
# parity      -0.13475186     0.00000000
# weight0     -0.21813361    -0.23643695
# 
# $std_diff$`2-3`
# std_diff_before std_diff_after
# age          0.06268244    -0.03193907
# parity       0.62204935     0.18285130
# weight0      0.28524744     0.02463471
btbnonoverlap=covbalance(.data=dfull, groupvar='groupix', imaginevar='imagineix',overlapvar='overlap',
                         matches = match,vars=vars,select=c(1,3),poolsd=NULL,copy=res$copy,overlap=F)
btbnonoverlap
# $meanbefore
# 1        3
# age     6.751425 6.258872
# parity  1.869565 1.750809
# weight0 7.109646 7.189851
# 
# $meanafter
# 1        3
# age     6.751425 6.705781
# parity  1.869565 1.888199
# weight0 7.109646 7.138495
# 
# $std_diff
# $std_diff$`1-3`
# std_diff_before std_diff_after
# age          0.18586016     0.01722334
# parity       0.06876327    -0.01078936
# weight0     -0.04998410    -0.01797875



### Inference
#calculate weights
ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
tab=table(matchix)
copy=numeric(length(res$copy))
copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
wt = copy/sum(copy)*nrow(drt)

yy3=drt$weight3
yy6=drt$weight6
xx=data.matrix(drt[,c('age','parity','weight0')])
drt$res3 = adjust.match(X=xx,Y=yy3,strata=drt_sim$MatchSet)
drt$res6 = adjust.match(X=xx,Y=yy6,strata=drt_sim$MatchSet)

yy3=res$dfull[c(res$match[,1],res$match[,3]),'weight3']
yy6=res$dfull[c(res$match[,1],res$match[,3]),'weight6']
st=rep(1:nrow(res$match),2)
xx=data.matrix(res$dfull[c(res$match[,1],res$match[,3]),c('age','parity','weight0')])
res3 = adjust.match(X=xx,Y=yy3,strata=st)
res6 = adjust.match(X=xx,Y=yy6,strata=st)



#### TABLE 3
#outcome 1: 6 months weight
###covarite adjustment
ddrt=data.frame(Z=zrt,Y=drt$res6)
## weighted outcomes
ddrt$Ywt = ddrt$Y*wt
uniroot(foo_rct, c(-10,10), data=ddrt, delta=0, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.6290262
# find confidence limit for 95% lower sided interval (-inf, CL]
ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
-uniroot(foo_rct, c(-10,10), data=ddrt1, delta=0, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#0.6478741

#os
y1=res6[1:nrow(res$match)]
y0=res6[(nrow(res$match)+1):(2*nrow(res$match))]
ymat = cbind(y1,y0)
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_os, c(-1,1), ymat=ymat, gamma=1, alpha=0.025)$root
#-0.2323058
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_os, c(-1,1), ymat=-ymat, gamma=1, alpha=0.025)$root
#0.07283127

#combined
## weighted outcomes
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_combined, c(-1,1), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.2561933
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_combined, c(-1,1), data=ddrt1, delta=0, ymat=-ymat, gamma=1, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#0.09376949


#outcome 2: 3 months weight
###covarite adjustment
ddrt=data.frame(Z=zrt,Y=drt$res3)
## weighted outcomes
ddrt$Ywt = ddrt$Y*wt
uniroot(foo_rct, c(-10,10), data=ddrt, delta=0, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
# -0.8261849
# find confidence limit for 95% lower sided interval (-inf, CL]
ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
-uniroot(foo_rct, c(-10,10), data=ddrt1, delta=0, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#0.05231053

#os
y1=res3[1:nrow(res$match)]
y0=res3[(1+nrow(res$match)):(2*nrow(res$match))]
ymat = cbind(y1,y0)
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_os, c(-1,1), ymat=ymat, gamma=1, alpha=0.025)$root
#-0.4225783
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_os, c(-1,1), ymat=-ymat, gamma=1, alpha=0.025)$root
#-0.156869
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_os, c(-1,1), ymat=ymat, gamma=1.48, alpha=0.025)$root
#-0.575881
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_os, c(-1,1), ymat=-ymat, gamma=1.48, alpha=0.025)$root
#-0.002324464

#combined
## weighted outcomes
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_combined, c(-1,1), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.4475546
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_combined, c(-1,1), data=ddrt1, delta=0, ymat=-ymat, gamma=1, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.1637006
# find confidence limit for 95% upper sided interval [CL, inf)
uniroot(foo_combined, c(-1,1), data=ddrt, delta=0.1, ymat=ymat, gamma=1.56, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.610935
# find confidence limit for 95% lower sided interval (-inf, CL]
-uniroot(foo_combined, c(-1,1), data=ddrt1, delta=0.1, ymat=-ymat, gamma=1.56, alpha=0.025, ri=T,rblock=drt_sim$MatchSet)$root
#-0.001117198


library(ggplot2)
trydelta=c(0,0.05,0.1,0.15,0.2)
trygamma=c(1,1.2,1.4,1.56,1.6)
pvals1=c()
for (d in trydelta){
  for (g in trygamma){
    pvals1=c(pvals1,
             combined_pval(0, ymat=ymat, delta=d, gamma=g, data=ddrt, rblock=drt_sim$MatchSet, ri=T))
  }
}

pvals2=c()
for (d in trydelta){
  for (g in trygamma){
    pvals2=c(pvals2,
             combined_pval(0, ymat=-ymat, delta=d, gamma=g, data=ddrt1, rblock=drt_sim$MatchSet, ri=T))
  }
}

pvals=2*pmin(pvals1,pvals2)

plot.df=data.frame(Pvalue=round(pvals,3),
                   Delta=rep(trydelta,each=length(trygamma)),
                   Gamma=rep(trygamma,length(trydelta)),
                   Col=as.numeric(pvals>0.05))
plot.df$Delta=as.factor(plot.df$Delta)
plot.df$Gamma=as.factor(plot.df$Gamma)


ggplot(plot.df, aes(y=Delta,x=Gamma,fill=Col,label = Pvalue))+
  geom_tile()+
  geom_text(col = "black",na.rm=T) +
  scale_fill_gradient(high = "#547298",low = "#DADEE7",na.value = "white") +
  xlab("Gamma") + 
  ylab("Delta")+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        legend.position="none")+
  coord_fixed(ratio = 0.6)+
  ggtitle('Two-sided P-values for Three-month Postpartum Maternal Weight')+
  theme(plot.title = element_text(hjust = 0.5))
