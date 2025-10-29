args = commandArgs(trailingOnly=TRUE)
id=as.numeric(args)[1]	# This choose the target state, and speciality type

library(mvtnorm)

options(show.error.messages = TRUE)

source('CombineMatching.R')
source('CombineInference.R')



#keyvalues=read.csv('keyvalues_v1.csv')


olist=2#1:3#c('all','majority','limited')
# dlist=c(0, .05, .1)
# glist=c(1, 1.5, 2)

nlist = 1000
 
seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)
dlist = c(0,.2,.5)
glist = c(1,1.2,1.5)


foo <- function(x, y) c(x, y)


o_d_seq = outer(olist, dlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_seq = outer(o_d_seq, glist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_n_seq = outer(o_d_g_seq, nlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = outer(o_d_g_n_seq, seedseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = sapply(setup, cbind)

#idx = (setup[4,]==750) | (setup[4,]==500 & setup[3,]==1.5)

#setup = setup[,idx]

#setup = setup[,c(21,  75, 129, 183, 237, 291, 345, 399, 453, 507)]
nvar=5

k = c('all','majority','limited')
k = k[setup[1,id]]
delta = setup[2,id]
gamma = setup[3,id]
n = setup[4,id]
thisseed = setup[5,id]

set.seed(thisseed)

print(c(k,delta,gamma))


nvar=5

###########################################################################
################### Calculate proper scaling values for simulation
path = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap_v4.csv')

if(!file.exists(path)){

	N=10^6
	nvar=5
	set.seed(123)
	x=mvtnorm::rmvnorm(N,mean=rep(0,nvar))
	u=runif(N)

	if (k=='all') o=rep(1,N)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)


	a=a+1
	print(a)

	if (k=='all') o=rep(1,N)
	  if (k=='majority') o=as.numeric(x[,1]>=-1)
	  if (k=='limited') o=as.numeric(x[,1]>=0)
	  
	 
			  pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
			  s=rbinom(N,1,pselect) #around 1/7 RCT & 6/7 OS
			  ps.rct=1/2
			  ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
			  ps[s==1]=ps.rct
			  ps.os=ps[s==0]
			  


	a=a+1
	print(a)

	# ## NEW
	z=rep(0,N) ## NEW
	z[s==1]=rbinom(sum(s==1),1,ps[s==1]) #around 25% T and 75% C in the OS. 
	z[sample(which(s==0),prob=ps[s==0])[1:(ceiling(.25*sum(s==0)))]]=1 
	# #around 25% T and 75% C in the OS. 

		  if (delta==0 | k=='all'){ 
			scale.factor=1
		  } else {
			scale.factor=sum(z==1 & s==0 & o==0)/sum(z[s==0])
			}
			
	a=a+1
	print(a)

		  y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(N)
		  y1=y0+delta*o/scale.factor
		  true.value=mean((y1-y0)[s==0 & z==1])
		  #res=rbind(res,c(k,delta,gamma,scale.factor,true.value))


	a=a+1
	print(a)

		print(c(k,delta,gamma,scale.factor,true.value))


	path = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap_v4.csv')
	write.csv(c(k,delta,gamma,scale.factor,true.value), path)
}

######## RCT outside OS

path = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap_v4_RCToutsideOS.csv')

if(!file.exists(path)){

	N=10^6
	nvar=5
	set.seed(123)
	x=mvtnorm::rmvnorm(N,mean=rep(0,nvar))
	u=runif(N)

	if (k=='all') o=rep(1,N)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)
			  

	a=a+1
	print(a)

	outside = rbinom(N, 1, 0.1)  # ~10% flagged as outside support

	# Selection into RCT or OS
	pselect = 1 / (1 + exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4]))) * o
	s = rbinom(N, 1, pselect)
	s[outside == 1] = 1  # Force outside units into RCT

	# Propensity scores
	ps.rct = 1/2
	ps = 1 / (1 + exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
	ps[s == 1] = 1/2
	ps.os = ps[s == 0]



	a=a+1
	print(a)

	# ## NEW
	z=rep(0,N) ## NEW
	z[s == 1 & outside == 1] = rbinom(sum(s == 1 & outside == 1), 1, ps[s == 1 & outside == 1])
	z[s == 1 & outside == 0] = rbinom(sum(s == 1 & outside == 0), 1, ps[s == 1 & outside == 0])
	z[sample(which(s == 0), prob = ps[s == 0])[1:(ceiling(.25 * sum(s == 0)))]] = 1

	# #around 25% T and 75% C in the OS. 

		  if (delta==0 | k=='all'){ 
			scale.factor=1
		  } else {
			scale.factor=sum(z==1 & s==0 & o==0)/sum(z[s==0])
			}
			
	a=a+1
	print(a)

		  y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(N)
		  y0[outside == 1] = y0[outside == 1] + intercept_shift
		  y1=y0+delta*o/scale.factor

		  true.value=mean((y1-y0)[s==0 & z==1])
		  #res=rbind(res,c(k,delta,gamma,scale.factor,true.value))


	a=a+1
	print(a)

		print(c(k,delta,gamma,scale.factor,true.value))


	path = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap_v4_RCToutsideOS.csv')
	write.csv(c(k,delta,gamma,scale.factor,true.value), path)
}


nb=25 #100

f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4.csv\')')))
eval(parse(text=paste0('scale.factor=as.numeric(',fv,'[4,2])')))
eval(parse(text=paste0('true.value=as.numeric(',fv,'[5,2])')))

######################################################################
################################################ Proposed method
#######################################################################

t=proc.time()
results=NULL
for(itr in 1:nb){
	## what do do with case ? #
	x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
	u=runif(n)

	if (k=='all') o=rep(1,n)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)


	pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
	s=rbinom(n,1,pselect) #around 1/6 rct & 5/6 os
	ps.rct=1/2
	ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
	ps[s==1]=1/2
	ps.os=ps[s==0]
	#z = rbinom(n,1,ps)
	## NEW
	z=rep(0,n) ## NEW
	z[s==1]=rbinom(sum(s==1),1,ps[s==1]) #around 25% T and 75% C in the OS. 
	z[sample(which(s==0),prob=ps[s==0])[1:(ceiling(.25*sum(s==0)))]]=1 
	#around 25% T and 75% C in the OS. 

	y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
	y1=y0+delta*o/scale.factor
	y=y1*z+y0*(1-z)


	if (sum(s)>0){
	drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
				   x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
				   overlap=rep(1,sum(s==1)))
	}

	oos=o[s==0]
	dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
				 x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
				 overlap=oos)
	zrt=drt$z
	zos=dos$z
	vars = c('x1','x2','x3','x4','x5')
	psd=rep(0,nvar)
	for (i in 1:nvar){
	xi=dos[,vars[i]]
	xit=xi[zos==1]
	xic=xi[zos==0]
	psd[i]=sqrt((var(xit)+var(xic))/2)
	}
	res = bridging2(drt,dos,zrt,zos,oos,vars,method='randomForest')
	match=res$match
	dfull=res$dfull
	match=match[dfull[match[,1],'imagineix']==0,]

	#inference
	res.infi=rep(NA,9)
	#rct
	## delta is the sensitivity parameter for generalizability bias; 0 is no bias
	# find confidence limit for 95% upper sided interval [CL, inf)
	ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
	matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
	tab=table(matchix)
	copy=numeric(length(res$copy))
	copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
	wt = copy/sum(copy)*nrow(drt)
	yrct=y[s==1]
	xx=x[s==1,]
	drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
	ddrt=data.frame(Z=zrt,Y=drt$res)
	## weighted outcomes
	ddrt$Ywt = ddrt$Y*wt

	res.infi[1]=uniroot(foo_rct2, c(-50,50), data=ddrt, delta=0, alpha=0.025, ri=T, nsim=10^3)$root		

	res.infi[2]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=0, alpha=0.025, ri=T, nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
	res.infi[3]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=0, alpha=0.025, ri=T, nsim=10^3)$root

	#os
	st=rep(1:nrow(match),2)
	xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
	resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
	y1=resy[1:nrow(match)]
	y0=resy[(nrow(match)+1):(2*nrow(match))]
	ymat = cbind(y1,y0)

	res.infi[4]=-uniroot(foo_os2, c(-2,2), ymat=-ymat, gamma=1, alpha=0.5)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[5]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=1, alpha=0.025)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[6]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=1, alpha=0.025)$root

	#combined

	res.infi[7]=uniroot(foo_combined2, c(-2,2), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.5, ri=T,nsim=10^3)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[8]=uniroot(foo_combined, c(-10,10), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.025, ri=T,nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[9]=-uniroot(foo_combined, c(-10,10), data=ddrt1, delta=0, ymat=-ymat, gamma=1, alpha=0.025, ri=T,nsim=10^3)$root


	results = rbind(results, c(res.infi))

}
print(proc.time()-t)
## Always shut the cluster down when done
#stopCluster(cl)

path=paste('results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v4.csv',sep='')
write.csv(results,path)


results=NULL
for(itr in 1:nb){
	## what do do with case ? #
	x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
	u=runif(n)

	if (k=='all') o=rep(1,n)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)


	pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
	s=rbinom(n,1,pselect) #around 1/6 rct & 5/6 os
	ps.rct=1/2
	ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
	ps[s==1]=1/2
	ps.os=ps[s==0]
	#z = rbinom(n,1,ps)
	## NEW
	z=rep(0,n) ## NEW
	z[s==1]=rbinom(sum(s==1),1,ps[s==1]) #around 25% T and 75% C in the OS. 
	z[sample(which(s==0),prob=ps[s==0])[1:(ceiling(.25*sum(s==0)))]]=1 
	#around 25% T and 75% C in the OS. 


	y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)

	y1=y0+delta*o/scale.factor
	y=y1*z+y0*(1-z)


	if (sum(s)>0){
	drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
				   x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
				   overlap=rep(1,sum(s==1)))
	}

	oos=o[s==0]
	dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
				 x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
				 overlap=oos)
	zrt=drt$z
	zos=dos$z
	vars = c('x1','x2','x3','x4','x5')
	psd=rep(0,nvar)
	for (i in 1:nvar){
	xi=dos[,vars[i]]
	xit=xi[zos==1]
	xic=xi[zos==0]
	psd[i]=sqrt((var(xit)+var(xic))/2)
	}
	res = bridging2(drt,dos,zrt,zos,oos,vars,method='randomForest')
	match=res$match
	dfull=res$dfull
	match=match[dfull[match[,1],'imagineix']==0,]

	#inference
	res.infi=rep(NA,9)
	#rct
	## delta is the sensitivity parameter for generalizability bias; 0 is no bias
	# find confidence limit for 95% upper sided interval [CL, inf)
	ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
	matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
	tab=table(matchix)
	copy=numeric(length(res$copy))
	copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
	wt = copy/sum(copy)*nrow(drt)
	yrct=y[s==1]
	xx=x[s==1,]
	drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
	ddrt=data.frame(Z=zrt,Y=drt$res)
	## weighted outcomes
	ddrt$Ywt = ddrt$Y*wt

	res.infi[1]=uniroot(foo_rct2, c(-50,50), data=ddrt, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root		

	res.infi[2]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
	res.infi[3]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root

	#os
	st=rep(1:nrow(match),2)
	xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
	resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
	y1=resy[1:nrow(match)]
	y0=resy[(nrow(match)+1):(2*nrow(match))]
	ymat = cbind(y1,y0)

	res.infi[4]=-uniroot(foo_os2, c(-2,2), ymat=-ymat, gamma=gamma, alpha=0.5)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[5]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=gamma, alpha=0.025)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[6]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=gamma, alpha=0.025)$root

	#combined

	res.infi[7]=uniroot(foo_combined2, c(-2,2), data=ddrt, delta=delta, ymat=ymat, gamma=gamma, alpha=0.5, ri=T,nsim=10^3)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[8]=uniroot(foo_combined, c(-10,10), data=ddrt, delta=delta, ymat=ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[9]=-uniroot(foo_combined, c(-10,10), data=ddrt1, delta=0, ymat=-ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root


	results = rbind(results, c(res.infi))

}

path=paste('true.results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v4.csv',sep='')
write.csv(results,path)



##############################################################


library(mvtnorm)

options(show.error.messages = TRUE)

source('CombineMatching.R')
source('CombineInference.R')

intercept_shift=2


olist=2#1:3#c('all','majority','limited')
# dlist=c(0, .05, .1)
# glist=c(1, 1.5, 2)

nlist = 1000 #c(500, 750)

 
seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)
dlist = c(0,.2,.5)
glist = c(1,1.2,1.5)


foo <- function(x, y) c(x, y)


o_d_seq = outer(olist, dlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_seq = outer(o_d_seq, glist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_n_seq = outer(o_d_g_seq, nlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = outer(o_d_g_n_seq, seedseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = sapply(setup, cbind)

nvar=5
nb=20 #100

k = c('all','majority','limited')
k = k[setup[1,id]]
delta = setup[2,id]
gamma = setup[3,id]
n = setup[4,id]
thisseed = setup[5,id]

set.seed(thisseed)


print(c(k,delta,gamma))

nvar=5


# # print(c(k,delta,gamma,scale.factor,true.value))

f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4_RCToutsideOS.csv\')')))
eval(parse(text=paste0('scale.factor=as.numeric(',fv,'[4,2])')))
eval(parse(text=paste0('true.value=as.numeric(',fv,'[5,2])')))

 t=proc.time()
results=NULL
for(itr in 1:nb){
	## what do do with case ? #
			x = mvtnorm::rmvnorm(n, mean = rep(0, nvar))
	u = runif(n)

	if (k=='all') o=rep(1,n)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)

	# New: define outside-support units
	outside = rbinom(n, 1, 0.1)  # ~10% flagged as outside support

	# Selection into RCT or OS
	pselect = 1 / (1 + exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4]))) * o
	s = rbinom(n, 1, pselect)
	s[outside == 1] = 1  # Force outside units into RCT

	# Propensity scores
	ps.rct = 1/2
	ps = 1 / (1 + exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
	ps[s == 1] = 1/2
	ps.os = ps[s == 0]

	# Treatment assignment
	z = rep(0, n)
	z[s == 1 & outside == 1] = rbinom(sum(s == 1 & outside == 1), 1, ps[s == 1 & outside == 1])
	z[s == 1 & outside == 0] = rbinom(sum(s == 1 & outside == 0), 1, ps[s == 1 & outside == 0])
	z[sample(which(s == 0), prob = ps[s == 0])[1:(ceiling(.25 * sum(s == 0)))]] = 1

	y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
	y0[outside == 1] = y0[outside == 1] + intercept_shift
	y1=y0+delta*o/scale.factor
	y=y1*z+y0*(1-z)


	if (sum(s)>0){
	drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
				   x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
				   overlap=rep(1,sum(s==1)))
	}

	oos=o[s==0]
	dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
				 x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
				 overlap=oos)
	zrt=drt$z
	zos=dos$z
	vars = c('x1','x2','x3','x4','x5')
	psd=rep(0,nvar)
	for (i in 1:nvar){
	xi=dos[,vars[i]]
	xit=xi[zos==1]
	xic=xi[zos==0]
	psd[i]=sqrt((var(xit)+var(xic))/2)
	}
	res = bridging2(drt,dos,zrt,zos,oos,vars,method='randomForest')
	match=res$match
	dfull=res$dfull
	match=match[dfull[match[,1],'imagineix']==0,]

	#inference
	res.infi=rep(NA,9)
	#rct
	## delta is the sensitivity parameter for generalizability bias; 0 is no bias
	# find confidence limit for 95% upper sided interval [CL, inf)
	ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
	matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
	tab=table(matchix)
	copy=numeric(length(res$copy))
	copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
	wt = copy/sum(copy)*nrow(drt)
	yrct=y[s==1]
	xx=x[s==1,]
	drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
	ddrt=data.frame(Z=zrt,Y=drt$res)
	## weighted outcomes
	ddrt$Ywt = ddrt$Y*wt

	res.infi[1]=uniroot(foo_rct2, c(-50,50), data=ddrt, delta=0, alpha=0.025, ri=T, nsim=10^3)$root		

	res.infi[2]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=0, alpha=0.025, ri=T, nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
	res.infi[3]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=0, alpha=0.025, ri=T, nsim=10^3)$root

	#os
	st=rep(1:nrow(match),2)
	xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
	resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
	y1=resy[1:nrow(match)]
	y0=resy[(nrow(match)+1):(2*nrow(match))]
	ymat = cbind(y1,y0)

	res.infi[4]=-uniroot(foo_os2, c(-2,2), ymat=-ymat, gamma=1, alpha=0.5)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[5]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=1, alpha=0.025)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[6]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=1, alpha=0.025)$root

	#combined

	res.infi[7]=uniroot(foo_combined2, c(-2,2), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.5, ri=T,nsim=10^3)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[8]=uniroot(foo_combined, c(-10,10), data=ddrt, delta=0, ymat=ymat, gamma=1, alpha=0.025, ri=T,nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[9]=-uniroot(foo_combined, c(-10,10), data=ddrt1, delta=0, ymat=-ymat, gamma=1, alpha=0.025, ri=T,nsim=10^3)$root


	results = rbind(results, c(res.infi))

}
print(proc.time()-t)
## Always shut the cluster down when done
#stopCluster(cl)

path=paste('results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v5_RCToutsideOS.csv',sep='')
write.csv(results,path)


results=NULL
for(itr in 1:nb){
	## what do do with case ? #
	x = mvtnorm::rmvnorm(n, mean = rep(0, nvar))
	u = runif(n)

	if (k=='all') o=rep(1,n)
	if (k=='majority') o=as.numeric(x[,1]>=-1)
	if (k=='limited') o=as.numeric(x[,1]>=0)

	# New: define outside-support units
	outside = rbinom(n, 1, 0.1)  # ~10% flagged as outside support

	# Selection into RCT or OS
	pselect = 1 / (1 + exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4]))) * o
	s = rbinom(n, 1, pselect)
	s[outside == 1] = 1  # Force outside units into RCT

	# Propensity scores
	ps.rct = 1/2
	ps = 1 / (1 + exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
	ps[s == 1] = 1/2
	ps.os = ps[s == 0]

	# Treatment assignment
	z = rep(0, n)
	z[s == 1 & outside == 1] = rbinom(sum(s == 1 & outside == 1), 1, ps[s == 1 & outside == 1])
	z[s == 1 & outside == 0] = rbinom(sum(s == 1 & outside == 0), 1, ps[s == 1 & outside == 0])
	z[sample(which(s == 0), prob = ps[s == 0])[1:(ceiling(.25 * sum(s == 0)))]] = 1


	#        scale.factor=as.numeric(keyvalues[ix,5])
	#        y0=1+3*x[,1]-2*x[,2]+u
	#        y1=y0+delta*o/scale.factor
	y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
	y0[outside == 1] = y0[outside == 1] + intercept_shift
	y1=y0+delta*o/scale.factor
	y=y1*z+y0*(1-z)



	if (sum(s)>0){
	drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
				   x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
				   overlap=rep(1,sum(s==1)))
	}

	oos=o[s==0]
	dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
				 x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
				 overlap=oos)
	zrt=drt$z
	zos=dos$z
	vars = c('x1','x2','x3','x4','x5')
	psd=rep(0,nvar)
	for (i in 1:nvar){
	xi=dos[,vars[i]]
	xit=xi[zos==1]
	xic=xi[zos==0]
	psd[i]=sqrt((var(xit)+var(xic))/2)
	}
	res = bridging2(drt,dos,zrt,zos,oos,vars,method='randomForest')
	match=res$match
	dfull=res$dfull
	match=match[dfull[match[,1],'imagineix']==0,]

	#inference
	res.infi=rep(NA,9)
	#rct
	## delta is the sensitivity parameter for generalizability bias; 0 is no bias
	# find confidence limit for 95% upper sided interval [CL, inf)
	ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
	matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
	tab=table(matchix)
	copy=numeric(length(res$copy))
	copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
	wt = copy/sum(copy)*nrow(drt)
	yrct=y[s==1]
	xx=x[s==1,]
	drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
	ddrt=data.frame(Z=zrt,Y=drt$res)
	## weighted outcomes
	ddrt$Ywt = ddrt$Y*wt

	res.infi[1]=uniroot(foo_rct2, c(-50,50), data=ddrt, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root		

	res.infi[2]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
	res.infi[3]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root

	#os
	st=rep(1:nrow(match),2)
	xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
	resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
	y1=resy[1:nrow(match)]
	y0=resy[(nrow(match)+1):(2*nrow(match))]
	ymat = cbind(y1,y0)

	res.infi[4]=-uniroot(foo_os2, c(-2,2), ymat=-ymat, gamma=gamma, alpha=0.5)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[5]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=gamma, alpha=0.025)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[6]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=gamma, alpha=0.025)$root

	#combined

	res.infi[7]=uniroot(foo_combined2, c(-2,2), data=ddrt, delta=delta, ymat=ymat, gamma=gamma, alpha=0.5, ri=T,nsim=10^3)$root

	# find confidence limit for 95% upper sided interval [CL, inf)
	res.infi[8]=uniroot(foo_combined, c(-10,10), data=ddrt, delta=delta, ymat=ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root
	# find confidence limit for 95% lower sided interval (-inf, CL]
	res.infi[9]=-uniroot(foo_combined, c(-10,10), data=ddrt1, delta=0, ymat=-ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root


	results = rbind(results, c(res.infi))

}

path=paste('true.results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v5_RCToutsideOS.csv',sep='')
write.csv(results,path)
#}


######################################################## Other methods: Elastic and R-Learner


keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)#+1
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)
deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


s_d_seq = outer(seedseq, deltaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
s_d_g_seq = outer(s_d_seq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup = sapply(s_d_g_seq, cbind)

 n=1000
 k='majority'
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]
 set.seed(thisseed)
 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)

library(ElasticIntegrative)
	  
library(SuperLearner)

library(glmnet)
library(mvtnorm)

library(stringr)
library(caret)
library(ncvreg)

source('intRlearner.R')
source('utils.R')		  

res = NULL

 for(itr in 1:5){
          x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
          u=rnorm(n)
          
          if (k=='all') o=rep(1,n)
          if (k=='majority') o=as.numeric(x[,1]>=-1)
          if (k=='limited') o=as.numeric(x[,1]>=0)
          
          pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
          s=rbinom(n,1,pselect) #around 1/7 RCT & 6/7 OS
          ps.rct=1/2
          ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
          ps[s==1]=ps.rct
          ps.os=ps[s==0]
		  
		## NEW
		z=rep(0,n) ## NEW
		z[s==1]=rbinom(sum(s==1),1,ps[s==1]) #around 25% T and 75% C in the OS. 
		z[sample(which(s==0),prob=ps[s==0])[1:(ceiling(.25*sum(s==0)))]]=1 
		#around 25% T and 75% C in the OS. 
          
          scale.factor=as.numeric(keyvalues[ix,5])
          y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
          y1=y0+delta*o/scale.factor
          y=y1*z+y0*(1-z)
          
          if (sum(s)>0){
            drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
                           x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
                           overlap=rep(1,sum(s==1)))
          }
          yrct=y[s==1]
          xx=x[s==1,]
          drt$res = MASS::rlm(yrct ~ xx)$residuals
          oos=o[s==0]
          dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
                         x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
                         overlap=oos)
          yos=y[s==0]
          xx=x[s==0,]
          dos$res = MASS::rlm(yos ~ xx)$residuals
          zrt=drt$z
          zos=dos$z
          vars = c('x1','x2','x3','x4','x5')
		  
		  dat.t <- data.frame(
		  Y = yrct, A = z[s==1], q = rep(1, length(yrct)),
		  ps = ps.rct,
		  ml.ps = ps.rct
		)
		dat.t$X1 = x[s==1,1]; dat.t$X2 = x[s==1,2]
		dat.t$X3 = x[s==1,3]; dat.t$X4 = x[s==1,4]; dat.t$X5 = x[s==1,5]

		dat.os <- data.frame(
		  Y = yos, A = zos, q = rep(1, length(yos))
		  )
		dat.os$X1 = x[s==0,1]; dat.os$X2 = x[s==0,2]
		dat.os$X3 = x[s==0,3]; dat.os$X4 = x[s==0,4]; dat.os$X5 = x[s==0,5]


  
		result.elastic <- elasticHTE(mainName=c("X1","X2","X3","X4","X5"),
							  contName=c("X1","X2","X3","X4","X5"),
							  propenName=c("X1","X2","X3","X4","X5"),
							  dat.t = dat.t,
                             dat.os = dat.os,
                             thres.psi = sqrt(log(length(yos))), 
							 # threshold for ACI psi
                             fixed = FALSE)
				

		  
	  x_rct <- poly_interaction(x[s==1,], interact = T)
	  x_rwe <- poly_interaction(x[s==0,], interact = T)
	  x_test <- poly_interaction(x[s==0,], interact = T)
	  newx_rlearner <- matrix(x_test, ncol=ncol(x_test))
	 
	   y_rct = yrct
	   a_rct = z[s==1]
	   
	   y_rwe = yos
	   a_rwe = z[s==0]
	   p=5
	   m=length(yos)
		########################################

		# R-learner method with RCT
		rlasso_fit_rct <- rlasso(x=x_rct, w=a_rct, y=y_rct, p=p)
		rlasso_est_rct <- predict.rlasso(rlasso_fit_rct, newx_rlearner)
		# R-learner method with RWE
		rlasso_fit_rwe <- rlasso(x=x_rwe, w=a_rwe, y=y_rwe, p=p)
		rlasso_est_rwe <- predict.rlasso(rlasso_fit_rwe, newx_rlearner)

		# R-learner for the combined data
		rlasso_fit_cmb <- intrlearner(x=rbind(x_rct, x_rwe), w=c(a_rct, a_rwe),
									  y=c(y_rct, y_rwe), s=c(rep(1, length(a_rct)), rep(0, m)), p=p)
		rlasso_est_cmb <- predict.rlasso(rlasso_fit_cmb, newx_rlearner)
		#cmb_rlasso <- sqrt(mean((rlasso_est_cmb - tau)**2))


		## Estimate
		est = mean(rlasso_est_cmb)
		
		#### Bootstrap ####
		best = NULL
		for(i in 1:150){
			resampler = sample(1:nrow(x_rct),replace=T)
			resamplewe = sample(1:nrow(x_rwe),replace=T)
			
			x_rct1 = x_rct[resampler,]
			y_rct1 = y_rct[resampler]
			a_rct1 = a_rct[resampler]
			
			x_rwe1 = x_rwe[resamplewe,]
			y_rwe1 = y_rwe[resamplewe]
			a_rwe1 = a_rwe[resamplewe]
			
			
			# R-learner method with RCT
			rlasso_fit_rct <- rlasso(x=x_rct1, w=a_rct1, y=y_rct1, p=p)
			rlasso_est_rct <- predict.rlasso(rlasso_fit_rct, newx_rlearner)
			# R-learner method with RWE
			rlasso_fit_rwe <- rlasso(x=x_rwe1, w=a_rwe1, y=y_rwe1, p=p)
			rlasso_est_rwe <- predict.rlasso(rlasso_fit_rwe, newx_rlearner)

			# R-learner for the combined data
			rlasso_fit_cmb <- intrlearner(x=rbind(x_rct1, x_rwe1), w=c(a_rct1, a_rwe1),
										  y=c(y_rct1, y_rwe1), s=c(rep(1, length(a_rct1)), rep(0, m)), p=p)
			rlasso_est_cmb <- predict.rlasso(rlasso_fit_cmb, newx_rlearner)
			#cmb_rlasso <- sqrt(mean((rlasso_est_cmb - tau)**2))

			best = c(best, mean(rlasso_est_cmb))
		}
		
		
		res = rbind(res, c(result.elastic$est,
							result.elastic$ve,
							result.elastic$CIs.inf,
							result.elastic$CIs.sup,
							est, 
							quantile(best, c(.025, .975))))
}
		  
		  
path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3.csv',sep='')

write.csv(res,path)		  
		
######################################################## Other methods: Elastic and R-Learner
########	RCT outside OS

intercept_shift = 2
keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)#+1
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)
deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


s_d_seq = outer(seedseq, deltaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
s_d_g_seq = outer(s_d_seq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup = sapply(s_d_g_seq, cbind)

 n=1000
 k='majority'
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]
 set.seed(thisseed)
 
 #delta=0.2
 #gamma=1.2
 nvar=5
 

library(ElasticIntegrative)
	  
library(SuperLearner)

library(glmnet)
library(mvtnorm)

library(stringr)
library(caret)
library(ncvreg)

source('intRlearner.R')
source('utils.R')		  

res = NULL

 for(itr in 1:2){
          x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
          u=rnorm(n)
          
          if (k=='all') o=rep(1,n)
          if (k=='majority') o=as.numeric(x[,1]>=-1)
          if (k=='limited') o=as.numeric(x[,1]>=0)
          
         outside = rbinom(n, 1, 0.1)  # ~10% flagged as outside support

		# Selection into RCT or OS
		pselect = 1 / (1 + exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4]))) * o
		s = rbinom(n, 1, pselect)
		s[outside == 1] = 1  # Force outside units into RCT

		# Propensity scores
		ps.rct = 1/2
		ps = 1 / (1 + exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
		ps[s == 1] = 1/2
		ps.os = ps[s == 0]

		# Treatment assignment
		z = rep(0, n)
		z[s == 1 & outside == 1] = rbinom(sum(s == 1 & outside == 1), 1, ps[s == 1 & outside == 1])
		z[s == 1 & outside == 0] = rbinom(sum(s == 1 & outside == 0), 1, ps[s == 1 & outside == 0])
		z[sample(which(s == 0), prob = ps[s == 0])[1:(ceiling(.25 * sum(s == 0)))]] = 1

			  
		scale.factor=as.numeric(keyvalues[ix,5])
		#        y0=1+3*x[,1]-2*x[,2]+u
		 y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
		        y1=y0+delta*o/scale.factor
		#		y0=1+x[,1]-2*x[,2]+u
				y0[outside == 1] = y0[outside == 1] + intercept_shift
				y1=y0+delta*o/scale.factor
				y=y1*z+y0*(1-z)
				
		#        if (sum(s)>0)
				  drt=data.frame(z=z[s==1],y=y[s==1],
								x1=x[s==1,1],x2=x[s==1,2],
								x6=outside[s==1],
								 u=u[s==1])
				yrct=y[s==1]
				xx=cbind(x[s==1,], outside[s==1])
          drt$res = MASS::rlm(yrct ~ xx)$residuals
          oos=o[s==0]
          dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
                         x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
                         overlap=oos)
          yos=y[s==0]
          xx=x[s==0,]
          dos$res = MASS::rlm(yos ~ xx)$residuals
          zrt=drt$z
          zos=dos$z
          vars = c('x1','x2','x3','x4','x5')
		  
		dat.t <- data.frame(
		  Y = yrct, A = z[s==1], q = rep(1, length(yrct)),
		  ps = ps.rct,
		  ml.ps = ps.rct
		)
		dat.t$X1 = x[s==1,1]; dat.t$X2 = x[s==1,2]
		dat.t$X3 = x[s==1,3]; dat.t$X4 = x[s==1,4]; dat.t$X5 = x[s==1,5]; dat.t$X6 = outside[s==1]+runif(sum(s==1),0,.1)

		dat.os <- data.frame(
		  Y = yos, A = zos, q = rep(1, length(yos))
		  )
		dat.os$X1 = x[s==0,1]; dat.os$X2 = x[s==0,2]
		dat.os$X3 = x[s==0,3]; dat.os$X4 = x[s==0,4]; dat.os$X5 = x[s==0,5]; dat.os$X6 = runif(sum(s==0))


  
		result.elastic <- elasticHTE(mainName=c("X1","X2","X3","X4","X5","X6"),
							  contName=c("X1","X2","X3","X4","X5"),
							  propenName=c("X1","X2","X3","X4","X5","X6"),
							  dat.t = dat.t,
                             dat.os = dat.os,
                             thres.psi = sqrt(log(length(yos))), 
							 # threshold for ACI psi
                             fixed = FALSE)
				

		  
	  x_rct <- poly_interaction(cbind(x[s==1,], outside[s==1]+runif(sum(s==1),0,.1)), interact = T)
	  x_rwe <- poly_interaction(cbind(x[s==0,], outside[s==0]+runif(sum(s==0),0,.1)), interact = T)
	  x_test <- poly_interaction(cbind(x[s==0,], outside[s==0]+runif(sum(s==0),0,.1)), interact = T)
	  newx_rlearner <- matrix(x_test, ncol=ncol(x_test))
	 
	   y_rct = yrct
	   a_rct = z[s==1]
	   
	   y_rwe = yos
	   a_rwe = z[s==0]
	   p=5
	   m=length(yos)
		########################################

		# R-learner method with RCT
		rlasso_fit_rct <- rlasso(x=x_rct, w=a_rct, y=y_rct, p=p)
		rlasso_est_rct <- predict.rlasso(rlasso_fit_rct, newx_rlearner)
		# R-learner method with RWE
		rlasso_fit_rwe <- rlasso(x=x_rwe, w=a_rwe, y=y_rwe, p=p)
		rlasso_est_rwe <- predict.rlasso(rlasso_fit_rwe, newx_rlearner)

		# R-learner for the combined data
		rlasso_fit_cmb <- intrlearner(x=rbind(x_rct, x_rwe), w=c(a_rct, a_rwe),
									  y=c(y_rct, y_rwe), s=c(rep(1, length(a_rct)), rep(0, m)), p=p)
		rlasso_est_cmb <- predict.rlasso(rlasso_fit_cmb, newx_rlearner)
		#cmb_rlasso <- sqrt(mean((rlasso_est_cmb - tau)**2))


		## Estimate
		est = mean(rlasso_est_cmb)
		
		#### Bootstrap ####
		best = NULL
		for(i in 1:150){
			resampler = sample(1:nrow(x_rct),replace=T)
			resamplewe = sample(1:nrow(x_rwe),replace=T)
			
			x_rct1 = x_rct[resampler,]
			y_rct1 = y_rct[resampler]
			a_rct1 = a_rct[resampler]
			
			x_rwe1 = x_rwe[resamplewe,]
			y_rwe1 = y_rwe[resamplewe]
			a_rwe1 = a_rwe[resamplewe]
			
			
			# R-learner method with RCT
			rlasso_fit_rct <- rlasso(x=x_rct1, w=a_rct1, y=y_rct1, p=p)
			rlasso_est_rct <- predict.rlasso(rlasso_fit_rct, newx_rlearner)
			# R-learner method with RWE
			rlasso_fit_rwe <- rlasso(x=x_rwe1, w=a_rwe1, y=y_rwe1, p=p)
			rlasso_est_rwe <- predict.rlasso(rlasso_fit_rwe, newx_rlearner)

			# R-learner for the combined data
			rlasso_fit_cmb <- intrlearner(x=rbind(x_rct1, x_rwe1), w=c(a_rct1, a_rwe1),
										  y=c(y_rct1, y_rwe1), s=c(rep(1, length(a_rct1)), rep(0, m)), p=p)
			rlasso_est_cmb <- predict.rlasso(rlasso_fit_cmb, newx_rlearner)
			#cmb_rlasso <- sqrt(mean((rlasso_est_cmb - tau)**2))

			best = c(best, mean(rlasso_est_cmb))
		}
		
		
		res = rbind(res, c(result.elastic$est,
							result.elastic$ve,
							result.elastic$CIs.inf,
							result.elastic$CIs.sup,
							est, 
							quantile(best, c(.025, .975))))
}
		  
		  
path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_RCToutsideOS.csv',sep='')

write.csv(res,path)		  
		  
	
############################################## classical_methods: Weighting and PS


keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)#+1
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)


#seedseq = 1
deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


s_d_seq = outer(seedseq, deltaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
s_d_g_seq = outer(s_d_seq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup = sapply(s_d_g_seq, cbind)

#for(id in 1:ncol(setup)){
 n=1000
 k='majority'
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]
 set.seed(thisseed)

 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)

# Load necessary libraries
library(MatchIt)
library(nloptr)
library(randomForest)


res = NULL

 for(itr in 1:4){
          x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
          u=rnorm(n)
          
          if (k=='all') o=rep(1,n)
          if (k=='majority') o=as.numeric(x[,1]>=-1)
          if (k=='limited') o=as.numeric(x[,1]>=0)
          
          pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
          s=rbinom(n,1,pselect) #around 1/7 RCT & 6/7 OS
          ps.rct=1/2
          ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
          ps[s==1]=ps.rct
          ps.os=ps[s==0]
          z=rbinom(n,1,ps) #around 25% T and 75% C in the OS
          
          scale.factor=as.numeric(keyvalues[ix,5])
          y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
          y1=y0+delta*o/scale.factor
          y=y1*z+y0*(1-z)
          
          if (sum(s)>0){
            drt=data.frame(z=z[s==1],y=y[s==1],u=u[s==1],
                           x1=x[s==1,1],x2=x[s==1,2],x3=x[s==1,3],x4=x[s==1,4],x5=x[s==1,5],
                           overlap=rep(1,sum(s==1)))
          }
          yrct=y[s==1]
          xx=x[s==1,]
          drt$res = MASS::rlm(yrct ~ xx)$residuals
          oos=o[s==0]
          dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
                         x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
                         overlap=oos)
          yos=y[s==0]
          xx=x[s==0,]
          dos$res = MASS::rlm(yos ~ xx)$residuals
          zrt=drt$z
          zos=dos$z
          vars = c('x1','x2','x3','x4','x5')
		  
		  dat.t <- data.frame(
		  Y = yrct, A = z[s==1], q = rep(1, length(yrct)),
		  ps = ps.rct,
		  ml.ps = ps.rct
		)
		dat.t$X1 = x[s==1,1]; dat.t$X2 = x[s==1,2]
		dat.t$X3 = x[s==1,3]; dat.t$X4 = x[s==1,4]; dat.t$X5 = x[s==1,5]

		dat.os <- data.frame(
		  Y = yos, A = zos, q = rep(1, length(yos))
		  )
		dat.os$X1 = x[s==0,1]; dat.os$X2 = x[s==0,2]
		dat.os$X3 = x[s==0,3]; dat.os$X4 = x[s==0,4]; dat.os$X5 = x[s==0,5]


	source('classical_methods_rough_code.R')
	est_weightingbased0 = est_weightingbased
	est_psbased0 = est_psbased
	
	dat.t0 = dat.t
	dat.os0 = dat.os
	est_weightingbased_boot = est_psbased_boot = NULL
	for(bitr in 1:150){
		dat.t = dat.t0[sample(1:nrow(dat.t0), replace=TRUE),]
		dat.os = dat.os0[sample(1:nrow(dat.os0), replace=TRUE),]
		source('classical_methods_rough_code.R')
		est_weightingbased_boot = c(est_weightingbased_boot, est_weightingbased)
		est_psbased_boot = c(est_psbased_boot, est_psbased)
		
	}

	res = rbind(res,c(est_weightingbased, est_psbased, 
					quantile(est_weightingbased_boot, c(.025, .975)),
					quantile(est_psbased_boot, c(.025, .975))
					))
	print(itr)
}	

path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_classicmethods.csv',sep='')

write.csv(res,path)		

######################################################## Other methods: Elastic and R-Learner
############ RCT outside OS


intercept_shift = 2
keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)#+1
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)

#seedseq = 1
deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


s_d_seq = outer(seedseq, deltaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
s_d_g_seq = outer(s_d_seq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup = sapply(s_d_g_seq, cbind)

#for(id in 1:ncol(setup)){
 n=1000
 k='majority'
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]
 set.seed(thisseed)
 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)

# Load necessary libraries
library(MatchIt)
library(nloptr)
library(randomForest)

res = NULL

 for(itr in 1:4){
          x=mvtnorm::rmvnorm(n,mean=rep(0,nvar))
          u=rnorm(n)
          
          if (k=='all') o=rep(1,n)
          if (k=='majority') o=as.numeric(x[,1]>=-1)
          if (k=='limited') o=as.numeric(x[,1]>=0)
          
         outside = rbinom(n, 1, 0.1)  # ~10% flagged as outside support

		# Selection into RCT or OS
		pselect = 1 / (1 + exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4]))) * o
		s = rbinom(n, 1, pselect)
		s[outside == 1] = 1  # Force outside units into RCT

		# Propensity scores
		ps.rct = 1/2
		ps = 1 / (1 + exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
		ps[s == 1] = 1/2
		ps.os = ps[s == 0]

		# Treatment assignment
		z = rep(0, n)
		z[s == 1 & outside == 1] = rbinom(sum(s == 1 & outside == 1), 1, ps[s == 1 & outside == 1])
		z[s == 1 & outside == 0] = rbinom(sum(s == 1 & outside == 0), 1, ps[s == 1 & outside == 0])
		z[sample(which(s == 0), prob = ps[s == 0])[1:(ceiling(.25 * sum(s == 0)))]] = 1

			  
		scale.factor=as.numeric(keyvalues[ix,5])
		#        y0=1+3*x[,1]-2*x[,2]+u
		#        y1=y0+delta*o/scale.factor
				y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
				y0[outside == 1] = y0[outside == 1] + intercept_shift
				y1=y0+delta*o/scale.factor
				y=y1*z+y0*(1-z)
				
		#        if (sum(s)>0)
				  drt=data.frame(z=z[s==1],y=y[s==1],
								x1=x[s==1,1],x2=x[s==1,2],
								x6=outside[s==1],
								 u=u[s==1])
				yrct=y[s==1]
				xx=cbind(x[s==1,], outside[s==1])
          drt$res = MASS::rlm(yrct ~ xx)$residuals
          oos=o[s==0]
          dos=data.frame(z=z[s==0],y=y[s==0],u=u[s==0],
                         x1=x[s==0,1],x2=x[s==0,2],x3=x[s==0,3],x4=x[s==0,4],x5=x[s==0,5],
                         overlap=oos)
          yos=y[s==0]
          xx=x[s==0,]
          dos$res = MASS::rlm(yos ~ xx)$residuals
          zrt=drt$z
          zos=dos$z
          vars = c('x1','x2','x3','x4','x5')
		  
		dat.t <- data.frame(
		  Y = yrct, A = z[s==1], q = rep(1, length(yrct)),
		  ps = ps.rct,
		  ml.ps = ps.rct
		)
		dat.t$X1 = x[s==1,1]; dat.t$X2 = x[s==1,2]
		dat.t$X3 = x[s==1,3]; dat.t$X4 = x[s==1,4]; dat.t$X5 = x[s==1,5]; dat.t$X6 = outside[s==1]+runif(sum(s==1),0,.1)

		dat.os <- data.frame(
		  Y = yos, A = zos, q = rep(1, length(yos))
		  )
		dat.os$X1 = x[s==0,1]; dat.os$X2 = x[s==0,2]
		dat.os$X3 = x[s==0,3]; dat.os$X4 = x[s==0,4]; dat.os$X5 = x[s==0,5]; dat.os$X6 = runif(sum(s==0))
		source('classical_methods_rough_code.R')

	est_weightingbased0 = est_weightingbased
	est_psbased0 = est_psbased
	
	dat.t0 = dat.t
	dat.os0 = dat.os
	est_weightingbased_boot = est_psbased_boot = NULL
	for(bitr in 1:150){
		dat.t = dat.t0[sample(1:nrow(dat.t0), replace=TRUE),]
		dat.os = dat.os0[sample(1:nrow(dat.os0), replace=TRUE),]
		source('classical_methods_rough_code.R')
		est_weightingbased_boot = c(est_weightingbased_boot, est_weightingbased)
		est_psbased_boot = c(est_psbased_boot, est_psbased)
		
	}

	res = rbind(res,c(est_weightingbased, est_psbased, 
					quantile(est_weightingbased_boot, c(.025, .975)),
					quantile(est_psbased_boot, c(.025, .975))
					))
	print(itr)
}	

path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_classicmethods_RCToutsideOS.csv',sep='')

write.csv(res,path)		