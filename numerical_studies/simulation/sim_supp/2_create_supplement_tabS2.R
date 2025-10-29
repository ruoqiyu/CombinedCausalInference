tab = NULL


#######################################################
########### RCT inside OS
#######################################################


####################################################### Proposed method
keyvalues = read.csv('keyvalues.csv')


### Gamma=1, delta=0

klist='majority' #c('all','majority','limited')
dlist=c(0,.2,.5)#c(0, .05, .1)
glist=c(1,1.2,1.5)#c(1, 1.5, 2)#c(1, 2, 2.5)

for(delta in dlist){

	for(gamma in glist){
		for(k in klist){
		
			f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4.csv\')[5,2]')))
			
		}
	}
}



olist=2

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


for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				eval(parse(text=paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap = NULL')))
									
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			
			}

for(id in 1:ncol(setup)){
	k = c('all','majority','limited')
	k = k[setup[1,id]]
	delta = setup[2,id]
	gamma = setup[3,id]
	n = setup[4,id]
	thisseed = setup[5,id]

	v= paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap')
	
	f = paste('results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v4.csv',sep='')
	if(file.exists(f)){
		res = read.csv(f)
		if(ncol(res)==10)
		eval(parse(text=paste(v,' = rbind(',v,', res)')))
	}
}
	



for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
							
				f = paste0('ci.n',n,'.delta',delta,'.gamma',gamma,
							'.',k,'overlap = n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,2:10]')
				eval(parse(text=f))
			}

for(n in nlist)
for(delta in dlist)
for(gamma in glist)
for(k in klist){
for (kk in 1:3){
  eval(parse(text=paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap=
		as.numeric(true.delta',delta,'.gamma',gamma,'.',k,'overlap)')))

  flength = paste0('ci.length.n',n,'.delta',delta,'.gamma',
				gamma,'.',k,'overlap=cbind(ci.length.n',n,
				'.delta',delta,'.gamma',gamma,'.',k,'overlap,
                        (ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]-ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]))')
						
   fcoverage = paste0('ci.coverage.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(ci.coverage.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]<=true.delta',delta,'.gamma',gamma,'.',k,'overlap) &
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]
					>=true.delta',delta,'.gamma',gamma,'.',k,'overlap))')

	fmse = paste0('mse.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(mse.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+1] - 
					true.delta',delta,'.gamma',gamma,'.',k,'overlap)^2)')
					
	eval(parse(text=fcoverage))
	eval(parse(text=flength))
	eval(parse(text=fmse))
}
}



for(n in nlist){
fvecNCov = fvec1NCov = fvec1NCovmse = c()


for(delta in dlist){
for(gamma in glist){


fNCov = 'c('
f1NCov = 'c('
f1NCovmse = 'c('
for(k in klist){
	
	fNCov = paste0(fNCov, 'apply(ci.coverage.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCov = paste0(f1NCov, 'apply(ci.length.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCovmse = paste0(f1NCovmse, 'apply(mse.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	if(k!=tail(klist,1)) {
		fNCov = paste(fNCov, ', ') 			   
		f1NCov = paste(f1NCov, ', ') 			   
		f1NCovmse = paste(f1NCovmse, ', ') 		
	}
	if(k==tail(klist,1)){
		fNCov = paste(fNCov, ')') 
		f1NCov = paste(f1NCov, ')') 
		f1NCovmse = paste(f1NCovmse, ')') 
	}
}


fvecNCov = c(fvecNCov, fNCov)
fvec1NCov = c(fvec1NCov, f1NCov)
fvec1NCovmse = c(fvec1NCovmse, f1NCovmse)

}
}


fvecNCov = paste(fvecNCov, collapse = ',')
fvec1NCov = paste(fvec1NCov, collapse = ',')
fvec1NCovmse = paste(fvec1NCovmse, collapse = ',')

eval(parse(text=paste0('tb.coverage',n,'NCov=rbind(',fvecNCov,')')))
eval(parse(text=paste0('tb.length',n,'NCov=rbind(',fvec1NCov,')')))
eval(parse(text=paste0('tb.mse',n,'NCov=rbind(',fvec1NCovmse,')')))


}

xtable::xtable(tb.mse1000NCov)
xtable::xtable(tb.coverage1000NCov)
xtable::xtable(tb.length1000NCov)

###################################################################
## Gamma = Gamma*, Delta=Delta*

options(show.error.messages = TRUE)


olist=2#c('all','majority','limited')
dlist=c(0,.2,.5)
glist=c(1,1.2,1.5)


klist='majority'

nlist = 1000 

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)


foo <- function(x, y) c(x, y)


o_d_seq = outer(olist, dlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_seq = outer(o_d_seq, glist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_n_seq = outer(o_d_g_seq, nlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = outer(o_d_g_n_seq, seedseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = sapply(setup, cbind)



for(delta in dlist){

	for(gamma in glist){
		for(k in klist){
		
			f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4.csv\')[5,2]')))
			
		}
	}
}


for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				eval(parse(text=paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap = NULL')))
									
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			}
			
			
for(id in 1:ncol(setup)){
	k = c('all','majority','limited')
	k = k[setup[1,id]]
	delta = setup[2,id]
	gamma = setup[3,id]
	n = setup[4,id]
	thisseed = setup[5,id]

	v= paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap')
	
	f = paste('true.results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v4.csv',sep='')
	if(file.exists(f)){
		res = read.csv(f)
		if(ncol(res)==10)
		eval(parse(text=paste(v,' = rbind(',v,', res)')))
	}
}
	
	

for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))			
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			
			
				f = paste0('ci.n',n,'.delta',delta,'.gamma',gamma,
							'.',k,'overlap = n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,2:10]')
				eval(parse(text=f))
			}



for(n in nlist)
for(delta in dlist)
for(gamma in glist)
for(k in klist){
for (kk in 1:3){
  eval(parse(text=paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap=
		as.numeric(true.delta',delta,'.gamma',gamma,'.',k,'overlap)')))

  flength = paste0('ci.length.n',n,'.delta',delta,'.gamma',
				gamma,'.',k,'overlap=cbind(ci.length.n',n,
				'.delta',delta,'.gamma',gamma,'.',k,'overlap,
                        (ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]-ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]))')
						
   fcoverage = paste0('ci.coverage.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(ci.coverage.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]<=true.delta',delta,'.gamma',gamma,'.',k,'overlap) &
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]
					>=true.delta',delta,'.gamma',gamma,'.',k,'overlap))')
					
	fmse = paste0('mse.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(mse.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+1] - 
					true.delta',delta,'.gamma',gamma,'.',k,'overlap)^2)')

	eval(parse(text=fcoverage))
	eval(parse(text=flength))
	eval(parse(text=fmse))
}
}



for(n in nlist){
fvecNCov = fvec1NCov = fvec1NCovmse = c()


for(delta in dlist){
for(gamma in glist){

fNCov = 'c('
f1NCov = 'c('
f1NCovmse = 'c('
for(k in klist){
		
	fNCov = paste0(fNCov, 'apply(ci.coverage.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCov = paste0(f1NCov, 'apply(ci.length.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCovmse = paste0(f1NCovmse, 'apply(mse.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	if(k!=tail(klist,1)) {
		fNCov = paste(fNCov, ', ') 			   
		f1NCov = paste(f1NCov, ', ') 			   
		f1NCovmse = paste(f1NCovmse, ', ') 		
	}
	if(k==tail(klist,1)){
		fNCov = paste(fNCov, ')') 
		f1NCov = paste(f1NCov, ')') 
		f1NCovmse = paste(f1NCovmse, ')') 
	}
}


fvecNCov = c(fvecNCov, fNCov)
fvec1NCov = c(fvec1NCov, f1NCov)
fvec1NCovmse = c(fvec1NCovmse, f1NCovmse)
}
}


fvecNCov = paste(fvecNCov, collapse = ',')
fvec1NCov = paste(fvec1NCov, collapse = ',')
fvec1NCovmse = paste(fvec1NCovmse, collapse = ',')
eval(parse(text=paste0('true.tb.coverage',n,'NCov=rbind(',fvecNCov,')')))
eval(parse(text=paste0('true.tb.length',n,'NCov=rbind(',fvec1NCov,')')))
eval(parse(text=paste0('true.tb.mse',n,'NCov=rbind(',fvec1NCovmse,')')))

}



tab = cbind(tab, cbind(c(true.tb.mse1000NCov[,3], true.tb.coverage1000NCov[,3]),
						c(tb.mse1000NCov[,3], tb.coverage1000NCov[,3])))
						
#################################################### Elastic and R Learner


keyvalues = read.csv('keyvalues.csv')

deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


d_g_seq = outer(deltaseq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup1 = sapply(d_g_seq, cbind)

 n=1000000
 k='majority'
truetau=NULL

for(id in 1:ncol(setup1)){
 delta=setup1[1,id]
 gamma=setup1[2,id]

 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)
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
          #y1=y0+delta*o/scale.factor
		  y1=y0+delta*o/scale.factor
          y=y1*z+y0*(1-z)

	truetau = rbind(truetau, 
		c(delta, gamma, mean((y1-y0)[z==1 & s==0]),mean((y1-y0)[s==1])))
}

###############################



keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
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

resCI = list()
for(id1 in 1:ncol(setup1))
	resCI[[id1]] = matrix(0, 0, 2+8)

for(id in 1:ncol(setup)){
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]
 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)


id1 = which(truetau[,1]==delta & truetau[,2]==gamma) 

path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3.csv',sep='')

if(file.exists(path)){
	res = read.csv(path)		  

	if(ncol(res)<157) next;

	tau1 = truetau[id1, 3]
	tau2 = truetau[id1, 4]		  

	intRerr1 = (res[,'X.1'] - tau1)^2
	intRcover1 = (res[,'X2.5.'] <= tau1) & (tau1 <= res[,'X97.5.'])
	
	intRerr2 = (res[,'X.1'] - tau2)^2
	intRcover2 = (res[,'X2.5.'] <= tau2) & (tau2 <= res[,'X97.5.'])
	
	elaserr1 = (res[,'elas.1'] - tau1)^2
	elaserr2 = (res[,'elas.1'] - tau2)^2

	elascover1 = (res[,'elas.1.2']<= tau1) & (tau1 <= res[,'elas.1.3'])
	elascover2 = (res[,'elas.1.2']<= tau2) & (tau2 <= res[,'elas.1.3'])


	resCI[[id1]] = rbind(resCI[[id1]],
			cbind(delta, gamma, 
				elaserr1, elaserr2,
				elascover1, elascover2,
				intRerr1, intRcover1,
				intRerr2, intRcover2))
						
}

}


temp = round(t(sapply(resCI, colMeans)), 3)[, c(1,2,3,5,7,8)]
temp = rbind(temp[order(temp[,1]), c(3,5)], temp[order(temp[,1]), c(4,6)])

tab = cbind(tab, temp)

############################################### Classic methods

keyvalues = read.csv('keyvalues.csv')
intercept_shift=2				 

deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


d_g_seq = outer(deltaseq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup1 = sapply(d_g_seq, cbind)

 n=1000000
 k='majority'
truetau=NULL

for(id in 1:ncol(setup1)){
 delta=setup1[1,id]
 gamma=setup1[2,id]

 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)
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
          
	rct_size = sum(s==1)


	truetau = rbind(truetau, 
		c(delta, gamma, mean((y1-y0)[z==1 & s==0]),mean((y1-y0)[s==1]), rct_size ))
}


deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


d_g_seq = outer(deltaseq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup1 = sapply(d_g_seq, cbind)

 n=1000000
 k='majority'
truetau_RCToutsideOS=NULL

for(id in 1:ncol(setup1)){
 delta=setup1[1,id]
 gamma=setup1[2,id]

 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)
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
				
	rct_size = sum(s==1)

	truetau_RCToutsideOS = rbind(truetau_RCToutsideOS, 
		c(delta, gamma, mean((y1-y0)[z==1 & s==0]),mean((y1-y0)[s==1]), rct_size ))
}


#################


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

res_all = res_RCToutsideOS_all = list()

for(id in 1:ncol(setup)){
  n=1000
  k='majority'
  delta=setup[2,id]
  gamma=setup[3,id]
  thisseed=setup[1,id]

  path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_classicmethods.csv',sep='')
  if(file.exists(path))				   
  res_all[[id]] = read.csv(path)

  path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_classicmethods_RCToutsideOS.csv',sep='')
  if(file.exists(path))					   
  res_RCToutsideOS_all[[id]] = read.csv(path)
}

res_table = NULL
for(id in 1:9){
	idx = which(setup[2,]==truetau[id,1] & setup[3,]==truetau[id,2])
	idx = idx[sapply(idx, function(x) length(res_all[x])>0)
	 & sapply(idx, function(x) length(res_RCToutsideOS_all[x])>0)]
	 res_all_id = do.call(rbind, res_all[idx])
	 res_RCToutsideOS_all_id = do.call(rbind, res_RCToutsideOS_all[idx])

	res_table = rbind(res_table, c(mean((res_all_id[,2] - truetau[id, 3])^2), mean((res_all_id[,3] - truetau[id, 3])^2),
	
				mean((res_all_id[,4] < truetau[id, 3]) & (res_all_id[,5] > truetau[id, 3])),
				mean((res_all_id[,6] < truetau[id, 3]) & (res_all_id[,7] > truetau[id, 3])),
				
				
				mean((res_RCToutsideOS_all_id[,2] - truetau_RCToutsideOS[id, 3])^2), mean((res_RCToutsideOS_all_id[,3] - truetau_RCToutsideOS[id, 3])^2),
				mean((res_RCToutsideOS_all_id[,4] < truetau_RCToutsideOS[id, 3]) & (res_RCToutsideOS_all_id[,5] > truetau_RCToutsideOS[id, 3])),
				mean((res_RCToutsideOS_all_id[,6] < truetau_RCToutsideOS[id, 3]) & (res_RCToutsideOS_all_id[,7] > truetau_RCToutsideOS[id, 3])))
				
				
				)
}

res_table = cbind(truetau[,1:2],res_table)
colnames(res_table) = c('delta', 'gamma', 'weighting', 'ps', 'ci.weighting', 'ci.ps', 'RCToutsideOS.weighting', 'RCToutsideOS.ps', 'ci.RCToutsideOS.weighting', 'ci.RCToutsideOS.ps')

res_table_classic_methods = res_table[order(res_table[,1]),]

tab = cbind(tab, rbind(res_table_classic_methods[,c('weighting', 'ps')], 
						res_table_classic_methods[,c('ci.weighting', 'ci.ps')]))
						
#####################################################
############# RCT outside OS
#####################################################

##################################################### Proposed method: RCT outside OS

keyvalues = read.csv('keyvalues.csv')
intercept_shift=2


### Gamma=1, Delta=0

klist='majority' #c('all','majority','limited')
dlist=c(0,.2,.5)#c(0, .05, .1)
glist=c(1,1.2,1.5)#c(1, 1.5, 2)#c(1, 2, 2.5)

for(delta in dlist){

	for(gamma in glist){
		for(k in klist){
		
			f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4_RCToutsideOS.csv\')[5,2]')))
			
		}
	}
}



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


for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				eval(parse(text=paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap = NULL')))
									
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			
			}

for(id in 1:ncol(setup)){
	k = c('all','majority','limited')
	k = k[setup[1,id]]
	delta = setup[2,id]
	gamma = setup[3,id]
	n = setup[4,id]
	thisseed = setup[5,id]

	v= paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap')
	
	f = paste('results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v5_RCToutsideOS.csv',sep='')
	if(file.exists(f)){
		res = read.csv(f)
		if(ncol(res)==10)
		eval(parse(text=paste(v,' = rbind(',v,', res)')))
	}
}
	



for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
							
				f = paste0('ci.n',n,'.delta',delta,'.gamma',gamma,
							'.',k,'overlap = n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,2:10]')
				eval(parse(text=f))
			}


for(n in nlist)
for(delta in dlist)
for(gamma in glist)
for(k in klist){
for (kk in 1:3){
  eval(parse(text=paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap=
		as.numeric(true.delta',delta,'.gamma',gamma,'.',k,'overlap)')))

  flength = paste0('ci.length.n',n,'.delta',delta,'.gamma',
				gamma,'.',k,'overlap=cbind(ci.length.n',n,
				'.delta',delta,'.gamma',gamma,'.',k,'overlap,
                        (ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]-ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]))')
						
   fcoverage = paste0('ci.coverage.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(ci.coverage.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]<=true.delta',delta,'.gamma',gamma,'.',k,'overlap) &
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]
					>=true.delta',delta,'.gamma',gamma,'.',k,'overlap))')

	fmse = paste0('mse.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(mse.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+1] - 
					true.delta',delta,'.gamma',gamma,'.',k,'overlap)^2)')
					
	eval(parse(text=fcoverage))
	eval(parse(text=flength))
	eval(parse(text=fmse))
}
}



for(n in nlist){
fvecNCov = fvec1NCov = fvec1NCovmse = c()


for(delta in dlist){
for(gamma in glist){

fNCov = 'c('
f1NCov = 'c('
f1NCovmse = 'c('
for(k in klist){

	
	fNCov = paste0(fNCov, 'apply(ci.coverage.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCov = paste0(f1NCov, 'apply(ci.length.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCovmse = paste0(f1NCovmse, 'apply(mse.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	if(k!=tail(klist,1)) {
		fNCov = paste(fNCov, ', ') 			   
		f1NCov = paste(f1NCov, ', ') 			   
		f1NCovmse = paste(f1NCovmse, ', ') 		
	}
	if(k==tail(klist,1)){
		fNCov = paste(fNCov, ')') 
		f1NCov = paste(f1NCov, ')') 
		f1NCovmse = paste(f1NCovmse, ')') 
	}
}

fvecNCov = c(fvecNCov, fNCov)
fvec1NCov = c(fvec1NCov, f1NCov)
fvec1NCovmse = c(fvec1NCovmse, f1NCovmse)

}
}


fvecNCov = paste(fvecNCov, collapse = ',')
fvec1NCov = paste(fvec1NCov, collapse = ',')
fvec1NCovmse = paste(fvec1NCovmse, collapse = ',')

eval(parse(text=paste0('tb.coverage',n,'NCov=rbind(',fvecNCov,')')))
eval(parse(text=paste0('tb.length',n,'NCov=rbind(',fvec1NCov,')')))
eval(parse(text=paste0('tb.mse',n,'NCov=rbind(',fvec1NCovmse,')')))


}


###################################################################
### Gamma=Gamma^*, delta=delta*


options(show.error.messages = TRUE)


olist=2#c('all','majority','limited')
dlist=c(0,.2,.5)#c(0, .05, .1)
glist=c(1,1.2,1.5)#c(1, 1.5, 2)#c(1, 2, 2.5)


klist='majority' #c('all','majority','limited')

nlist = 1000 #c(500, 750)

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
seedseq = c(seedseq, 2*seedseq)
seedseq = c(seedseq, 3*seedseq)
seedseq = c(seedseq, 4*seedseq)


foo <- function(x, y) c(x, y)


o_d_seq = outer(olist, dlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_seq = outer(o_d_seq, glist, FUN = Vectorize(foo, SIMPLIFY = FALSE))
o_d_g_n_seq = outer(o_d_g_seq, nlist, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = outer(o_d_g_n_seq, seedseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))

setup = sapply(setup, cbind)



for(delta in dlist){

	for(gamma in glist){
		for(k in klist){
		
			f = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			fv = paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap')
			eval(parse(text=paste0(fv, '= read.csv(\'',f,'_v4_RCToutsideOS.csv\')[5,2]')))
			
		}
	}
}


for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				eval(parse(text=paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap = NULL')))
									
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			}
			
			
for(id in 1:ncol(setup)){
	k = c('all','majority','limited')
	k = k[setup[1,id]]
	delta = setup[2,id]
	gamma = setup[3,id]
	n = setup[4,id]
	thisseed = setup[5,id]

	v= paste0('n',n,'.delta',delta,'.gamma',
									gamma,'.',k,'overlap')
	
	f = paste('true.results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.seed',thisseed,'_v5_RCToutsideOS.csv',sep='')
	if(file.exists(f)){
		res = read.csv(f)
		eval(parse(text=paste(v,' = rbind(',v,', res)')))
	}
}
	
	

for(n in nlist)
	for(delta in dlist)
		for(gamma in glist)
			for(k in klist){
				
				eval(parse(text=paste0('ci.coverage.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
				eval(parse(text=paste0('ci.length.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))			
				eval(parse(text=paste0('mse.n',n,'.delta',delta,
										'.gamma',gamma,'.',k,'overlap=c()')))
			
			
				f = paste0('ci.n',n,'.delta',delta,'.gamma',gamma,
							'.',k,'overlap = n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,2:10]')
				eval(parse(text=f))
			}



for(n in nlist)
for(delta in dlist)
for(gamma in glist)
for(k in klist){
for (kk in 1:3){
  eval(parse(text=paste0('true.delta',delta,'.gamma',gamma,'.',k,'overlap=
		as.numeric(true.delta',delta,'.gamma',gamma,'.',k,'overlap)')))

  flength = paste0('ci.length.n',n,'.delta',delta,'.gamma',
				gamma,'.',k,'overlap=cbind(ci.length.n',n,
				'.delta',delta,'.gamma',gamma,'.',k,'overlap,
                        (ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]-ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]))')
						
   fcoverage = paste0('ci.coverage.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(ci.coverage.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3-1]<=true.delta',delta,'.gamma',gamma,'.',k,'overlap) &
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+3]
					>=true.delta',delta,'.gamma',gamma,'.',k,'overlap))')
					
	fmse = paste0('mse.n',n,'.delta',delta,'.gamma',
					gamma,'.',k,'overlap=cbind(mse.n',n,
					'.delta',delta,'.gamma',gamma,'.',k,'overlap,
					(ci.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap[,3*(kk-1)+1] - 
					true.delta',delta,'.gamma',gamma,'.',k,'overlap)^2)')

	eval(parse(text=fcoverage))
	eval(parse(text=flength))
	eval(parse(text=fmse))
}
}



for(n in nlist){
fvecNCov = fvec1NCov = fvec1NCovmse = c()


for(delta in dlist){
for(gamma in glist){

fNCov = 'c('
f1NCov = 'c('
f1NCovmse = 'c('
for(k in klist){

	
	fNCov = paste0(fNCov, 'apply(ci.coverage.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCov = paste0(f1NCov, 'apply(ci.length.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	f1NCovmse = paste0(f1NCovmse, 'apply(mse.n',n,'.delta',delta,
			'.gamma',gamma,'.',k,'overlap,2,mean)[1:3]')
	if(k!=tail(klist,1)) {
		fNCov = paste(fNCov, ', ') 			   
		f1NCov = paste(f1NCov, ', ') 			   
		f1NCovmse = paste(f1NCovmse, ', ') 		
	}
	if(k==tail(klist,1)){
		fNCov = paste(fNCov, ')') 
		f1NCov = paste(f1NCov, ')') 
		f1NCovmse = paste(f1NCovmse, ')') 
	}
}


fvecNCov = c(fvecNCov, fNCov)
fvec1NCov = c(fvec1NCov, f1NCov)
fvec1NCovmse = c(fvec1NCovmse, f1NCovmse)
}
}


fvecNCov = paste(fvecNCov, collapse = ',')
fvec1NCov = paste(fvec1NCov, collapse = ',')
fvec1NCovmse = paste(fvec1NCovmse, collapse = ',')
eval(parse(text=paste0('true.tb.coverage',n,'NCov=rbind(',fvecNCov,')')))
eval(parse(text=paste0('true.tb.length',n,'NCov=rbind(',fvec1NCov,')')))
eval(parse(text=paste0('true.tb.mse',n,'NCov=rbind(',fvec1NCovmse,')')))

}

tab1=NULL
tab1 = cbind(tab1, cbind(c(true.tb.mse1000NCov[,3], true.tb.coverage1000NCov[,3]),
						c(tb.mse1000NCov[,3], tb.coverage1000NCov[,3])))
						
################################################ Elastic and R Learner: RCT outside OS

intercept_shift = 2

keyvalues = read.csv('keyvalues.csv')

deltaseq = c(0,.2,.5)
gammaseq = c(1,1.2,1.5)
 
foo <- function(x, y) c(x, y)


d_g_seq = outer(deltaseq, gammaseq, FUN = Vectorize(foo, SIMPLIFY = FALSE))
setup1 = sapply(d_g_seq, cbind)

 n=1000000
 k='majority'
truetau=NULL

for(id in 1:ncol(setup1)){
 delta=setup1[1,id]
 gamma=setup1[2,id]

 
 #delta=0.2
 #gamma=1.2
 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)
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
				

	truetau = rbind(truetau, 
		c(delta, gamma, mean((y1-y0)[z==1 & s==0]),mean((y1-y0)[s==1])))
}

###############################



keyvalues = read.csv('keyvalues.csv')

seedseq <- c(1234, 0841, 9876, 2345, 7654,
 3181, 9478, 5867, 2696, 2824)
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

resCI = list()
for(id1 in 1:ncol(setup1))
	resCI[[id1]] = matrix(0, 0, 2+8)

for(id in 1:ncol(setup)){
 delta=setup[2,id]
 gamma=setup[3,id]
 thisseed=setup[1,id]

 nvar=5
 
ix = which(keyvalues[,2]==k & keyvalues[,3]== delta & keyvalues[,4]==gamma)


id1 = which(truetau[,1]==delta & truetau[,2]==gamma) 

path=paste('comparisons.n',n,'.delta',delta*10,'.gamma',gamma*10,'.',k,'overlap.seed',thisseed,'_v3_RCToutsideOS.csv',sep='')

if(file.exists(path)){
	res = read.csv(path)		  

	if(ncol(res)<157) next;

	tau1 = truetau[id1, 3]
	tau2 = truetau[id1, 4]		  

	intRerr1 = (res[,'X.1'] - tau1)^2
	intRcover1 = (res[,'X2.5.'] <= tau1) & (tau1 <= res[,'X97.5.'])
	
	intRerr2 = (res[,'X.1'] - tau2)^2
	intRcover2 = (res[,'X2.5.'] <= tau2) & (tau2 <= res[,'X97.5.'])
	
	elaserr1 = (res[,'elas.1'] - tau1)^2
	elaserr2 = (res[,'elas.1'] - tau2)^2

	elascover1 = (res[,'elas.1.2']<= tau1) & (tau1 <= res[,'elas.1.3'])
	elascover2 = (res[,'elas.1.2']<= tau2) & (tau2 <= res[,'elas.1.3'])


	resCI[[id1]] = rbind(resCI[[id1]],
			cbind(delta, gamma, 
				elaserr1, elaserr2,
				elascover1, elascover2,
				intRerr1, intRcover1,
				intRerr2, intRcover2))
						
}

}


temp = round(t(sapply(resCI, colMeans)), 3)[, c(1,2,3,5,7,8)]
temp = rbind(temp[order(temp[,1]), c(3,5)], temp[order(temp[,1]), c(4,6)])

tab1 = cbind(tab1, temp)

#################################### Classic methods: RCT outside OS


res_table_classic_methods = res_table[order(res_table[,1]),]

tab1 = cbind(tab1, rbind(res_table_classic_methods[,c('RCToutsideOS.weighting', 'RCToutsideOS.ps')], 
						res_table_classic_methods[,c('ci.RCToutsideOS.weighting', 'ci.RCToutsideOS.ps')]))
						
tab = cbind(tab, tab1)