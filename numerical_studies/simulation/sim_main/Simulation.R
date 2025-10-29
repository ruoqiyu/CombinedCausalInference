library(mvtnorm)
library(doParallel)
library(doRNG)

# set working directory as dir
source(paste(dir,'CombineMatching.R',sep=''))
source(paste(dir,'CombineInference.R',sep=''))

#### CREATE KEYVALUES TABLE
set.seed(123)

res=c()
N=10^8
nvar=5
x=mvtnorm::rmvnorm(N,mean=rep(0,nvar))
u=rnorm(N)

olist=c('all','majority','limited')
dlist=c(0,0.2,0.5)
glist=c(1,1.2,1.5)
for (k in olist){
  if (k=='all') o=rep(1,N)
  if (k=='majority') o=as.numeric(x[,1]>=-1)
  if (k=='limited') o=as.numeric(x[,1]>=0)
  for (delta in dlist){
    for (gamma in glist){
      pselect=1/(1+exp(-(-1.5+0.1*x[,1]+0.1*x[,2]-0.3*x[,4])))*o
      s=rbinom(N,1,pselect) #around 1/7 RCT & 6/7 OS
      ps.rct=1/2
      ps=1/(1+exp(-(-2-0.3*x[,1]+0.1*x[,3]-0.2*x[,5]+log(gamma)*u)))
      ps[s==1]=ps.rct
      ps.os=ps[s==0]
      z=rbinom(N,1,ps)
      
      if (delta==0 | k=='all') scale.factor=1
      else scale.factor=sum(z==1 & s==0 & o==0)/sum(z[s==0])
      y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(N)
      y1=y0+delta*o/scale.factor
      true.value=mean((y1-y0)[s==0 & z==1])
      res=rbind(res,c(k,delta,gamma,scale.factor,true.value))
    }
  }
}
colnames(res)=c('overlap','delta','gamma','scaling','true')
keyvalues=res
remove(x,u,o,pselect,s,ps,ps.os,ps.rct,pselect,s,z,y0,y1)




####RUN NUMERICAL EXPERIMENTS
####Section 5.1
##sample size: n
#overlap: o
##internal validity: gamma
##external validity: delta
nlist=c(500,1000)
olist=c('all','majority','limited')
dlist=c(0,0.2,0.5)
glist=c(1,1.2,1.5)

nvar=5
nb=1000
# how many cores to use in the cluster? 
ncores=10

ix=0
for (n in nlist){
  for (k in olist){
    for (delta in dlist){
      for (gamma in glist){
        if (ix==27) ix=0
        ix=ix+1
        
        print(ix)
        print(k)
        print(delta)
        print(gamma)
        # set up a cluster called 'cl'
        cl = makeCluster(ncores)
        
        # register the cluster
        registerDoParallel(cl)
        
        ## do some parallel computaitons with foreach
        t=proc.time()
        results=foreach(i=1:nb,.options.RNG=123,.combine='rbind',.packages='randomForest') %dorng% {
          ## what do do with case ? #
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
          
          scale.factor=as.numeric(keyvalues[ix,'scaling'])
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
          
          ## Check match
          res.matchi=rep(NA,4)
          btboverlap=covbalance(.data=dfull, groupvar='groupix', imaginevar='imagineix',overlapvar='overlap',
                                matches = match,vars=vars,copy=res$copy,overlap=T)
          btboverlap$std_diff
          
          
          res.matchi[1]=max(max(abs(btboverlap$std_diff$`1-2`[,1])),
                            max(abs(btboverlap$std_diff$`1-3`[,1])),
                            max(abs(btboverlap$std_diff$`2-3`[,1])))
          res.matchi[2]=max(max(abs(btboverlap$std_diff$`1-2`[,2])),
                            max(abs(btboverlap$std_diff$`1-3`[,2])),
                            max(abs(btboverlap$std_diff$`2-3`[,2])))
          
          if (k!='all'){
            btbnonoverlap=covbalance(.data=dfull, groupvar='groupix', imaginevar='imagineix',overlapvar='overlap',
                                     matches = match,vars,select=c(1,3),copy=res$copy,overlap=F)
            btbnonoverlap
            res.matchi[3]=max(abs(btbnonoverlap$std_diff$`1-3`[,1]))
            res.matchi[4]=max(abs(btbnonoverlap$std_diff$`1-3`[,2]))
          }
          
          
          #inference
          res.infi=rep(NA,6)
          #rct
          ## delta is the sensitivity parameter for generalizability bias; 0 is no bias
          ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
          matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
          tab=table(matchix)
          copy=numeric(length(res$copy))
          copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
          wt = copy/sum(copy)*nrow(drt)
          ###covarite adjustment
          yrct=y[s==1]
          xx=x[s==1,]
          drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
          ddrt=data.frame(Z=zrt,Y=drt$res)
          ddrt$Ywt = ddrt$Y*wt
          # find confidence limit for 95% upper sided interval [CL, inf)
          res.infi[1]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root
          # find confidence limit for 95% lower sided interval (-inf, CL]
          ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
          res.infi[2]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=delta, alpha=0.025, ri=T, nsim=10^3)$root
          
          #os
          st=rep(1:nrow(match),2)
          xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
          resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
          y1=resy[1:nrow(match)]
          y0=resy[(nrow(match)+1):(2*nrow(match))]
          ymat = cbind(y1,y0)
          # find confidence limit for 95% upper sided interval [CL, inf)
          res.infi[3]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=gamma, alpha=0.025)$root
          # find confidence limit for 95% lower sided interval (-inf, CL]
          res.infi[4]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=gamma, alpha=0.025)$root
          
          #combined
          # find confidence limit for 95% upper sided interval [CL, inf)
          res.infi[5]=uniroot(foo_combined, c(-20,20), data=ddrt, delta=delta, ymat=ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root
          # find confidence limit for 95% lower sided interval (-inf, CL]
          res.infi[6]=-uniroot(foo_combined, c(-20,20), data=ddrt1, delta=delta, ymat=-ymat, gamma=gamma, alpha=0.025, ri=T,nsim=10^3)$root
          c(res.matchi,res.infi)
        }
        print(proc.time()-t)
        ## Always shut the cluster down when done
        stopCluster(cl)
        
        path=paste(dir,'sim_results/true.results.n',n,'.delta',delta,'.gamma',gamma,'.',k,'overlap.csv',sep='')
        write.csv(results,path)
      }
    }
  }
}






####Section 5.2
ix=14
n=1000
k='majority'
delta=0.2
gamma=1.2
dlist=c(0,0.2,0.4,0.6)
glist=c(1,1.2,1.5,1.8)
for (di in dlist){
  for (gi in glist){
    print(di)
    print(gi)
    # set up a cluster called 'cl'
    cl = makeCluster(ncores)
    
    # register the cluster
    registerDoParallel(cl)
    
    ## do some parallel computaitons with foreach
    t=proc.time()
    results=foreach(i=1:nb,.options.RNG=123,.combine='rbind',.packages='randomForest') %dorng% {
      ## what do do with case ? #
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
      
      scale.factor=as.numeric(keyvalues[ix,'scaling'])
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
      res.infi=rep(NA,6)
      #rct
      ## delta is the sensitivity parameter for generalizability bias; 0 is no bias
      # find confidence limit for 95% upper sided interval [CL, inf)
      ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
      matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
      tab=table(matchix)
      copy=numeric(length(res$copy))
      copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
      wt = copy/sum(copy)*nrow(drt)
      ###covarite adjustment
      yrct=y[s==1]
      xx=x[s==1,]
      drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
      ddrt=data.frame(Z=zrt,Y=drt$res)
      ## weighted outcomes
      ddrt$Ywt = ddrt$Y*wt
      res.infi[1]=uniroot(foo_rct, c(-50,50), data=ddrt, delta=di, alpha=0.025, ri=T, nsim=10^3)$root
      # find confidence limit for 95% lower sided interval (-inf, CL]
      ddrt1 = ddrt; ddrt1$Ywt = -ddrt1$Ywt
      res.infi[2]=-uniroot(foo_rct, c(-50,50), data=ddrt1, delta=di, alpha=0.025, ri=T, nsim=10^3)$root
      
      #os
      st=rep(1:nrow(match),2)
      xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
      resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
      y1=resy[1:nrow(match)]
      y0=resy[(nrow(match)+1):(2*nrow(match))]
      ymat = cbind(y1,y0)
      # find confidence limit for 95% upper sided interval [CL, inf)
      res.infi[3]=uniroot(foo_os, c(-20,20), ymat=ymat, gamma=gi, alpha=0.025)$root
      # find confidence limit for 95% lower sided interval (-inf, CL]
      res.infi[4]=-uniroot(foo_os, c(-20,20), ymat=-ymat, gamma=gi, alpha=0.025)$root
      
      #combined
      # find confidence limit for 95% upper sided interval [CL, inf)
      res.infi[5]=uniroot(foo_combined, c(-20,20), data=ddrt, delta=di, ymat=ymat, gamma=gi, alpha=0.025, ri=T,nsim=10^3)$root
      # find confidence limit for 95% lower sided interval (-inf, CL]
      res.infi[6]=-uniroot(foo_combined, c(-20,20), data=ddrt1, delta=di, ymat=-ymat, gamma=gi, alpha=0.025, ri=T,nsim=10^3)$root
      res.infi
    }
    print(proc.time()-t)
    ## Always shut the cluster down when done
    stopCluster(cl)
    
    path=paste(dir,'sim_results/results.n',n,'.delta',di,'.gamma',gi,'.',k,'overlap.csv',sep='')
    write.csv(results,path)
  }
}



### Section 5.3
### power analysis
nvar=5
n=1000
delta=0
gamma=1
dlist=c(0,0.2,0.4,0.6)
glist=c(1,1.2,1.5,1.8)
k='majority'
tlist=c(0,0.2,0.4,0.6,0.8,1)
for (tau in tlist){
  for (di in dlist){
    for (gi in glist){
      
      print(di)
      print(gi)
      
      # set up a cluster called 'cl'
      cl = makeCluster(ncores)
      
      # register the cluster
      registerDoParallel(cl)
      
      ## do some parallel computaitons with foreach
      t=proc.time()
      results=foreach(i=1:nb,.options.RNG=123,.combine='rbind',.packages='randomForest') %dorng% {
        ## what do do with case ? #
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
        
        scale.factor=1
        y0=10+4*x[,1]-2*x[,2]+3*x[,5]+u+rnorm(n)
        y1=y0+delta*o/scale.factor+tau
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
        res.infi=rep(NA,3)
        #rct
        ## delta is the sensitivity parameter for generalizability bias; 0 is no bias
        # find confidence limit for 95% upper sided interval [CL, inf)
        ddrt=dfull[dfull$groupix==2 & dfull$imagineix==0,]
        matchix=rep(1:length(res$copy),res$copy)[rownames(ddrt)%in%match[,2]]
        tab=table(matchix)
        copy=numeric(length(res$copy))
        copy[(1:length(res$copy)) %in%as.numeric(rownames(tab))]=tab
        wt = copy/sum(copy)*nrow(drt)
        
        ###covarite adjustment
        yrct=y[s==1]
        xx=x[s==1,]
        drt$res = adjust.match(xx,yrct,strata=rep(1,nrow(drt)))
        ddrt=data.frame(Z=zrt,Y=drt$res)
        ## weighted outcomes
        ddrt$Ywt = ddrt$Y*wt
        res.infi[1]=rct_pval(0, data=ddrt, rblock=NULL, delta=di, ri=T, nsim=10^3)
        
        #os
        st=rep(1:nrow(match),2)
        xx=data.matrix(dfull[c(match[,1],match[,3]),c('x1','x2','x3','x4','x5')])
        resy = adjust.match(X=xx,Y=dfull[c(match[,1],match[,3]),'y'],strata=st)
        y1=resy[1:nrow(match)]
        y0=resy[(nrow(match)+1):(2*nrow(match))]
        ymat = cbind(y1,y0)
        res.infi[2]=os_pval(0, ymat, gamma=gi)
        
        #combined
        res.infi[3]=combined_pval(0, ymat, gamma=gi, data=ddrt, rblock=NULL, delta=di, ri=T, nsim=10^3)
        res.infi
      }
      print(proc.time()-t)
      ## Always shut the cluster down when done
      stopCluster(cl)
      
      path=paste(dir,'sim_results/power.results.n1000.delta',di,'.gamma',gi,'.effect',tau,'.csv',sep='')
      write.csv(results,path)
    }
  }
}



