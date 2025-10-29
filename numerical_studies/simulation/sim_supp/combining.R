##install relax iv package
#drat::addRepo("rrelaxiv", "https://errickson.net/rrelaxiv")
#install.packages("rrelaxiv")
#install.packages('optmatch')

#library(approxmatch)
library(optmatch)
source('covbalance.R')
source('kwaymatching.R')
source('multigrp_dist_struc.R')
source('nrbalancematch.R')
source('tripletmatching.R')

#drt: randomized trial data frame
#dos: observational study data frame
#zrt: treatment indicator in the randomized trial
#zos: treatment indicator in the observational study
#oos: overlap domain indicator in the observational study
#vars: name of covariates

#group 1: treated os; group 2: rt; group 3: control os

bridging2<-function(drt,dos,zrt,zos,oos,vars,vars.extra=NULL,weight.extra=1,penalty=1000){
#  stopifnot(all(colnames(dos)==colnames(drt)))
  if (!is.data.frame(drt)) drt=as.data.frame(drt)
  if (!is.data.frame(dos)) drt=as.data.frame(dos)
  nrt=nrow(drt)
  nos=nrow(dos)
  nos1=sum(zos)
  nos0=sum(1-zos)
  nos1_overlap=sum(zos[oos==1])
  nos0_overlap=sum(1-zos[oos==1])
  
  d=rbind(drt,dos)
  d$dtype=c(rep('rt',nrt),rep('os',nos))
  d$overlap=c(rep(1,nrt),oos)
  
  Xrt=drt[,vars]
  Xos=dos[,vars]
  X=d[,vars]
  z=c(zrt,zos)
  Xz_os=as.data.frame(cbind(zos,Xos))
  colnames(Xz_os)=c('zos',vars)
  model_os=glm(zos~.,data=Xz_os,family='binomial')
  ps_os=model_os$fitted
  ps_rt=predict(model_os,cbind(rep(1,nrt),Xrt),type="response")
  s=as.numeric(d$dtype=='rt')
  Xs=as.data.frame(cbind(s,X))
  colnames(Xs)=c('s',vars)
  Xs_overlap=Xs[d$overlap==1,]
  model_c=glm(s~.,data=Xs_overlap,family='binomial')
  ps_select=model_c$fitted
  
  copy=pmax(1,floor(ps_rt*(1-ps_select[which(s==1)])/ps_select[which(s==1)]))
  ixr=c()
  for (ii in 1:nrt) ixr=c(ixr,rep(ii,copy[ii]))
  nrt_copy=sum(copy)
  
  if (nrt_copy<nos1){
    d0=matrix(0,ncol=ncol(drt),nrow=nos1-nrt_copy)
    colnames(d0)=colnames(dos)
    dfull=rbind(drt[ixr,],d0,dos[order(zos),])
    #group 1: treated os; group 2: rt+imaginary; group 3: control os
    dfull$groupix=c(rep(2,nos1),rep(3,nos0),rep(1,nos1))
    dfull$imagineix=c(rep(0,nrt_copy),rep(1,nos1-nrt_copy),rep(0,nos))
    
    dist_str <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars), wgts=1)
    if (!is.null(vars.extra)){
      for (j in 1:length(vars.extra)){
        dist_str_extra <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars.extra[[j]]), wgts=1)
        for (dj in 1:length(dist_str)){
          dist_str[[dj]]=dist_str[[dj]]+dist_str_extra[[dj]]*weight.extra[j]
        }
      }
    }
    max.dist <- max(sapply(dist_str, max))
    for (k in 1:length(dist_str)){
      dist_obj=dist_str[[k]]
      row_name=rownames(dist_obj)
      col_name=colnames(dist_obj)
      for (rowi in 1:nrow(dist_obj)){
        if (dfull[row_name[rowi],]$imagineix==1) dist_obj[rowi,]=penalty*max.dist
      }
      for (coli in 1:ncol(dist_obj)){
        if (dfull[col_name[coli],]$imagineix==1) dist_obj[,coli]=penalty*max.dist
      }
      dist_str[[k]]=dist_obj
    }
  }else{
    dfull=rbind(drt[ixr,],dos[order(zos),])
    #group 1: treated os; group 2: rt; group 3: control os
    dfull$groupix=c(rep(2,nrt_copy),rep(3,nos0),rep(1,nos1))
    dfull$imagineix=rep(0,nrt_copy+nos)
    
    dist_str <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars), wgts=1)
    if (!is.null(vars.extra)){
      for (j in 1:length(vars.extra)){
        dist_str_extra <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars.extra[[j]]), wgts=1)
        for (dj in 1:length(dist_str)){
          dist_str[[dj]]=dist_str[[dj]]+dist_str_extra[[dj]]*weight.extra[j]
        }
      }
    }
  }
  
  ## create the match
  res = kwaymatching(dist_str, 'groupix', c(1,1,1),.data=dfull) # can include e.g., finebalanceVars='race' or 
  match = res$matches
  
  list(match=res$matches,dfull=dfull,copy=copy)
}

covbalance <-function(.data, groupvar, imaginevar, matches, vars, select=NULL, poolsd=NULL){
  
  stopifnot(all(vars %in% colnames(.data)))
  stopifnot(length(vars)>0)
  
  if(is.null(rownames(.data))){
    warning('Missing rownames of ".data", assuming 1:nrow(.data)')
    rownames(.data) <- 1:nrow(.data)
  }
  
  grouplabel=.data[,groupvar]
  imaginelabel=.data[,imaginevar]
  
  grpnames <- unique(grouplabel)
  k <- dim(table(grouplabel))
  
  if (is.null(select)) select=1:k  
  matches <- as.matrix(matches)
  matches=matches[,select,drop=FALSE]
  .data.after <- .data[rownames(.data)%in%as.character(matches),]
  stopifnot(dim(.data.after)[1]>0)
  grouplabel.after <- grouplabel[rownames(.data)%in%as.character(matches)]
  #    grouplabel.after <- grouplabel.after[rownames(.data.after)]
  imaginelabel.after <- imaginelabel[rownames(.data)%in%as.character(matches)]
  #    imaginelabel.after <- imaginelabel.after[rownames(.data.after)]
  .data.observe=.data.after[imaginelabel.after==0,]
  .grouplabel.observe=grouplabel.after[imaginelabel.after==0]
  if(is.null(rownames(matches))) rownames(matches) <- paste('strata', 1:nrow(matches), sep='_')
  
  .strata <- rep(NA, nrow(.data.after[]))
  for(i in 1:nrow(matches))
    .strata[match(as.character(matches[i,]), rownames(.data.after), nomatch=0)] <- rownames(matches)[i]
  
  ## Create the structure of the result
  
    temp <- aggregate(.data[,vars], by = list(grp=grouplabel), FUN = mean, na.rm=TRUE)
    meanbefore <- t(temp[select,vars])
    colnames(meanbefore) <-  temp[select,'grp']
#    meanbefore <- meanbefore[vars, grpnames]
    rm(temp)
    
    temp <- aggregate(.data.after[,vars], by = list(grp=.data.after[,groupvar]), FUN = mean, na.rm=TRUE)
    meanafter <- t(temp[,vars])
    colnames(meanafter) <- temp[,'grp']
#    meanafter <- meanafter[vars, grpnames]
    rm(temp)
  
    temp <- aggregate(.data[,vars], by = list(grp=grouplabel), FUN = var, na.rm=TRUE)
    varbefore <- t(temp[,vars])
    colnames(varbefore) <-  temp[,'grp']
#    varbefore <- varbefore[vars, grpnames]
    rm(temp)
  
  ## calculate standized differences
  
  std_diff <- list()
  k=length(select)
  for(i in 1:(k-1)){
    for(j in (i+1):k){
      i1 <- colnames(meanbefore)[i]
      j1 <- colnames(meanbefore)[j]
      
      if (is.null(poolsd)) std_var <- sqrt((varbefore[,i1]+varbefore[,j1])/2)
      else std_var <-poolsd
      std_diff[[paste0(i1,'-',j1)]] <- 
        cbind((meanbefore[,i1]-meanbefore[,j1])/std_var,
              (meanafter[,i1]-meanafter[,j1])/std_var)
      colnames(std_diff[[paste0(i1,'-',j1)]]) <- c('std_diff_before', 'std_diff_after')
    }
  }
  
  # .data=.data[.data$imagineix==0,]
  # .data.after=.data.after[.data.after$imagineix==0,]
  #pval.before=apply(.data[,vars],2,function(v) summary(aov(v~factor(.data$groupix)))[[1]][5][1,])
  #pval.after=apply(.data.after[,vars],2,function(v) summary(aov(v~factor(.data.after$groupix)+factor(.strata)))[[1]][5][1,])
  # Kruskal-Wallis Rank Sum Test
  #list(meanbefore=meanbefore,meanafter=meanafter,std_diff=std_diff,f.pval=cbind(pval.before,pval.after))
  list(meanbefore=meanbefore,meanafter=meanafter,std_diff=std_diff)
}


