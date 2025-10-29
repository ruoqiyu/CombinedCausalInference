library(optmatch)
library(randomForest)

#drt: randomized trial data frame
#dos: observational study data frame
#zrt: treatment indicator in the randomized trial
#zos: treatment indicator in the observational study
#oos: overlap domain indicator in the observational study
#vars: name of covariates

#group 1: treated os; group 2: rt; group 3: control os

bridging2<-function(drt,dos,zrt,zos,oos,vars,vars.extra='overlap',weight.extra=10^3,match.ratio=c(1,1,1),method='logistic',penalty=10^2){
  
  if (!is.data.frame(drt)) drt=as.data.frame(drt)
  if (!is.data.frame(dos)) drt=as.data.frame(dos)
  nrt=nrow(drt)
  nos=nrow(dos)
  nos1=sum(zos)
  nos0=sum(1-zos)
  nos1_overlap=sum(zos[oos==1])
  nos1_nonoverlap=nos1-nos1_overlap
  
  d=rbind(drt,dos)
  d$dtype=c(rep('rt',nrt),rep('os',nos))
  d$overlap=c(rep(1,nrt),oos)
  
  Xrt=drt[,vars]
  Xos=dos[,vars]
  X=d[,vars]
  z=c(zrt,zos)
  
  if (method=='logistic'){
    s=as.numeric(d$dtype=='rt')
    Xs=as.data.frame(cbind(s,X))
    colnames(Xs)=c('s',vars)
    Xs_overlap=rbind(Xs[d$overlap==1 & d$dtype=='os' & d$z==1,],
                     Xs[d$dtype=='rt',])
    model_c=glm(s~.,data=Xs_overlap,family='binomial')
    ps_select=model_c$fitted
  }else if (method=='randomForest'){
    s=as.numeric(d$dtype=='rt')
    Xs=as.data.frame(cbind(factor(s),X))
    colnames(Xs)=c('s',vars)
    Xs_overlap=rbind(Xs[d$overlap==1 & d$dtype=='os' & d$z==1,],
                     Xs[d$dtype=='rt',])
    model_c=randomForest(s~.,data=Xs_overlap, norm.votes = TRUE, proximity = TRUE)
    ps_select=predict(model_c, Xs_overlap, type = "prob")[,2]
  }
  
  gscore=(1-ps_select[which(Xs_overlap$s==1)])/ps_select[which(Xs_overlap$s==1)]
  copy=ceiling(gscore/sum(gscore)*nos1_overlap)
  ixr=c()
  for (ii in 1:nrt) ixr=c(ixr,rep(ii,copy[ii]))
  nrt_copy=sum(copy)
  
  os.avg.overlap=apply(dos[zos==1 & oos==1,],2,mean)
  os.avg.nonoverlap=apply(dos[zos==1 & oos==0,],2,mean)
  if (nrt_copy<=nos1_overlap){
    d0=matrix(rep(os.avg.overlap,nos1_overlap-nrt_copy),ncol=ncol(drt),byrow=T)
    colnames(d0)=colnames(dos)
    d1=matrix(rep(os.avg.nonoverlap,nos1_nonoverlap),ncol=ncol(drt),byrow=T)
    colnames(d1)=colnames(dos)
    dfull=rbind(drt[ixr,],d0,d1,dos[order(zos),])
    #group 1: treated os; group 2: rt+imaginary; group 3: control os
    dfull$groupix=c(rep(2,nos1),rep(3,nos0),rep(1,nos1))
    dfull$imagineix=c(rep(0,nrt_copy),rep(1,nos1-nrt_copy),rep(0,nos))
    dfull$overlap=c(rep(1,nos1_overlap),rep(0,nos1_nonoverlap),oos[order(zos)])
    dfull$z=c(zrt[ixr],rep(0,nos1-nrt_copy),zos[order(zos)])
  }else{
    d0=matrix(rep(os.avg.overlap,nrt_copy-nos1_overlap),ncol=ncol(drt),byrow=T)
    colnames(d0)=colnames(dos)
    d1=matrix(rep(os.avg.nonoverlap,nos1_nonoverlap),ncol=ncol(drt),byrow=T)
    colnames(d1)=colnames(dos)
    dfull=rbind(drt[ixr,],d1,dos[order(zos),],d0)
    #group 1: treated os; group 2: rt+imaginary; group 3: control os
    dfull$groupix=c(rep(2,nrt_copy+nos1_nonoverlap),rep(3,nos0),rep(1,nos1+nrt_copy-nos1_overlap))
    dfull$imagineix=c(rep(0,nrt_copy),rep(1,nos1_nonoverlap),rep(0,nos),rep(1,nrt_copy-nos1_overlap))
    dfull$overlap=c(rep(1,nrt_copy),rep(0,nos1_nonoverlap),oos[order(zos)],rep(1,nrt_copy-nos1_overlap))
    dfull$z=c(zrt[ixr],rep(0,nos1_nonoverlap),zos[order(zos)],rep(1,nrt_copy-nos1_overlap))
  }
  
  dist_str <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars), wgts=1)
  
  if (!('overlap'%in%vars.extra)){
    vars.extra=c('overlap',vars.extra)
    if (is.null(weight.extra)) weight.extra=1
    weight.extra=c(1000*max(weight.extra+1),weight.extra)
  }
  for (j in 1:length(vars.extra)){
    dist_str_extra <- multigrp_dist_struc(dfull, 'groupix', components=list(mahal=vars.extra[[j]]), wgts=1)
    for (dj in 1:length(dist_str)){
      dist_str[[dj]]=dist_str[[dj]]+dist_str_extra[[dj]]*weight.extra[j]
    }
  }
  
  for (k in 1:length(dist_str)){
    dist_obj=dist_str[[k]]
    row_name=rownames(dist_obj)
    col_name=colnames(dist_obj)
    for (rowi in 1:nrow(dist_obj)){
      if (dfull[row_name[rowi],]$imagineix==1) dist_obj[rowi,]=penalty
    }
    for (coli in 1:ncol(dist_obj)){
      if (dfull[col_name[coli],]$imagineix==1) dist_obj[,coli]=penalty
    }
    dist_str[[k]]=dist_obj
  }
  
  ## create the match
  res = kwaymatching(dist_str, 'groupix', match.ratio,.data=dfull) # can include e.g., finebalanceVars='race' or 
  match = res$matches
  
  list(match=res$matches,dfull=dfull,copy=copy,gscore=gscore)
}

covbalance <-function(.data, groupvar, imaginevar, overlapvar, matches, vars, select=NULL, poolsd=NULL, copy=1, overlap=T){
  
  stopifnot(all(vars %in% colnames(.data)))
  stopifnot(length(vars)>0)
  
  if(is.null(rownames(.data))){
    warning('Missing rownames of ".data", assuming 1:nrow(.data)')
    rownames(.data) <- 1:nrow(.data)
  }
  
  overlaplabel=.data[,overlapvar]
  match.overlap=.data[match[,1],overlapvar]
  overlap.strata.ix=which(match.overlap==1)
  if (overlap){
    .data.before=.data[overlaplabel==1,]
    matches=matches[overlap.strata.ix,,drop=FALSE]
    matches=matches[.data[matches[,1],imaginevar]==0,]
  }else{
    .data.before=.data[overlaplabel==0,]
    matches=matches[-overlap.strata.ix,,drop=FALSE]
    matches=matches[.data[matches[,1],imaginevar]==0,]
  }
  
  grouplabel=.data.before[,groupvar]
  imaginelabel=.data.before[,imaginevar]
  
  grpnames <- unique(grouplabel)
  k <- dim(table(grouplabel))
  
  if (is.null(select)) select=1:k  
  matches <- as.matrix(matches)
  matches=matches[,select,drop=FALSE]
  inmatch=rownames(.data.before)%in%as.character(matches)
  .data.after <- .data.before[inmatch,]
  stopifnot(dim(.data.after)[1]>0)
  grouplabel.after <- grouplabel[inmatch]
  imaginelabel.after <- imaginelabel[inmatch]
  if(is.null(rownames(matches))) rownames(matches) <- paste('strata', 1:nrow(matches), sep='_')
  
  .strata <- rep(NA, nrow(.data.after[]))
  for(i in 1:nrow(matches))
    .strata[match(as.character(matches[i,]), rownames(.data.after), nomatch=0)] <- rownames(matches)[i]
  
  ## Create the structure of the result
  
  temp <- aggregate(.data.before[,vars], by = list(grp=grouplabel), FUN = mean, na.rm=TRUE)
  meanbefore <- t(temp[select,vars])
  if (2%in%select){
    drto=.data.before[(grouplabel==2) & (imaginelabel==0),]
    drtoo=c()
    ix=0
    for (ck in copy){
      ix=ix+ck
      drtoo=rbind(drtoo,drto[ix,])
    }
    meanbefore[,'2']=apply(drtoo[,vars],2,mean,na.rm=TRUE)
  }
  colnames(meanbefore) <-  temp[select,'grp']
  rm(temp)
  
  temp <- aggregate(.data.after[,vars], by = list(grp=grouplabel.after), FUN = mean, na.rm=TRUE)
  meanafter <- t(temp[,vars])
  colnames(meanafter) <- temp[,'grp']
  rm(temp)
  
  temp <- aggregate(.data[,vars], by = list(grp=.data[,groupvar]), FUN = var, na.rm=TRUE)
  varbefore <- t(temp[,vars])
  colnames(varbefore) <-  temp[,'grp']
  rm(temp)
  
  ## calculate standardized differences
  if (is.null(poolsd)) std_var <- sqrt((varbefore[,1]+varbefore[,3])/2)
  else std_var <-poolsd
  
  std_diff <- list()
  k=length(select)
  for(i in 1:(k-1)){
    for(j in (i+1):k){
      i1 <- colnames(meanbefore)[i]
      j1 <- colnames(meanbefore)[j]
      
      std_diff[[paste0(i1,'-',j1)]] <- 
        cbind((meanbefore[,i1]-meanbefore[,j1])/std_var,
              (meanafter[,i1]-meanafter[,j1])/std_var)
      colnames(std_diff[[paste0(i1,'-',j1)]]) <- c('std_diff_before', 'std_diff_after')
    }
  }
  
  list(meanbefore=meanbefore,meanafter=meanafter,std_diff=std_diff)
}


multigrp_dist_struc<-function(.data, grouplabel, components, wgts){
  stopifnot(length(wgts) == length(components))
  stopifnot(is.numeric(wgts))
  stopifnot(all(wgts>=0))
    
  smahal<-function(z,X){
    X<-as.matrix(X)
    n<-dim(X)[1]
    rownames(X)<-1:n
    k<-dim(X)[2]
    m<-sum(z)
    for (j in 1:k) X[,j]<-rank(X[,j])
    cv<-cov(X)
    if (any(diag(cv)!=0)){
      vuntied<-var(1:n)
      rat<-sqrt(vuntied/diag(cv))
      if (length(rat)==1) cv<-matrix(rat)%*%cv%*%matrix(rat)
      else cv<-diag(rat)%*%cv%*%diag(rat)
    }
    out<-matrix(NA,m,n-m)
    Xc<-X[z==0,,drop=FALSE]
    Xt<-X[z==1,,drop=FALSE]
    rownames(out)<-rownames(X)[z==1]
    colnames(out)<-rownames(X)[z==0]
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Please load package 'MASS'")
    }
    icov<-MASS::ginv(cv)
    for (i in 1:m) out[i,]<-mahalanobis(Xc,Xt[i,],icov,inverted=T)
    out
  }
    
  stopifnot(!is.null(names(components)))
  compt <- names(components)
  compt[tolower(compt) %in% c("prop", "propensity", "mahal", "mahalanobis")] <- ''
  compt <- setdiff(unique(compt), '')
    
  for (i in seq(from = 1, length.out = length(compt))){
    if(!exists(compt[i], mode='function')) stop('Function ',compt[i], ' not found.\n')
  }
    
  stopifnot(!missing(grouplabel))
  stopifnot(is.vector(grouplabel) | is.matrix(grouplabel) | is.data.frame(grouplabel))
    
  gpname <- ''
  if (!is.vector(grouplabel)){
    if (is.null(colnames(grouplabel))){
      grouplabel = apply(grouplabel, 2, function(i) i*grouplabel[,i])
    }else{
      grouplabeltemp = rep('dummy', nrow(grouplabel))
      colLevels <- colnames(grouplabel)
      for (i in 1:length(colLevels)) grouplabeltemp[grouplabel[,i]==1] = colLevels[i]
      grouplabel = grouplabeltemp
      rm(grouplabeltemp)
    }
  }else{
    if (!missing(.data) & all(grouplabel %in% colnames(.data))){
      if (length(grouplabel)==1){
        grouplabeltemp = .data[,grouplabel]
        if(is.numeric(grouplabeltemp)) gpname <- grouplabel
        grouplabel = grouplabeltemp
        rm(grouplabeltemp)
      }else{
        grouplabeltemp = rep('dummy', nrow(.data))
        colLevels <- grouplabel
        for(i in 1:length(colLevels))
          grouplabeltemp[.data[,colLevels[i]]==1] = colLevels[i]
        grouplabel = grouplabeltemp
        rm(grouplabeltemp)
      }
    }
  }
    
  if(is.null(names(grouplabel))){
    names(grouplabel) = rownames(.data)
  } else grouplabel = grouplabel[rownames(.data)]
  stopifnot(all.equal(names(grouplabel), rownames(.data)))
  
  .data <- cbind(.data, .grouplabel = grouplabel)
  grpnames <- unique(grouplabel)
  k <- dim(table(grouplabel))
  stopifnot(k>1)
  
  calculatedist <- function(id, components, .data, i1, j1, wgts){
    
    .data <- .data[.data[,'.grouplabel'] %in% c(i1, j1),]
    grpi1 <- .data[,'.grouplabel'] == i1
    names(grpi1) <- rownames(.data)
    desc <- names(components)[id]
    vars <- components[[id]]
    
    if((!is.null(names(wgts))) & all(names(wgts) %in% names(components))){
      wgt <- wgts[desc]
    } else wgt <- wgts[id]
    
    if(tolower(desc) %in% c("prop", "propensity", "logprop", "logpropensity")){
      .data <- cbind(.data, grpi1 = grpi1)
      proepnsity.model.formula <- as.formula(paste('grpi1 ','~', paste(vars, collapse = " + "), collapse= ' '))
      
      propscore.model = glm(proepnsity.model.formula, data = .data, family = 'binomial')
      
      d <- t(sapply((propscore.model$fitted.values[names(which(grpi1))]),
                    function(x) abs(x - (propscore.model$fitted.values[names(which(!grpi1))]))))
    } else {
      if(tolower(desc) %in% c("mahalanobis", "mahal")){
        d <- sqrt(smahal(grpi1, .data[, vars]))
      } else {
        desc <- eval(parse(text=desc))
        d <- desc(grpi1, .data[,vars])
      }
    }
    
    d <- wgt*d
    rownames(d) <- rownames(.data)[grpi1]
    colnames(d) <- rownames(.data)[!grpi1]
    return(d)
  }
  
  distmat <- list()
  nparts <- length(components)
  
  for(i in 1:(k-1)){
    i1 <- grpnames[i]
    for(j in (i+1):k) {
      j1 <- grpnames[j]
      
      dlist <- lapply(1:nparts, calculatedist, .data = .data, 
                      components = components, i1 = i1, j1 = j1, wgts = wgts)
      distmat[[paste0(i1, '-', j1)]] <- Reduce("+", dlist)
      
    }
  }
  
  return(distmat)
}    


kwaymatching <- function (distmat, grouplabel, design, indexgroup = 1, .data, 
                          finebalanceVars, exactmatchon, ordering, reorder = FALSE, 
                          verbose = TRUE){
  stopifnot(!missing(grouplabel))
  stopifnot(is.vector(grouplabel) | is.matrix(grouplabel) | 
              is.data.frame(grouplabel))
  gpname <- ""
  if (!is.vector(grouplabel)) {
    if (is.null(colnames(grouplabel))) 
      grouplabel = apply(grouplabel, 2, function(i) i * 
                           grouplabel[, i])
    if (!is.null(colnames(grouplabel))) {
      grouplabeltemp = rep("dummy", nrow(grouplabel))
      colLevels <- colnames(grouplabel)
      for (i in 1:length(colLevels)) grouplabeltemp[grouplabel[, 
                                                               i] == 1] = colLevels[i]
      grouplabel = grouplabeltemp
      rm(grouplabeltemp)
    }
  }
  else {
    if (!missing(.data)){
      if (all(grouplabel %in% colnames(.data))) {
        if (length(grouplabel) == 1) {
          grouplabeltemp = .data[, grouplabel]
          if (is.numeric(grouplabeltemp)) 
            gpname <- grouplabel
          grouplabel = grouplabeltemp
          rm(grouplabeltemp)
        }
        else {
          grouplabeltemp = rep("dummy", nrow(.data))
          colLevels <- grouplabel
          for (i in 1:length(colLevels)) grouplabeltemp[.data[, 
                                                              colLevels[i]] == 1] = colLevels[i]
          grouplabel = grouplabeltemp
          rm(grouplabeltemp)
        }
      }
    }
  }
  if (missing(finebalanceVars) & missing(exactmatchon)) {
    if (missing(.data)) {
      warning("Assuming units labels 1:N\nplease make sure that rownames and column names of distance matrices are names so.")
      .data = data.frame(spholder = rep(NA, length(grouplabel)))
      rownames(.data) = 1:length(grouplabel)
    }
  }
  if (!missing(finebalanceVars) | !missing(exactmatchon)) {
    stopifnot(!missing(.data))
    if (!missing(finebalanceVars)) 
      stopifnot(all(finebalanceVars %in% colnames(.data)))
    if (!missing(exactmatchon)) 
      stopifnot(all(exactmatchon %in% colnames(.data)))
  }
  stopifnot(dim(.data)[1] == length(grouplabel))
  if (!missing(ordering)) {
    orgplevels = ordering
    if (reorder) {
      cat("NOTE: Since 'ordering' is given, ignoring 'reorder=TURE'\n")
      reorder = FALSE
    }
  }
  else orgplevels <- levels(factor(grouplabel))
  if (reorder) {
    orgplevels0 = orgplevels
    stopifnot(!missing(indexgroup))
    while (1) {
      orgplevelstemp = sample(orgplevels)
      if (!all((setdiff(orgplevelstemp, indexgroup) == 
                setdiff(orgplevels, indexgroup)))) {
        orgplevels = orgplevelstemp
        break
      }
    }
  }
  grouplabel <- as.numeric(factor(grouplabel, levels = orgplevels, 
                                  ordered = T))
  if (is.null(names(grouplabel))) {
    names(grouplabel) = rownames(.data)
  }
  else grouplabel = grouplabel[rownames(.data)]
  grpsizes <- table(grouplabel)
  names(grpsizes) <- orgplevels
  k <- dim(grpsizes)
  stopifnot(all.equal(names(grouplabel), rownames(.data)))
  if (missing(design)) 
    design = rep(1, k)
  stopifnot(length(design) == k)
  stopifnot(all(design%%1 == 0))
  if (is.null(names(design))) {
    if (exists("colLevels")) 
      names(design) = colLevels
    if (!exists("colLevels")) {
      names(design) = orgplevels
      if (reorder) 
        names(design) = orgplevels0
    }
    if (verbose) {
      cat("Using the design structure: \n")
      print(design)
      cat("If this is incorrect please provide appropriate named vector for 'design'\n")
    }
  }
  else if (verbose) {
    cat("Using the design structure: \n")
    print(design)
  }
  if (missing(indexgroup)) 
    indexgroup <- orgplevels[which.min(grpsizes[orgplevels])]
  if (!is.character(indexgroup)) 
    indexgroup <- as.character(indexgroup)
  stopifnot(design[indexgroup] == 1)
  notindexset = orgplevels[which(orgplevels != indexgroup)]
  stopifnot(all(grpsizes[notindexset] >= (design[notindexset] * 
                                            grpsizes[indexgroup])))
  for (i in 1:(k - 1)){
    for (j in (i + 1):k) {
      i1 = orgplevels[i]
      j1 = orgplevels[j]
      stopifnot(exists(paste0(i1, "-", j1), where = distmat) | 
                  exists(paste0(j1, "-", i1), where = distmat))
      if (!exists(paste0(i1, "-", j1), where = distmat) & j1 != 
          indexgroup) {
        distmat[[paste0(i1, "-", j1)]] = t(distmat[[paste0(j1, 
                                                           "-", i1)]])
        distmat[[paste0(j1, "-", i1)]] = NULL
      }
      if (!exists(paste0(j1, "-", i1), where = distmat) & j1 == 
          indexgroup) {
        distmat[[paste0(j1, "-", i1)]] = t(distmat[[paste0(i1, 
                                                           "-", j1)]])
        distmat[[paste0(i1, "-", j1)]] = NULL
      }
    }
  }
  if (missing(finebalanceVars)) 
    stratify = rep("-0-", nrow(.data))
  if (!missing(finebalanceVars)) 
    stratify = paste0("-", apply(.data[, finebalanceVars, 
                                       drop = FALSE], 1, paste, collapse = "-"))
  names(stratify) = rownames(.data)
  balanceValues = floor(apply(table(stratify, grouplabel), 
                              1, function(x) min(x/design[orgplevels])))
  if (missing(exactmatchon)) 
    exactmatchon = NA
  disttemp = distmat[[paste0(indexgroup, "-", notindexset[1])]]
  interMatch <- nrbalancematch(.data, grouplabel == which(indexgroup == 
                                                            orgplevels), grouplabel == which(notindexset[1] == orgplevels), 
                               stratify, extmatch = exactmatchon, disttemp, balanceValues, 
                               nmatch = design[notindexset[1]])
  interMatch <- interMatch[order(interMatch[, 1]), ,drop=FALSE]
  cost <- sum(apply(interMatch, 1, function(x) disttemp[x[1], 
                                                        x[2]]))
  interMatch <- as.matrix(aggregate(interMatch[, 2], by = list(interMatch[, 
                                                                          1]), function(x) x))
  colnames(interMatch) <- paste(gpname, c(indexgroup, paste(notindexset[1], 
                                                            1:design[notindexset[1]], sep = ".")), sep = "_")
  matchedsofar = c(indexgroup, notindexset[1])
  matchedsofar = rep(matchedsofar, design[matchedsofar])
  for (j in seq(from = 2, length = (k - 2))) {
    j1 = notindexset[j]
    disttemp = lapply(1:length(matchedsofar), function(i) distmat[[paste0(matchedsofar[i], 
                                                                          "-", j1)]][interMatch[, i], ]/design[matchedsofar[i]])
    disttemp = Reduce("+", disttemp)
    rownames(disttemp) = as.character(interMatch[, 1])
    grouplabeltemp = grouplabel
    grouplabeltemp[grouplabeltemp != which(j1 == orgplevels)] <- 0
    grouplabeltemp[as.character(interMatch[, 1])] <- 1
    secondMatch <- nrbalancematch(.data, grouplabeltemp == 
                                    1, grouplabeltemp == which(j1 == orgplevels), stratify, 
                                  extmatch = exactmatchon, disttemp, balanceValues, 
                                  nmatch = design[j1])
    rm(grouplabeltemp)
    cost <- cost + sum(apply(secondMatch, 1, function(x) disttemp[x[1], 
                                                                  x[2]]))
    secondMatch <- as.matrix(aggregate(secondMatch[, 2], 
                                       by = list(secondMatch[, 1]), function(x) x))
    colnames(secondMatch) <- paste(gpname, c(indexgroup, 
                                             paste(j1, 1:design[j1], sep = ".")), sep = "_")
    interMatch <- merge(data.frame(interMatch), data.frame(secondMatch), 
                        by = colnames(data.frame(interMatch))[1])
    matchedsofar = c(matchedsofar, rep(j1, design[j1]))
  }
  return(list(matches = as.matrix(interMatch), cost = cost))
}


nrbalancematch <- function (cardata.fil, trt_labs, ctrl_labs, stratify, extmatch = NA, 
                            distmat, balanceValues, nmatch = 1, solver = 'rlemon') 
{
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    stop("Error: package optmatch (>= 0.9-1) not loaded.  To run rcbalance command, you must install optmatch first and agree to the terms of its license.")
  }
  
  treated = seq(from = 1, length = sum(trt_labs))
  treated.names = rownames(cardata.fil)[trt_labs]
  control = seq(from = length(treated) + 1, length = sum(ctrl_labs))
  control.names = rownames(cardata.fil)[ctrl_labs]
  controlLabs = seq(from = length(treated) + length(control) + 
                      1, length = length(table(stratify)))
  controlLabs.names = names(balanceValues)
  nnodes = c(treated.names, control.names, controlLabs.names)
  controlLab_extended = unlist(sapply(control.names, function(x){
    tag <- which(nnodes == stratify[x])
    ifelse(length(tag)>0, tag, -9999)
  }))
  names(controlLab_extended) = c()

  startn = c(rep(treated, length(control)), control, controlLabs)
  endn = c(rep(control, rep(length(treated), length(control))), 
           controlLab_extended, rep(length(nnodes) + 1, length(controlLabs)))
  
  startn = startn[endn!=-9999]
  endn = endn[endn!=-9999]
  controlLab_extended1 <- controlLab_extended
  controlLab_extended <- controlLab_extended[controlLab_extended!=-999]
  
  #print(balanceValues)
  ucap = rep(1, length(startn)-length(controlLabs))
  ucap = c(ucap, rep(length(treated), length(controlLabs)))
  
  b = c(rep(nmatch, length(treated)), rep(0, length(control)), 
        -nmatch * balanceValues, -(nmatch * (length(treated) - 
                                               sum(balanceValues))))
  
  arc.names.start = c(rep(treated.names, length(control)), 
                      control.names[controlLab_extended1!=-999], controlLabs.names)
  arc.names.end = c(rep(control.names, rep(length(treated), 
                                           length(control))), rep("balance", length(controlLab_extended)), 
                    rep("outflow", length(controlLabs)))
  cost = c()
  cost = c(rep(0, (length(startn) - length(controlLabs))), rep(10 * 
                                                                 max(distmat, na.rm = T), length(controlLabs)))
  
  distmat = distmat[treated.names, control.names]
  cost[1:(length(treated) * length(control))] = as.numeric(distmat)
  if (!is.na(extmatch)) {
    extmatch <- outer(treated.names, control.names, FUN = function(x, 
                                                                   y) cardata.fil[x, extmatch] == cardata.fil[y, extmatch])
    ucap[1:(length(treated) * length(control))] = as.numeric(extmatch)
    cost[1:(length(treated) * length(control))] = cost[1:(length(treated) * length(control))] + 1000 * 
      max(distmat, na.rm = T)*as.numeric(extmatch)
  }
  cost <- cost/max(cost)*1e8
  
  if(solver == 'rrelaxiv'){
    if(requireNamespace('rrelaxiv', quietly = TRUE)) {
      rout <- rrelaxiv::RELAX_IV(startnodes = as.integer(startn),
                                 endnodes = as.integer(endn),
                                 arccosts = as.integer(cost),
                                 arccapacity = as.integer(ucap),
                                 supply = as.integer(b))
      res <- list(crash = 0,
                  feasible = !all(rout == 0),
                  x = rout)
    } else {
      solver = 'rlemon'
      warning('Package rrelaxiv not available, using rlemon instead.')
    }
  }
  if(solver == 'rlemon'){    
    lout <- rlemon::MinCostFlow(arcSources = as.integer(startn),
                                arcTargets = as.integer(endn),
                                arcCapacities = as.integer(ucap),
                                arcCosts = as.integer(cost),
                                nodeSupplies = as.integer(b),
                                numNodes = max(c(startn, endn)),
                                algorithm = 'CycleCancelling')
    res <- list(crash = 0,
                feasible = !all(lout[[1]] == 0),
                x = lout[[1]])
  }else{
    stop(
      'Argument to solver not recognized: please use one of rlemon and rrelaxiv')
  }
  
  if (res$feasible == 0) {
    print("NOT FEASIBLE")
  }
  
  res = res$x
  arc.names.start.res = arc.names.start[res == 1]
  arc.names.end.res = arc.names.end[res == 1]
  match.treated = arc.names.start.res[!(arc.names.end.res %in% c("balance", "outflow"))]
  match.control = arc.names.end.res[!(arc.names.end.res %in% c("balance", "outflow"))]
  
  temp <- cbind(arc.names.start, arc.names.end, startn, endn, cost, ucap, res)
  
  triplets = cbind(match.treated, match.control)
  triplets
}

