#############################
## RCT inference
## delta is the sensitivity parameter for generalizability bias; 0 is no bias
# Function for fixed beta0, calculates p-value
# Use ri=TRUE for randomization inference for homegeneous treatment effect
rct_pval <- function(beta0, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6){
  ## using t distribution here. 
  # The sign here are a bit confusing because Ywt~Z test for 0 vs 1
  if(!ri) return(t.test(Ywt~Z, data=data, mu=-(beta0+delta), alternative='less')$p.val)
  
  ## Randomization inference for a small sample
  if (!is.null(rblock)){
    ## stratfied randomization inference
    block_m <- with(data, tapply(Z, rblock, sum))
    
    declaration <- 
      with(data,{
        declare_ra(
          blocks = rblock,
          block_m = block_m)
      })
  }else{
    declaration <- randomizr::declare_ra(N = length(data$Z), m = sum(data$Z))
  }
  

  sdo <- function(data) {
    mean(data$Ywt[data$Z == 1], na.rm=TRUE) - 
      mean(data$Ywt[data$Z == 0], na.rm=TRUE)
  }
  
  res = ri2::conduct_ri(
    test_function = sdo,
    assignment = "Z",
    outcome = "Ywt",
    declaration = declaration,
    sharp_hypothesis = beta0+delta,
    data = data, sims=nsim, p = 'upper')
  pval=as.numeric(summary(res)[3])
  
  pval=(pval*nsim+1)/(nsim+1)
  return(pval)
}

foo_rct <- function(beta0, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6, alpha=0.05){	
  rct_pval(beta0,data=data, rblock=rblock, delta=delta, ri=ri, nsim=nsim) - alpha
}


foo_rct2 <- function(beta0, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6, alpha=0.5){	
  data1=data
  data1$Ywt = -data1$Ywt
  min(rct_pval(beta0,data=data, rblock=rblock, delta=delta, ri=ri, nsim=nsim), rct_pval(beta0,data=data1, rblock=rblock, delta=delta, ri=ri, nsim=nsim)) - alpha
}

########################
## CI for the observational data

## Assume equal sized blocks
separable1kA <- function (ymat, gamma = 1)
{
  # Modified from separable1k in sensitivitymw package
  # Instead of producing the final inference,
  # this version computes the max expectation and var
  n <- dim(ymat)[1]
  m <- dim(ymat)[2]
  o <- t(apply(ymat, 1, sort))
  yord = t(apply(ymat,1,order))
  
  allmu <- matrix(NA, n, m - 1)
  allsigma2 <- matrix(NA, n, m - 1)
  prworst <- matrix(NA, m, n)
  maxmu <- rep(-Inf, n)
  maxsig2 <- rep(-Inf, n)
  for (j in 1:(m - 1)) {
    pr <- c(rep(1, j), rep(gamma, m - j))/(j + ((m - j) *
                                                  gamma))
    #print(sum(pr))									
    mu <- as.vector( o %*% pr )
    sigma2 <- as.vector((o * o) %*% pr) - (mu * mu)
    chgmu <- (mu > maxmu)
    samemu <- (mu == maxmu)
    if (sum(chgmu) > 0) {
      maxmu[chgmu] <- mu[chgmu]
      maxsig2[chgmu] <- sigma2[chgmu]
      prworst[,chgmu] = pr
    }
    if (sum(samemu) > 0) {
      if(sum(sigma2[samemu] > maxsig2[samemu]) > 0){
        largevar = samemu[sigma2[samemu] > maxsig2[samemu]]
        prworst[,largevar] = pr
      }
      maxsig2[samemu] <- pmax(sigma2[samemu], maxsig2[samemu])
      
    }
  }
  # Reorder 
  prworst = t(prworst)	
  prworst = t(sapply(1:n, function(i) prworst[i,yord[i,]]))
  list(maxmu=maxmu,maxsig2=maxsig2, prworst=prworst)
}


# Function for fixed beta0, calculates p-value
os_pval <- function(beta0, ymat, gamma=1){
  m = dim(ymat)[2]
  ymatbeta0 = ymat
  ymatbeta0[,1] = ymatbeta0[,1] - beta0
  
  extprbeta0 = separable1kA(ymatbeta0, gamma)
  
  tauvec = ymat[,1] - rowMeans(ymat[,-1,drop=FALSE] )
  tauhat = mean( tauvec )
  ymat_extprbeta0 = ymatbeta0 * extprbeta0$prworst
  extauvec = rowSums(ymat_extprbeta0)*(m/(m-1)) - rowSums(ymatbeta0)/(m-1)
  extmu = mean(extauvec)
  extvr = var(tauvec - extauvec)
  zvalbeta0 = (tauhat - extmu - beta0)/sqrt(extvr/nrow(ymat))
  
  pnorm(zvalbeta0, lower=FALSE)
}


foo_os <- function(beta0, ymat, gamma=1, alpha=0.05){
  os_pval(beta0,ymat=ymat, gamma=gamma) - alpha
}

foo_os2 <- function(beta0, ymat, gamma=1, alpha=0.5){
  min(os_pval(beta0,ymat=ymat, gamma=gamma),
  os_pval(beta0,ymat=-ymat, gamma=gamma)) - alpha
}


################
## Confidence interval by combining os and rct
combined_pval <- function(beta0, ymat, gamma=1, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6){
  pval_os=os_pval(beta0,ymat=ymat, gamma=gamma)
  pval_rct=rct_pval(beta0,data=data, rblock=rblock, delta=delta, ri=ri, nsim=nsim)
  tstat=-2*log(pval_os)-2*log(pval_rct)
  pchisq(tstat,4,lower.tail=FALSE)
}

foo_combined <- function(beta0, ymat, gamma=1, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6, alpha=0.05){
  pval_os=os_pval(beta0,ymat=ymat, gamma=gamma)
  pval_rct=rct_pval(beta0,data=data, rblock=rblock, delta=delta, ri=ri, nsim=nsim)
  -2*log(pval_os)-2*log(pval_rct)-qchisq(1-alpha, 4)
}

# Point estimate
foo_combined2 <- function(beta0, ymat, gamma=1, data, rblock=NULL, delta=0, ri=FALSE, nsim=10^6, alpha=0.05){
  pval_os1=os_pval(beta0,ymat=ymat, gamma=gamma)
  pval_rct1=rct_pval(beta0,data=data, rblock=rblock, delta=delta, ri=ri, nsim=nsim)
  pval_os2=os_pval(beta0,ymat=-ymat, gamma=gamma)
  ddata = data; ddata$Ywt = -ddata$Ywt
  pval_rct2=rct_pval(beta0,data=ddata, rblock=rblock, delta=delta, ri=ri, nsim=nsim)
  pval_os=2*min(pval_os1,pval_os2)
  pval_rct=2*min(pval_rct1,pval_rct2)
  -2*log(pval_os)-2*log(pval_rct)-qchisq(1-alpha, 4)
}


#Rosenbaum-style (2002 Stat Science) regression adjustment.  Takes in raw Z,
# X, Y, all excluding unmatched individuals,  plus strata output element from
# optimal.match return object (also excluding unmatched individuals).
adjust.match <- function(X,Y,strata){
  #handle edge cases with 1 pair or no unmatched people.
  strata <- as.factor(strata)
  if(length(unique(strata)) > 1){
    if(is.data.frame(X)){
      if(any(names(X) %in% c('Y','strata')))
        warning('Duplicate column names in regression data frame,avoid "Y" and "strata"')
      new.df <- cbind(X,Y,strata)
      my.lm <- lm(Y ~ ., data = new.df)
    }else{
      my.lm <- lm(Y ~ X + strata)
    }
  } else {
    if(is.data.frame(X)){
      if(any(names(X) %in% c('Y')))
        warning('Duplicate column names in regression data frame,avoid "Y"')
      new.df <- cbind(X,Y)
      my.lm <- lm(Y ~ ., data = new.df)
    }else{    
      my.lm  <- lm(Y ~ X)
    }
  }
  return(residuals(my.lm))
}
