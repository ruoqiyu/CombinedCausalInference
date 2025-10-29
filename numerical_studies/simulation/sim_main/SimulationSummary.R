library(ggplot2)
library(ggpubr)

## Table S1 and Table 1
true.delta0.gamma1.alloverlap=as.numeric(keyvalues[1,'true'])
true.delta0.gamma1.2.alloverlap=as.numeric(keyvalues[2,'true'])
true.delta0.gamma1.5.alloverlap=as.numeric(keyvalues[3,'true'])
true.delta0.2.gamma1.alloverlap=as.numeric(keyvalues[4,'true'])
true.delta0.2.gamma1.2.alloverlap=as.numeric(keyvalues[5,'true'])
true.delta0.2.gamma1.5.alloverlap=as.numeric(keyvalues[6,'true'])
true.delta0.5.gamma1.alloverlap=as.numeric(keyvalues[7,'true'])
true.delta0.5.gamma1.2.alloverlap=as.numeric(keyvalues[8,'true'])
true.delta0.5.gamma1.5.alloverlap=as.numeric(keyvalues[9,'true'])
true.delta0.gamma1.majorityoverlap=as.numeric(keyvalues[10,'true'])
true.delta0.gamma1.2.majorityoverlap=as.numeric(keyvalues[11,'true'])
true.delta0.gamma1.5.majorityoverlap=as.numeric(keyvalues[12,'true'])
true.delta0.2.gamma1.majorityoverlap=as.numeric(keyvalues[13,'true'])
true.delta0.2.gamma1.2.majorityoverlap=as.numeric(keyvalues[14,'true'])
true.delta0.2.gamma1.5.majorityoverlap=as.numeric(keyvalues[15,'true'])
true.delta0.5.gamma1.majorityoverlap=as.numeric(keyvalues[16,'true'])
true.delta0.5.gamma1.2.majorityoverlap=as.numeric(keyvalues[17,'true'])
true.delta0.5.gamma1.5.majorityoverlap=as.numeric(keyvalues[18,'true'])
true.delta0.gamma1.limitedoverlap=as.numeric(keyvalues[19,'true'])
true.delta0.gamma1.2.limitedoverlap=as.numeric(keyvalues[20,'true'])
true.delta0.gamma1.5.limitedoverlap=as.numeric(keyvalues[21,'true'])
true.delta0.2.gamma1.limitedoverlap=as.numeric(keyvalues[22,'true'])
true.delta0.2.gamma1.2.limitedoverlap=as.numeric(keyvalues[23,'true'])
true.delta0.2.gamma1.5.limitedoverlap=as.numeric(keyvalues[24,'true'])
true.delta0.5.gamma1.limitedoverlap=as.numeric(keyvalues[25,'true'])
true.delta0.5.gamma1.2.limitedoverlap=as.numeric(keyvalues[26,'true'])
true.delta0.5.gamma1.5.limitedoverlap=as.numeric(keyvalues[27,'true'])

###true parameters
n500.delta0.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.alloverlap.csv',sep=''))
match.n500.delta0.gamma1.alloverlap=n500.delta0.gamma1.alloverlap[,2:5]
ci.n500.delta0.gamma1.alloverlap=n500.delta0.gamma1.alloverlap[,6:11]

n500.delta0.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.majorityoverlap.csv',sep=''))
match.n500.delta0.gamma1.majorityoverlap=n500.delta0.gamma1.majorityoverlap[,2:5]
ci.n500.delta0.gamma1.majorityoverlap=n500.delta0.gamma1.majorityoverlap[,6:11]

n500.delta0.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.limitedoverlap.csv',sep=''))
match.n500.delta0.gamma1.limitedoverlap=n500.delta0.gamma1.limitedoverlap[,2:5]
ci.n500.delta0.gamma1.limitedoverlap=n500.delta0.gamma1.limitedoverlap[,6:11]

n500.delta0.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.2.alloverlap.csv',sep=''))
match.n500.delta0.gamma1.2.alloverlap=n500.delta0.gamma1.2.alloverlap[,2:5]
ci.n500.delta0.gamma1.2.alloverlap=n500.delta0.gamma1.2.alloverlap[,6:11]

n500.delta0.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.2.majorityoverlap.csv',sep=''))
match.n500.delta0.gamma1.2.majorityoverlap=n500.delta0.gamma1.2.majorityoverlap[,2:5]
ci.n500.delta0.gamma1.2.majorityoverlap=n500.delta0.gamma1.2.majorityoverlap[,6:11]

n500.delta0.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.2.limitedoverlap.csv',sep=''))
match.n500.delta0.gamma1.2.limitedoverlap=n500.delta0.gamma1.2.limitedoverlap[,2:5]
ci.n500.delta0.gamma1.2.limitedoverlap=n500.delta0.gamma1.2.limitedoverlap[,6:11]

n500.delta0.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.5.alloverlap.csv',sep=''))
match.n500.delta0.gamma1.5.alloverlap=n500.delta0.gamma1.5.alloverlap[,2:5]
ci.n500.delta0.gamma1.5.alloverlap=n500.delta0.gamma1.5.alloverlap[,6:11]

n500.delta0.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.5.majorityoverlap.csv',sep=''))
match.n500.delta0.gamma1.5.majorityoverlap=n500.delta0.gamma1.5.majorityoverlap[,2:5]
ci.n500.delta0.gamma1.5.majorityoverlap=n500.delta0.gamma1.5.majorityoverlap[,6:11]

n500.delta0.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.gamma1.5.limitedoverlap.csv',sep=''))
match.n500.delta0.gamma1.5.limitedoverlap=n500.delta0.gamma1.5.limitedoverlap[,2:5]
ci.n500.delta0.gamma1.5.limitedoverlap=n500.delta0.gamma1.5.limitedoverlap[,6:11]

n500.delta0.2.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.alloverlap.csv',sep=''))
match.n500.delta0.2.gamma1.alloverlap=n500.delta0.2.gamma1.alloverlap[,2:5]
ci.n500.delta0.2.gamma1.alloverlap=n500.delta0.2.gamma1.alloverlap[,6:11]

n500.delta0.2.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.majorityoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.majorityoverlap=n500.delta0.2.gamma1.majorityoverlap[,2:5]
ci.n500.delta0.2.gamma1.majorityoverlap=n500.delta0.2.gamma1.majorityoverlap[,6:11]

n500.delta0.2.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.limitedoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.limitedoverlap=n500.delta0.2.gamma1.limitedoverlap[,2:5]
ci.n500.delta0.2.gamma1.limitedoverlap=n500.delta0.2.gamma1.limitedoverlap[,6:11]

n500.delta0.2.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.2.alloverlap.csv',sep=''))
match.n500.delta0.2.gamma1.2.alloverlap=n500.delta0.2.gamma1.2.alloverlap[,2:5]
ci.n500.delta0.2.gamma1.2.alloverlap=n500.delta0.2.gamma1.2.alloverlap[,6:11]

n500.delta0.2.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.2.majorityoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.2.majorityoverlap=n500.delta0.2.gamma1.2.majorityoverlap[,2:5]
ci.n500.delta0.2.gamma1.2.majorityoverlap=n500.delta0.2.gamma1.2.majorityoverlap[,6:11]

n500.delta0.2.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.2.limitedoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.2.limitedoverlap=n500.delta0.2.gamma1.2.limitedoverlap[,2:5]
ci.n500.delta0.2.gamma1.2.limitedoverlap=n500.delta0.2.gamma1.2.limitedoverlap[,6:11]

n500.delta0.2.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.5.alloverlap.csv',sep=''))
match.n500.delta0.2.gamma1.5.alloverlap=n500.delta0.2.gamma1.5.alloverlap[,2:5]
ci.n500.delta0.2.gamma1.5.alloverlap=n500.delta0.2.gamma1.5.alloverlap[,6:11]

n500.delta0.2.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.5.majorityoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.5.majorityoverlap=n500.delta0.2.gamma1.5.majorityoverlap[,2:5]
ci.n500.delta0.2.gamma1.5.majorityoverlap=n500.delta0.2.gamma1.5.majorityoverlap[,6:11]

n500.delta0.2.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.2.gamma1.5.limitedoverlap.csv',sep=''))
match.n500.delta0.2.gamma1.5.limitedoverlap=n500.delta0.2.gamma1.5.limitedoverlap[,2:5]
ci.n500.delta0.2.gamma1.5.limitedoverlap=n500.delta0.2.gamma1.5.limitedoverlap[,6:11]

n500.delta0.5.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.alloverlap.csv',sep=''))
match.n500.delta0.5.gamma1.alloverlap=n500.delta0.5.gamma1.alloverlap[,2:5]
ci.n500.delta0.5.gamma1.alloverlap=n500.delta0.5.gamma1.alloverlap[,6:11]

n500.delta0.5.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.majorityoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.majorityoverlap=n500.delta0.5.gamma1.majorityoverlap[,2:5]
ci.n500.delta0.5.gamma1.majorityoverlap=n500.delta0.5.gamma1.majorityoverlap[,6:11]

n500.delta0.5.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.limitedoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.limitedoverlap=n500.delta0.5.gamma1.limitedoverlap[,2:5]
ci.n500.delta0.5.gamma1.limitedoverlap=n500.delta0.5.gamma1.limitedoverlap[,6:11]

n500.delta0.5.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.2.alloverlap.csv',sep=''))
match.n500.delta0.5.gamma1.2.alloverlap=n500.delta0.5.gamma1.2.alloverlap[,2:5]
ci.n500.delta0.5.gamma1.2.alloverlap=n500.delta0.5.gamma1.2.alloverlap[,6:11]

n500.delta0.5.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.2.majorityoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.2.majorityoverlap=n500.delta0.5.gamma1.2.majorityoverlap[,2:5]
ci.n500.delta0.5.gamma1.2.majorityoverlap=n500.delta0.5.gamma1.2.majorityoverlap[,6:11]

n500.delta0.5.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.2.limitedoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.2.limitedoverlap=n500.delta0.5.gamma1.2.limitedoverlap[,2:5]
ci.n500.delta0.5.gamma1.2.limitedoverlap=n500.delta0.5.gamma1.2.limitedoverlap[,6:11]

n500.delta0.5.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.5.alloverlap.csv',sep=''))
match.n500.delta0.5.gamma1.5.alloverlap=n500.delta0.5.gamma1.5.alloverlap[,2:5]
ci.n500.delta0.5.gamma1.5.alloverlap=n500.delta0.5.gamma1.5.alloverlap[,6:11]

n500.delta0.5.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.5.majorityoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.5.majorityoverlap=n500.delta0.5.gamma1.5.majorityoverlap[,2:5]
ci.n500.delta0.5.gamma1.5.majorityoverlap=n500.delta0.5.gamma1.5.majorityoverlap[,6:11]

n500.delta0.5.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n500.delta0.5.gamma1.5.limitedoverlap.csv',sep=''))
match.n500.delta0.5.gamma1.5.limitedoverlap=n500.delta0.5.gamma1.5.limitedoverlap[,2:5]
ci.n500.delta0.5.gamma1.5.limitedoverlap=n500.delta0.5.gamma1.5.limitedoverlap[,6:11]


btb500=rbind(c(apply(match.n500.delta0.gamma1.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.gamma1.2.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.2.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.gamma1.5.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.5.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.gamma1.5.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.2.gamma1.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.2.gamma1.2.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.2.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.2.gamma1.5.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.5.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.2.gamma1.5.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.5.gamma1.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.5.gamma1.2.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.2.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
             c(apply(match.n500.delta0.5.gamma1.5.alloverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.5.majorityoverlap,2,mean,na.rm=T),
               apply(match.n500.delta0.5.gamma1.5.limitedoverlap,2,mean,na.rm=T)))
xtable::xtable(btb500)


ci.length.n500.delta0.gamma1.alloverlap=c()
ci.coverage.n500.delta0.gamma1.alloverlap=c()
ci.length.n500.delta0.gamma1.2.alloverlap=c()
ci.coverage.n500.delta0.gamma1.2.alloverlap=c()
ci.length.n500.delta0.gamma1.5.alloverlap=c()
ci.coverage.n500.delta0.gamma1.5.alloverlap=c()
ci.length.n500.delta0.2.gamma1.alloverlap=c()
ci.coverage.n500.delta0.2.gamma1.alloverlap=c()
ci.length.n500.delta0.2.gamma1.2.alloverlap=c()
ci.coverage.n500.delta0.2.gamma1.2.alloverlap=c()
ci.length.n500.delta0.2.gamma1.5.alloverlap=c()
ci.coverage.n500.delta0.2.gamma1.5.alloverlap=c()
ci.length.n500.delta0.5.gamma1.alloverlap=c()
ci.coverage.n500.delta0.5.gamma1.alloverlap=c()
ci.length.n500.delta0.5.gamma1.2.alloverlap=c()
ci.coverage.n500.delta0.5.gamma1.2.alloverlap=c()
ci.length.n500.delta0.5.gamma1.5.alloverlap=c()
ci.coverage.n500.delta0.5.gamma1.5.alloverlap=c()
for (kk in 1:3){
  ci.length.n500.delta0.gamma1.alloverlap=cbind(ci.length.n500.delta0.gamma1.alloverlap,
                                                ci.n500.delta0.gamma1.alloverlap[,2*kk]-ci.n500.delta0.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.alloverlap=cbind(ci.coverage.n500.delta0.gamma1.alloverlap,
                                                  (ci.n500.delta0.gamma1.alloverlap[,2*kk-1]<=true.delta0.gamma1.alloverlap)&
                                                    (ci.n500.delta0.gamma1.alloverlap[,2*kk]>=true.delta0.gamma1.alloverlap))
  ci.length.n500.delta0.gamma1.2.alloverlap=cbind(ci.length.n500.delta0.gamma1.2.alloverlap,
                                                  ci.n500.delta0.gamma1.2.alloverlap[,2*kk]-ci.n500.delta0.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.2.alloverlap=cbind(ci.coverage.n500.delta0.gamma1.2.alloverlap,
                                                    (ci.n500.delta0.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.gamma1.2.alloverlap)&
                                                      (ci.n500.delta0.gamma1.2.alloverlap[,2*kk]>=true.delta0.gamma1.2.alloverlap))
  ci.length.n500.delta0.gamma1.5.alloverlap=cbind(ci.length.n500.delta0.gamma1.5.alloverlap,
                                                  ci.n500.delta0.gamma1.5.alloverlap[,2*kk]-ci.n500.delta0.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.5.alloverlap=cbind(ci.coverage.n500.delta0.gamma1.5.alloverlap,
                                                    (ci.n500.delta0.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.gamma1.5.alloverlap)&
                                                      (ci.n500.delta0.gamma1.5.alloverlap[,2*kk]>=true.delta0.gamma1.5.alloverlap))
  ci.length.n500.delta0.2.gamma1.alloverlap=cbind(ci.length.n500.delta0.2.gamma1.alloverlap,
                                                  ci.n500.delta0.2.gamma1.alloverlap[,2*kk]-ci.n500.delta0.2.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.alloverlap=cbind(ci.coverage.n500.delta0.2.gamma1.alloverlap,
                                                    (ci.n500.delta0.2.gamma1.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.alloverlap)&
                                                      (ci.n500.delta0.2.gamma1.alloverlap[,2*kk]>=true.delta0.2.gamma1.alloverlap))
  ci.length.n500.delta0.2.gamma1.2.alloverlap=cbind(ci.length.n500.delta0.2.gamma1.2.alloverlap,
                                                    ci.n500.delta0.2.gamma1.2.alloverlap[,2*kk]-ci.n500.delta0.2.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.2.alloverlap=cbind(ci.coverage.n500.delta0.2.gamma1.2.alloverlap,
                                                      (ci.n500.delta0.2.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.2.alloverlap)&
                                                        (ci.n500.delta0.2.gamma1.2.alloverlap[,2*kk]>=true.delta0.2.gamma1.2.alloverlap))
  ci.length.n500.delta0.2.gamma1.5.alloverlap=cbind(ci.length.n500.delta0.2.gamma1.5.alloverlap,
                                                    ci.n500.delta0.2.gamma1.5.alloverlap[,2*kk]-ci.n500.delta0.2.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.5.alloverlap=cbind(ci.coverage.n500.delta0.2.gamma1.5.alloverlap,
                                                      (ci.n500.delta0.2.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.5.alloverlap)&
                                                        (ci.n500.delta0.2.gamma1.5.alloverlap[,2*kk]>=true.delta0.2.gamma1.5.alloverlap))
  ci.length.n500.delta0.5.gamma1.alloverlap=cbind(ci.length.n500.delta0.5.gamma1.alloverlap,
                                                  ci.n500.delta0.5.gamma1.alloverlap[,2*kk]-ci.n500.delta0.5.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.alloverlap=cbind(ci.coverage.n500.delta0.5.gamma1.alloverlap,
                                                    (ci.n500.delta0.5.gamma1.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.alloverlap)&
                                                      (ci.n500.delta0.5.gamma1.alloverlap[,2*kk]>=true.delta0.5.gamma1.alloverlap))
  ci.length.n500.delta0.5.gamma1.2.alloverlap=cbind(ci.length.n500.delta0.5.gamma1.2.alloverlap,
                                                    ci.n500.delta0.5.gamma1.2.alloverlap[,2*kk]-ci.n500.delta0.5.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.2.alloverlap=cbind(ci.coverage.n500.delta0.5.gamma1.2.alloverlap,
                                                      (ci.n500.delta0.5.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.2.alloverlap)&
                                                        (ci.n500.delta0.5.gamma1.2.alloverlap[,2*kk]>=true.delta0.5.gamma1.2.alloverlap))
  ci.length.n500.delta0.5.gamma1.5.alloverlap=cbind(ci.length.n500.delta0.5.gamma1.5.alloverlap,
                                                    ci.n500.delta0.5.gamma1.5.alloverlap[,2*kk]-ci.n500.delta0.5.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.5.alloverlap=cbind(ci.coverage.n500.delta0.5.gamma1.5.alloverlap,
                                                      (ci.n500.delta0.5.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.5.alloverlap)&
                                                        (ci.n500.delta0.5.gamma1.5.alloverlap[,2*kk]>=true.delta0.5.gamma1.5.alloverlap))
}


ci.length.n500.delta0.gamma1.majorityoverlap=c()
ci.coverage.n500.delta0.gamma1.majorityoverlap=c()
ci.length.n500.delta0.gamma1.2.majorityoverlap=c()
ci.coverage.n500.delta0.gamma1.2.majorityoverlap=c()
ci.length.n500.delta0.gamma1.5.majorityoverlap=c()
ci.coverage.n500.delta0.gamma1.5.majorityoverlap=c()
ci.length.n500.delta0.2.gamma1.majorityoverlap=c()
ci.coverage.n500.delta0.2.gamma1.majorityoverlap=c()
ci.length.n500.delta0.2.gamma1.2.majorityoverlap=c()
ci.coverage.n500.delta0.2.gamma1.2.majorityoverlap=c()
ci.length.n500.delta0.2.gamma1.5.majorityoverlap=c()
ci.coverage.n500.delta0.2.gamma1.5.majorityoverlap=c()
ci.length.n500.delta0.5.gamma1.majorityoverlap=c()
ci.coverage.n500.delta0.5.gamma1.majorityoverlap=c()
ci.length.n500.delta0.5.gamma1.2.majorityoverlap=c()
ci.coverage.n500.delta0.5.gamma1.2.majorityoverlap=c()
ci.length.n500.delta0.5.gamma1.5.majorityoverlap=c()
ci.coverage.n500.delta0.5.gamma1.5.majorityoverlap=c()
for (kk in 1:3){
  ci.length.n500.delta0.gamma1.majorityoverlap=cbind(ci.length.n500.delta0.gamma1.majorityoverlap,
                                                     ci.n500.delta0.gamma1.majorityoverlap[,2*kk]-ci.n500.delta0.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.majorityoverlap=cbind(ci.coverage.n500.delta0.gamma1.majorityoverlap,
                                                       (ci.n500.delta0.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.majorityoverlap)&
                                                         (ci.n500.delta0.gamma1.majorityoverlap[,2*kk]>=true.delta0.gamma1.majorityoverlap))
  ci.length.n500.delta0.gamma1.2.majorityoverlap=cbind(ci.length.n500.delta0.gamma1.2.majorityoverlap,
                                                       ci.n500.delta0.gamma1.2.majorityoverlap[,2*kk]-ci.n500.delta0.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.2.majorityoverlap=cbind(ci.coverage.n500.delta0.gamma1.2.majorityoverlap,
                                                         (ci.n500.delta0.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.2.majorityoverlap)&
                                                           (ci.n500.delta0.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.gamma1.2.majorityoverlap))
  ci.length.n500.delta0.gamma1.5.majorityoverlap=cbind(ci.length.n500.delta0.gamma1.5.majorityoverlap,
                                                       ci.n500.delta0.gamma1.5.majorityoverlap[,2*kk]-ci.n500.delta0.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.5.majorityoverlap=cbind(ci.coverage.n500.delta0.gamma1.5.majorityoverlap,
                                                         (ci.n500.delta0.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.5.majorityoverlap)&
                                                           (ci.n500.delta0.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.gamma1.5.majorityoverlap))
  ci.length.n500.delta0.2.gamma1.majorityoverlap=cbind(ci.length.n500.delta0.2.gamma1.majorityoverlap,
                                                       ci.n500.delta0.2.gamma1.majorityoverlap[,2*kk]-ci.n500.delta0.2.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.majorityoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.majorityoverlap,
                                                         (ci.n500.delta0.2.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.majorityoverlap)&
                                                           (ci.n500.delta0.2.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.majorityoverlap))
  ci.length.n500.delta0.2.gamma1.2.majorityoverlap=cbind(ci.length.n500.delta0.2.gamma1.2.majorityoverlap,
                                                         ci.n500.delta0.2.gamma1.2.majorityoverlap[,2*kk]-ci.n500.delta0.2.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.2.majorityoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.2.majorityoverlap,
                                                           (ci.n500.delta0.2.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                             (ci.n500.delta0.2.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n500.delta0.2.gamma1.5.majorityoverlap=cbind(ci.length.n500.delta0.2.gamma1.5.majorityoverlap,
                                                         ci.n500.delta0.2.gamma1.5.majorityoverlap[,2*kk]-ci.n500.delta0.2.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.5.majorityoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.5.majorityoverlap,
                                                           (ci.n500.delta0.2.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.5.majorityoverlap)&
                                                             (ci.n500.delta0.2.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.5.majorityoverlap))
  ci.length.n500.delta0.5.gamma1.majorityoverlap=cbind(ci.length.n500.delta0.5.gamma1.majorityoverlap,
                                                       ci.n500.delta0.5.gamma1.majorityoverlap[,2*kk]-ci.n500.delta0.5.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.majorityoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.majorityoverlap,
                                                         (ci.n500.delta0.5.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.majorityoverlap)&
                                                           (ci.n500.delta0.5.gamma1.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.majorityoverlap))
  ci.length.n500.delta0.5.gamma1.2.majorityoverlap=cbind(ci.length.n500.delta0.5.gamma1.2.majorityoverlap,
                                                         ci.n500.delta0.5.gamma1.2.majorityoverlap[,2*kk]-ci.n500.delta0.5.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.2.majorityoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.2.majorityoverlap,
                                                           (ci.n500.delta0.5.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.2.majorityoverlap)&
                                                             (ci.n500.delta0.5.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.2.majorityoverlap))
  ci.length.n500.delta0.5.gamma1.5.majorityoverlap=cbind(ci.length.n500.delta0.5.gamma1.5.majorityoverlap,
                                                         ci.n500.delta0.5.gamma1.5.majorityoverlap[,2*kk]-ci.n500.delta0.5.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.5.majorityoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.5.majorityoverlap,
                                                           (ci.n500.delta0.5.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.5.majorityoverlap)&
                                                             (ci.n500.delta0.5.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.5.majorityoverlap))
}


ci.length.n500.delta0.gamma1.limitedoverlap=c()
ci.coverage.n500.delta0.gamma1.limitedoverlap=c()
ci.length.n500.delta0.gamma1.2.limitedoverlap=c()
ci.coverage.n500.delta0.gamma1.2.limitedoverlap=c()
ci.length.n500.delta0.gamma1.5.limitedoverlap=c()
ci.coverage.n500.delta0.gamma1.5.limitedoverlap=c()
ci.length.n500.delta0.2.gamma1.limitedoverlap=c()
ci.coverage.n500.delta0.2.gamma1.limitedoverlap=c()
ci.length.n500.delta0.2.gamma1.2.limitedoverlap=c()
ci.coverage.n500.delta0.2.gamma1.2.limitedoverlap=c()
ci.length.n500.delta0.2.gamma1.5.limitedoverlap=c()
ci.coverage.n500.delta0.2.gamma1.5.limitedoverlap=c()
ci.length.n500.delta0.5.gamma1.limitedoverlap=c()
ci.coverage.n500.delta0.5.gamma1.limitedoverlap=c()
ci.length.n500.delta0.5.gamma1.2.limitedoverlap=c()
ci.coverage.n500.delta0.5.gamma1.2.limitedoverlap=c()
ci.length.n500.delta0.5.gamma1.5.limitedoverlap=c()
ci.coverage.n500.delta0.5.gamma1.5.limitedoverlap=c()
for (kk in 1:3){
  ci.length.n500.delta0.gamma1.limitedoverlap=cbind(ci.length.n500.delta0.gamma1.limitedoverlap,
                                                    ci.n500.delta0.gamma1.limitedoverlap[,2*kk]-ci.n500.delta0.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.limitedoverlap=cbind(ci.coverage.n500.delta0.gamma1.limitedoverlap,
                                                      (ci.n500.delta0.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.limitedoverlap)&
                                                        (ci.n500.delta0.gamma1.limitedoverlap[,2*kk]>=true.delta0.gamma1.limitedoverlap))
  ci.length.n500.delta0.gamma1.2.limitedoverlap=cbind(ci.length.n500.delta0.gamma1.2.limitedoverlap,
                                                      ci.n500.delta0.gamma1.2.limitedoverlap[,2*kk]-ci.n500.delta0.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.2.limitedoverlap=cbind(ci.coverage.n500.delta0.gamma1.2.limitedoverlap,
                                                        (ci.n500.delta0.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.2.limitedoverlap)&
                                                          (ci.n500.delta0.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.gamma1.2.limitedoverlap))
  ci.length.n500.delta0.gamma1.5.limitedoverlap=cbind(ci.length.n500.delta0.gamma1.5.limitedoverlap,
                                                      ci.n500.delta0.gamma1.5.limitedoverlap[,2*kk]-ci.n500.delta0.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.gamma1.5.limitedoverlap=cbind(ci.coverage.n500.delta0.gamma1.5.limitedoverlap,
                                                        (ci.n500.delta0.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.5.limitedoverlap)&
                                                          (ci.n500.delta0.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.gamma1.5.limitedoverlap))
  ci.length.n500.delta0.2.gamma1.limitedoverlap=cbind(ci.length.n500.delta0.2.gamma1.limitedoverlap,
                                                      ci.n500.delta0.2.gamma1.limitedoverlap[,2*kk]-ci.n500.delta0.2.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.limitedoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.limitedoverlap,
                                                        (ci.n500.delta0.2.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.limitedoverlap)&
                                                          (ci.n500.delta0.2.gamma1.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.limitedoverlap))
  ci.length.n500.delta0.2.gamma1.2.limitedoverlap=cbind(ci.length.n500.delta0.2.gamma1.2.limitedoverlap,
                                                        ci.n500.delta0.2.gamma1.2.limitedoverlap[,2*kk]-ci.n500.delta0.2.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.2.limitedoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.2.limitedoverlap,
                                                          (ci.n500.delta0.2.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.limitedoverlap)&
                                                            (ci.n500.delta0.2.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.2.limitedoverlap))
  ci.length.n500.delta0.2.gamma1.5.limitedoverlap=cbind(ci.length.n500.delta0.2.gamma1.5.limitedoverlap,
                                                        ci.n500.delta0.2.gamma1.5.limitedoverlap[,2*kk]-ci.n500.delta0.2.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.2.gamma1.5.limitedoverlap=cbind(ci.coverage.n500.delta0.2.gamma1.5.limitedoverlap,
                                                          (ci.n500.delta0.2.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.5.limitedoverlap)&
                                                            (ci.n500.delta0.2.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.5.limitedoverlap))
  ci.length.n500.delta0.5.gamma1.limitedoverlap=cbind(ci.length.n500.delta0.5.gamma1.limitedoverlap,
                                                      ci.n500.delta0.5.gamma1.limitedoverlap[,2*kk]-ci.n500.delta0.5.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.limitedoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.limitedoverlap,
                                                        (ci.n500.delta0.5.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.limitedoverlap)&
                                                          (ci.n500.delta0.5.gamma1.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.limitedoverlap))
  ci.length.n500.delta0.5.gamma1.2.limitedoverlap=cbind(ci.length.n500.delta0.5.gamma1.2.limitedoverlap,
                                                        ci.n500.delta0.5.gamma1.2.limitedoverlap[,2*kk]-ci.n500.delta0.5.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.2.limitedoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.2.limitedoverlap,
                                                          (ci.n500.delta0.5.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.2.limitedoverlap)&
                                                            (ci.n500.delta0.5.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.2.limitedoverlap))
  ci.length.n500.delta0.5.gamma1.5.limitedoverlap=cbind(ci.length.n500.delta0.5.gamma1.5.limitedoverlap,
                                                        ci.n500.delta0.5.gamma1.5.limitedoverlap[,2*kk]-ci.n500.delta0.5.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n500.delta0.5.gamma1.5.limitedoverlap=cbind(ci.coverage.n500.delta0.5.gamma1.5.limitedoverlap,
                                                          (ci.n500.delta0.5.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.5.limitedoverlap)&
                                                            (ci.n500.delta0.5.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.5.limitedoverlap))
}


tb.coverage500=rbind(c(apply(ci.coverage.n500.delta0.gamma1.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.gamma1.2.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.2.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.2.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.gamma1.5.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.5.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.gamma1.5.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.2.gamma1.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.2.gamma1.2.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.2.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.2.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.2.gamma1.5.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.5.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.2.gamma1.5.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.5.gamma1.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.5.gamma1.2.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.2.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.2.limitedoverlap,2,mean)),
                     c(apply(ci.coverage.n500.delta0.5.gamma1.5.alloverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.5.majorityoverlap,2,mean),
                       apply(ci.coverage.n500.delta0.5.gamma1.5.limitedoverlap,2,mean)))
xtable::xtable(tb.coverage500)


tb.length500=rbind(c(apply(ci.length.n500.delta0.gamma1.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.gamma1.2.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.2.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.2.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.gamma1.5.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.5.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.gamma1.5.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.2.gamma1.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.2.gamma1.2.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.2.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.2.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.2.gamma1.5.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.5.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.2.gamma1.5.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.5.gamma1.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.5.gamma1.2.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.2.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.2.limitedoverlap,2,mean)),
                   c(apply(ci.length.n500.delta0.5.gamma1.5.alloverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.5.majorityoverlap,2,mean),
                     apply(ci.length.n500.delta0.5.gamma1.5.limitedoverlap,2,mean)))
xtable::xtable(tb.length500)




n1000.delta0.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.alloverlap.csv',sep=''))
match.n1000.delta0.gamma1.alloverlap=n1000.delta0.gamma1.alloverlap[,2:5]
ci.n1000.delta0.gamma1.alloverlap=n1000.delta0.gamma1.alloverlap[,6:11]

n1000.delta0.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.majorityoverlap.csv',sep=''))
match.n1000.delta0.gamma1.majorityoverlap=n1000.delta0.gamma1.majorityoverlap[,2:5]
ci.n1000.delta0.gamma1.majorityoverlap=n1000.delta0.gamma1.majorityoverlap[,6:11]

n1000.delta0.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.limitedoverlap.csv',sep=''))
match.n1000.delta0.gamma1.limitedoverlap=n1000.delta0.gamma1.limitedoverlap[,2:5]
ci.n1000.delta0.gamma1.limitedoverlap=n1000.delta0.gamma1.limitedoverlap[,6:11]

n1000.delta0.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.2.alloverlap.csv',sep=''))
match.n1000.delta0.gamma1.2.alloverlap=n1000.delta0.gamma1.2.alloverlap[,2:5]
ci.n1000.delta0.gamma1.2.alloverlap=n1000.delta0.gamma1.2.alloverlap[,6:11]

n1000.delta0.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.2.majorityoverlap.csv',sep=''))
match.n1000.delta0.gamma1.2.majorityoverlap=n1000.delta0.gamma1.2.majorityoverlap[,2:5]
ci.n1000.delta0.gamma1.2.majorityoverlap=n1000.delta0.gamma1.2.majorityoverlap[,6:11]

n1000.delta0.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.2.limitedoverlap.csv',sep=''))
match.n1000.delta0.gamma1.2.limitedoverlap=n1000.delta0.gamma1.2.limitedoverlap[,2:5]
ci.n1000.delta0.gamma1.2.limitedoverlap=n1000.delta0.gamma1.2.limitedoverlap[,6:11]

n1000.delta0.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.5.alloverlap.csv',sep=''))
match.n1000.delta0.gamma1.5.alloverlap=n1000.delta0.gamma1.5.alloverlap[,2:5]
ci.n1000.delta0.gamma1.5.alloverlap=n1000.delta0.gamma1.5.alloverlap[,6:11]

n1000.delta0.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.5.majorityoverlap.csv',sep=''))
match.n1000.delta0.gamma1.5.majorityoverlap=n1000.delta0.gamma1.5.majorityoverlap[,2:5]
ci.n1000.delta0.gamma1.5.majorityoverlap=n1000.delta0.gamma1.5.majorityoverlap[,6:11]

n1000.delta0.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.gamma1.5.limitedoverlap.csv',sep=''))
match.n1000.delta0.gamma1.5.limitedoverlap=n1000.delta0.gamma1.5.limitedoverlap[,2:5]
ci.n1000.delta0.gamma1.5.limitedoverlap=n1000.delta0.gamma1.5.limitedoverlap[,6:11]

n1000.delta0.2.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.alloverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.alloverlap=n1000.delta0.2.gamma1.alloverlap[,2:5]
ci.n1000.delta0.2.gamma1.alloverlap=n1000.delta0.2.gamma1.alloverlap[,6:11]

n1000.delta0.2.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.majorityoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.majorityoverlap=n1000.delta0.2.gamma1.majorityoverlap[,2:5]
ci.n1000.delta0.2.gamma1.majorityoverlap=n1000.delta0.2.gamma1.majorityoverlap[,6:11]

n1000.delta0.2.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.limitedoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.limitedoverlap=n1000.delta0.2.gamma1.limitedoverlap[,2:5]
ci.n1000.delta0.2.gamma1.limitedoverlap=n1000.delta0.2.gamma1.limitedoverlap[,6:11]

n1000.delta0.2.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.2.alloverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.2.alloverlap=n1000.delta0.2.gamma1.2.alloverlap[,2:5]
ci.n1000.delta0.2.gamma1.2.alloverlap=n1000.delta0.2.gamma1.2.alloverlap[,6:11]

n1000.delta0.2.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.2.majorityoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.2.majorityoverlap=n1000.delta0.2.gamma1.2.majorityoverlap[,2:5]
ci.n1000.delta0.2.gamma1.2.majorityoverlap=n1000.delta0.2.gamma1.2.majorityoverlap[,6:11]

n1000.delta0.2.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.2.limitedoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.2.limitedoverlap=n1000.delta0.2.gamma1.2.limitedoverlap[,2:5]
ci.n1000.delta0.2.gamma1.2.limitedoverlap=n1000.delta0.2.gamma1.2.limitedoverlap[,6:11]

n1000.delta0.2.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.5.alloverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.5.alloverlap=n1000.delta0.2.gamma1.5.alloverlap[,2:5]
ci.n1000.delta0.2.gamma1.5.alloverlap=n1000.delta0.2.gamma1.5.alloverlap[,6:11]

n1000.delta0.2.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.5.majorityoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.5.majorityoverlap=n1000.delta0.2.gamma1.5.majorityoverlap[,2:5]
ci.n1000.delta0.2.gamma1.5.majorityoverlap=n1000.delta0.2.gamma1.5.majorityoverlap[,6:11]

n1000.delta0.2.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.2.gamma1.5.limitedoverlap.csv',sep=''))
match.n1000.delta0.2.gamma1.5.limitedoverlap=n1000.delta0.2.gamma1.5.limitedoverlap[,2:5]
ci.n1000.delta0.2.gamma1.5.limitedoverlap=n1000.delta0.2.gamma1.5.limitedoverlap[,6:11]

n1000.delta0.5.gamma1.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.alloverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.alloverlap=n1000.delta0.5.gamma1.alloverlap[,2:5]
ci.n1000.delta0.5.gamma1.alloverlap=n1000.delta0.5.gamma1.alloverlap[,6:11]

n1000.delta0.5.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.majorityoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.majorityoverlap=n1000.delta0.5.gamma1.majorityoverlap[,2:5]
ci.n1000.delta0.5.gamma1.majorityoverlap=n1000.delta0.5.gamma1.majorityoverlap[,6:11]

n1000.delta0.5.gamma1.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.limitedoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.limitedoverlap=n1000.delta0.5.gamma1.limitedoverlap[,2:5]
ci.n1000.delta0.5.gamma1.limitedoverlap=n1000.delta0.5.gamma1.limitedoverlap[,6:11]

n1000.delta0.5.gamma1.2.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.2.alloverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.2.alloverlap=n1000.delta0.5.gamma1.2.alloverlap[,2:5]
ci.n1000.delta0.5.gamma1.2.alloverlap=n1000.delta0.5.gamma1.2.alloverlap[,6:11]

n1000.delta0.5.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.2.majorityoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.2.majorityoverlap=n1000.delta0.5.gamma1.2.majorityoverlap[,2:5]
ci.n1000.delta0.5.gamma1.2.majorityoverlap=n1000.delta0.5.gamma1.2.majorityoverlap[,6:11]

n1000.delta0.5.gamma1.2.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.2.limitedoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.2.limitedoverlap=n1000.delta0.5.gamma1.2.limitedoverlap[,2:5]
ci.n1000.delta0.5.gamma1.2.limitedoverlap=n1000.delta0.5.gamma1.2.limitedoverlap[,6:11]

n1000.delta0.5.gamma1.5.alloverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.5.alloverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.5.alloverlap=n1000.delta0.5.gamma1.5.alloverlap[,2:5]
ci.n1000.delta0.5.gamma1.5.alloverlap=n1000.delta0.5.gamma1.5.alloverlap[,6:11]

n1000.delta0.5.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.5.majorityoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.5.majorityoverlap=n1000.delta0.5.gamma1.5.majorityoverlap[,2:5]
ci.n1000.delta0.5.gamma1.5.majorityoverlap=n1000.delta0.5.gamma1.5.majorityoverlap[,6:11]

n1000.delta0.5.gamma1.5.limitedoverlap=read.csv(paste(dir,'sim_results/true.results.n1000.delta0.5.gamma1.5.limitedoverlap.csv',sep=''))
match.n1000.delta0.5.gamma1.5.limitedoverlap=n1000.delta0.5.gamma1.5.limitedoverlap[,2:5]
ci.n1000.delta0.5.gamma1.5.limitedoverlap=n1000.delta0.5.gamma1.5.limitedoverlap[,6:11]


btb1000=rbind(c(apply(match.n1000.delta0.gamma1.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.gamma1.2.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.2.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.gamma1.5.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.5.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.gamma1.5.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.2.gamma1.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.2.gamma1.2.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.2.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.2.gamma1.5.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.5.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.2.gamma1.5.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.5.gamma1.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.5.gamma1.2.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.2.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.2.limitedoverlap,2,mean,na.rm=T)),
              c(apply(match.n1000.delta0.5.gamma1.5.alloverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.5.majorityoverlap,2,mean,na.rm=T),
                apply(match.n1000.delta0.5.gamma1.5.limitedoverlap,2,mean,na.rm=T)))
xtable::xtable(btb1000)


ci.length.n1000.delta0.gamma1.alloverlap=c()
ci.coverage.n1000.delta0.gamma1.alloverlap=c()
ci.length.n1000.delta0.gamma1.2.alloverlap=c()
ci.coverage.n1000.delta0.gamma1.2.alloverlap=c()
ci.length.n1000.delta0.gamma1.5.alloverlap=c()
ci.coverage.n1000.delta0.gamma1.5.alloverlap=c()
ci.length.n1000.delta0.2.gamma1.alloverlap=c()
ci.coverage.n1000.delta0.2.gamma1.alloverlap=c()
ci.length.n1000.delta0.2.gamma1.2.alloverlap=c()
ci.coverage.n1000.delta0.2.gamma1.2.alloverlap=c()
ci.length.n1000.delta0.2.gamma1.5.alloverlap=c()
ci.coverage.n1000.delta0.2.gamma1.5.alloverlap=c()
ci.length.n1000.delta0.5.gamma1.alloverlap=c()
ci.coverage.n1000.delta0.5.gamma1.alloverlap=c()
ci.length.n1000.delta0.5.gamma1.2.alloverlap=c()
ci.coverage.n1000.delta0.5.gamma1.2.alloverlap=c()
ci.length.n1000.delta0.5.gamma1.5.alloverlap=c()
ci.coverage.n1000.delta0.5.gamma1.5.alloverlap=c()
for (kk in 1:3){
  ci.length.n1000.delta0.gamma1.alloverlap=cbind(ci.length.n1000.delta0.gamma1.alloverlap,
                                                 ci.n1000.delta0.gamma1.alloverlap[,2*kk]-ci.n1000.delta0.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.alloverlap=cbind(ci.coverage.n1000.delta0.gamma1.alloverlap,
                                                   (ci.n1000.delta0.gamma1.alloverlap[,2*kk-1]<=true.delta0.gamma1.alloverlap)&
                                                     (ci.n1000.delta0.gamma1.alloverlap[,2*kk]>=true.delta0.gamma1.alloverlap))
  ci.length.n1000.delta0.gamma1.2.alloverlap=cbind(ci.length.n1000.delta0.gamma1.2.alloverlap,
                                                   ci.n1000.delta0.gamma1.2.alloverlap[,2*kk]-ci.n1000.delta0.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.2.alloverlap=cbind(ci.coverage.n1000.delta0.gamma1.2.alloverlap,
                                                     (ci.n1000.delta0.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.gamma1.2.alloverlap)&
                                                       (ci.n1000.delta0.gamma1.2.alloverlap[,2*kk]>=true.delta0.gamma1.2.alloverlap))
  ci.length.n1000.delta0.gamma1.5.alloverlap=cbind(ci.length.n1000.delta0.gamma1.5.alloverlap,
                                                   ci.n1000.delta0.gamma1.5.alloverlap[,2*kk]-ci.n1000.delta0.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.5.alloverlap=cbind(ci.coverage.n1000.delta0.gamma1.5.alloverlap,
                                                     (ci.n1000.delta0.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.gamma1.5.alloverlap)&
                                                       (ci.n1000.delta0.gamma1.5.alloverlap[,2*kk]>=true.delta0.gamma1.5.alloverlap))
  ci.length.n1000.delta0.2.gamma1.alloverlap=cbind(ci.length.n1000.delta0.2.gamma1.alloverlap,
                                                   ci.n1000.delta0.2.gamma1.alloverlap[,2*kk]-ci.n1000.delta0.2.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.alloverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.alloverlap,
                                                     (ci.n1000.delta0.2.gamma1.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.alloverlap)&
                                                       (ci.n1000.delta0.2.gamma1.alloverlap[,2*kk]>=true.delta0.2.gamma1.alloverlap))
  ci.length.n1000.delta0.2.gamma1.2.alloverlap=cbind(ci.length.n1000.delta0.2.gamma1.2.alloverlap,
                                                     ci.n1000.delta0.2.gamma1.2.alloverlap[,2*kk]-ci.n1000.delta0.2.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.2.alloverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.2.alloverlap,
                                                       (ci.n1000.delta0.2.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.2.alloverlap)&
                                                         (ci.n1000.delta0.2.gamma1.2.alloverlap[,2*kk]>=true.delta0.2.gamma1.2.alloverlap))
  ci.length.n1000.delta0.2.gamma1.5.alloverlap=cbind(ci.length.n1000.delta0.2.gamma1.5.alloverlap,
                                                     ci.n1000.delta0.2.gamma1.5.alloverlap[,2*kk]-ci.n1000.delta0.2.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.5.alloverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.5.alloverlap,
                                                       (ci.n1000.delta0.2.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.2.gamma1.5.alloverlap)&
                                                         (ci.n1000.delta0.2.gamma1.5.alloverlap[,2*kk]>=true.delta0.2.gamma1.5.alloverlap))
  ci.length.n1000.delta0.5.gamma1.alloverlap=cbind(ci.length.n1000.delta0.5.gamma1.alloverlap,
                                                   ci.n1000.delta0.5.gamma1.alloverlap[,2*kk]-ci.n1000.delta0.5.gamma1.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.alloverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.alloverlap,
                                                     (ci.n1000.delta0.5.gamma1.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.alloverlap)&
                                                       (ci.n1000.delta0.5.gamma1.alloverlap[,2*kk]>=true.delta0.5.gamma1.alloverlap))
  ci.length.n1000.delta0.5.gamma1.2.alloverlap=cbind(ci.length.n1000.delta0.5.gamma1.2.alloverlap,
                                                     ci.n1000.delta0.5.gamma1.2.alloverlap[,2*kk]-ci.n1000.delta0.5.gamma1.2.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.2.alloverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.2.alloverlap,
                                                       (ci.n1000.delta0.5.gamma1.2.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.2.alloverlap)&
                                                         (ci.n1000.delta0.5.gamma1.2.alloverlap[,2*kk]>=true.delta0.5.gamma1.2.alloverlap))
  ci.length.n1000.delta0.5.gamma1.5.alloverlap=cbind(ci.length.n1000.delta0.5.gamma1.5.alloverlap,
                                                     ci.n1000.delta0.5.gamma1.5.alloverlap[,2*kk]-ci.n1000.delta0.5.gamma1.5.alloverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.5.alloverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.5.alloverlap,
                                                       (ci.n1000.delta0.5.gamma1.5.alloverlap[,2*kk-1]<=true.delta0.5.gamma1.5.alloverlap)&
                                                         (ci.n1000.delta0.5.gamma1.5.alloverlap[,2*kk]>=true.delta0.5.gamma1.5.alloverlap))
}


ci.length.n1000.delta0.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.5.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.5.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.5.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.5.majorityoverlap=c()
for (kk in 1:3){
  ci.length.n1000.delta0.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.majorityoverlap,
                                                      ci.n1000.delta0.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.majorityoverlap,
                                                        (ci.n1000.delta0.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.majorityoverlap)&
                                                          (ci.n1000.delta0.gamma1.majorityoverlap[,2*kk]>=true.delta0.gamma1.majorityoverlap))
  ci.length.n1000.delta0.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.2.majorityoverlap,
                                                        ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.2.majorityoverlap,
                                                          (ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.5.majorityoverlap,
                                                        ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.5.majorityoverlap,
                                                          (ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.gamma1.5.majorityoverlap)&
                                                            (ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.gamma1.5.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.majorityoverlap,
                                                        ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.majorityoverlap,
                                                          (ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.majorityoverlap)&
                                                            (ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.2.majorityoverlap,
                                                          ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap,
                                                            (ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.5.majorityoverlap,
                                                          ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap,
                                                            (ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.5.majorityoverlap)&
                                                              (ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.5.majorityoverlap))
  ci.length.n1000.delta0.5.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.5.gamma1.majorityoverlap,
                                                        ci.n1000.delta0.5.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.majorityoverlap,
                                                          (ci.n1000.delta0.5.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.majorityoverlap)&
                                                            (ci.n1000.delta0.5.gamma1.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.majorityoverlap))
  ci.length.n1000.delta0.5.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.5.gamma1.2.majorityoverlap,
                                                          ci.n1000.delta0.5.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.2.majorityoverlap,
                                                            (ci.n1000.delta0.5.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.5.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.5.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.5.gamma1.5.majorityoverlap,
                                                          ci.n1000.delta0.5.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.5.majorityoverlap,
                                                            (ci.n1000.delta0.5.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.5.gamma1.5.majorityoverlap)&
                                                              (ci.n1000.delta0.5.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.5.gamma1.5.majorityoverlap))
}


ci.length.n1000.delta0.gamma1.limitedoverlap=c()
ci.coverage.n1000.delta0.gamma1.limitedoverlap=c()
ci.length.n1000.delta0.gamma1.2.limitedoverlap=c()
ci.coverage.n1000.delta0.gamma1.2.limitedoverlap=c()
ci.length.n1000.delta0.gamma1.5.limitedoverlap=c()
ci.coverage.n1000.delta0.gamma1.5.limitedoverlap=c()
ci.length.n1000.delta0.2.gamma1.limitedoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.limitedoverlap=c()
ci.length.n1000.delta0.2.gamma1.2.limitedoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.2.limitedoverlap=c()
ci.length.n1000.delta0.2.gamma1.5.limitedoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.5.limitedoverlap=c()
ci.length.n1000.delta0.5.gamma1.limitedoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.limitedoverlap=c()
ci.length.n1000.delta0.5.gamma1.2.limitedoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.2.limitedoverlap=c()
ci.length.n1000.delta0.5.gamma1.5.limitedoverlap=c()
ci.coverage.n1000.delta0.5.gamma1.5.limitedoverlap=c()
for (kk in 1:3){
  ci.length.n1000.delta0.gamma1.limitedoverlap=cbind(ci.length.n1000.delta0.gamma1.limitedoverlap,
                                                     ci.n1000.delta0.gamma1.limitedoverlap[,2*kk]-ci.n1000.delta0.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.limitedoverlap=cbind(ci.coverage.n1000.delta0.gamma1.limitedoverlap,
                                                       (ci.n1000.delta0.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.limitedoverlap)&
                                                         (ci.n1000.delta0.gamma1.limitedoverlap[,2*kk]>=true.delta0.gamma1.limitedoverlap))
  ci.length.n1000.delta0.gamma1.2.limitedoverlap=cbind(ci.length.n1000.delta0.gamma1.2.limitedoverlap,
                                                       ci.n1000.delta0.gamma1.2.limitedoverlap[,2*kk]-ci.n1000.delta0.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.2.limitedoverlap=cbind(ci.coverage.n1000.delta0.gamma1.2.limitedoverlap,
                                                         (ci.n1000.delta0.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.2.limitedoverlap)&
                                                           (ci.n1000.delta0.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.gamma1.2.limitedoverlap))
  ci.length.n1000.delta0.gamma1.5.limitedoverlap=cbind(ci.length.n1000.delta0.gamma1.5.limitedoverlap,
                                                       ci.n1000.delta0.gamma1.5.limitedoverlap[,2*kk]-ci.n1000.delta0.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.5.limitedoverlap=cbind(ci.coverage.n1000.delta0.gamma1.5.limitedoverlap,
                                                         (ci.n1000.delta0.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.gamma1.5.limitedoverlap)&
                                                           (ci.n1000.delta0.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.gamma1.5.limitedoverlap))
  ci.length.n1000.delta0.2.gamma1.limitedoverlap=cbind(ci.length.n1000.delta0.2.gamma1.limitedoverlap,
                                                       ci.n1000.delta0.2.gamma1.limitedoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.limitedoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.limitedoverlap,
                                                         (ci.n1000.delta0.2.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.limitedoverlap)&
                                                           (ci.n1000.delta0.2.gamma1.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.limitedoverlap))
  ci.length.n1000.delta0.2.gamma1.2.limitedoverlap=cbind(ci.length.n1000.delta0.2.gamma1.2.limitedoverlap,
                                                         ci.n1000.delta0.2.gamma1.2.limitedoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.2.limitedoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.2.limitedoverlap,
                                                           (ci.n1000.delta0.2.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.limitedoverlap)&
                                                             (ci.n1000.delta0.2.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.2.limitedoverlap))
  ci.length.n1000.delta0.2.gamma1.5.limitedoverlap=cbind(ci.length.n1000.delta0.2.gamma1.5.limitedoverlap,
                                                         ci.n1000.delta0.2.gamma1.5.limitedoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.5.limitedoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.5.limitedoverlap,
                                                           (ci.n1000.delta0.2.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.2.gamma1.5.limitedoverlap)&
                                                             (ci.n1000.delta0.2.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.2.gamma1.5.limitedoverlap))
  ci.length.n1000.delta0.5.gamma1.limitedoverlap=cbind(ci.length.n1000.delta0.5.gamma1.limitedoverlap,
                                                       ci.n1000.delta0.5.gamma1.limitedoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.limitedoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.limitedoverlap,
                                                         (ci.n1000.delta0.5.gamma1.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.limitedoverlap)&
                                                           (ci.n1000.delta0.5.gamma1.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.limitedoverlap))
  ci.length.n1000.delta0.5.gamma1.2.limitedoverlap=cbind(ci.length.n1000.delta0.5.gamma1.2.limitedoverlap,
                                                         ci.n1000.delta0.5.gamma1.2.limitedoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.2.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.2.limitedoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.2.limitedoverlap,
                                                           (ci.n1000.delta0.5.gamma1.2.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.2.limitedoverlap)&
                                                             (ci.n1000.delta0.5.gamma1.2.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.2.limitedoverlap))
  ci.length.n1000.delta0.5.gamma1.5.limitedoverlap=cbind(ci.length.n1000.delta0.5.gamma1.5.limitedoverlap,
                                                         ci.n1000.delta0.5.gamma1.5.limitedoverlap[,2*kk]-ci.n1000.delta0.5.gamma1.5.limitedoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.5.gamma1.5.limitedoverlap=cbind(ci.coverage.n1000.delta0.5.gamma1.5.limitedoverlap,
                                                           (ci.n1000.delta0.5.gamma1.5.limitedoverlap[,2*kk-1]<=true.delta0.5.gamma1.5.limitedoverlap)&
                                                             (ci.n1000.delta0.5.gamma1.5.limitedoverlap[,2*kk]>=true.delta0.5.gamma1.5.limitedoverlap))
}


tb.coverage1000=rbind(c(apply(ci.coverage.n1000.delta0.gamma1.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.gamma1.2.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.2.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.2.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.gamma1.5.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.5.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.gamma1.5.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.2.gamma1.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.2.gamma1.2.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.2.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.2.gamma1.5.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.2.gamma1.5.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.5.gamma1.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.5.gamma1.2.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.2.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.2.limitedoverlap,2,mean)),
                      c(apply(ci.coverage.n1000.delta0.5.gamma1.5.alloverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.5.majorityoverlap,2,mean),
                        apply(ci.coverage.n1000.delta0.5.gamma1.5.limitedoverlap,2,mean)))
xtable::xtable(tb.coverage1000)


tb.length1000=rbind(c(apply(ci.length.n1000.delta0.gamma1.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.gamma1.2.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.2.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.2.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.gamma1.5.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.5.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.gamma1.5.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.2.gamma1.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.2.gamma1.2.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.2.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.2.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.2.gamma1.5.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.5.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.2.gamma1.5.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.5.gamma1.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.5.gamma1.2.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.2.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.2.limitedoverlap,2,mean)),
                    c(apply(ci.length.n1000.delta0.5.gamma1.5.alloverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.5.majorityoverlap,2,mean),
                      apply(ci.length.n1000.delta0.5.gamma1.5.limitedoverlap,2,mean)))
xtable::xtable(tb.length1000)




###figures
# Figure 2
n1000.delta0.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.gamma1.majorityoverlap.csv',sep=''))
ci.n1000.delta0.gamma1.majorityoverlap=n1000.delta0.gamma1.majorityoverlap[,2:7]

n1000.delta0.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.gamma1.2.majorityoverlap.csv',sep=''))
ci.n1000.delta0.gamma1.2.majorityoverlap=n1000.delta0.gamma1.2.majorityoverlap[,2:7]

n1000.delta0.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.gamma1.5.majorityoverlap.csv',sep=''))
ci.n1000.delta0.gamma1.5.majorityoverlap=n1000.delta0.gamma1.5.majorityoverlap[,2:7]

n1000.delta0.gamma1.8.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.gamma1.8.majorityoverlap.csv',sep=''))
ci.n1000.delta0.gamma1.8.majorityoverlap=n1000.delta0.gamma1.8.majorityoverlap[,2:7]

n1000.delta0.2.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.2.gamma1.majorityoverlap.csv',sep=''))
ci.n1000.delta0.2.gamma1.majorityoverlap=n1000.delta0.2.gamma1.majorityoverlap[,2:7]

n1000.delta0.2.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.2.gamma1.2.majorityoverlap.csv',sep=''))
ci.n1000.delta0.2.gamma1.2.majorityoverlap=n1000.delta0.2.gamma1.2.majorityoverlap[,2:7]

n1000.delta0.2.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.2.gamma1.5.majorityoverlap.csv',sep=''))
ci.n1000.delta0.2.gamma1.5.majorityoverlap=n1000.delta0.2.gamma1.5.majorityoverlap[,2:7]

n1000.delta0.2.gamma1.8.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.2.gamma1.8.majorityoverlap.csv',sep=''))
ci.n1000.delta0.2.gamma1.8.majorityoverlap=n1000.delta0.2.gamma1.8.majorityoverlap[,2:7]

n1000.delta0.4.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.4.gamma1.majorityoverlap.csv',sep=''))
ci.n1000.delta0.4.gamma1.majorityoverlap=n1000.delta0.4.gamma1.majorityoverlap[,2:7]

n1000.delta0.4.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.4.gamma1.2.majorityoverlap.csv',sep=''))
ci.n1000.delta0.4.gamma1.2.majorityoverlap=n1000.delta0.4.gamma1.2.majorityoverlap[,2:7]

n1000.delta0.4.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.4.gamma1.5.majorityoverlap.csv',sep=''))
ci.n1000.delta0.4.gamma1.5.majorityoverlap=n1000.delta0.4.gamma1.5.majorityoverlap[,2:7]

n1000.delta0.4.gamma1.8.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.4.gamma1.8.majorityoverlap.csv',sep=''))
ci.n1000.delta0.4.gamma1.8.majorityoverlap=n1000.delta0.4.gamma1.8.majorityoverlap[,2:7]

n1000.delta0.6.gamma1.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.6.gamma1.majorityoverlap.csv',sep=''))
ci.n1000.delta0.6.gamma1.majorityoverlap=n1000.delta0.6.gamma1.majorityoverlap[,2:7]

n1000.delta0.6.gamma1.2.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.6.gamma1.2.majorityoverlap.csv',sep=''))
ci.n1000.delta0.6.gamma1.2.majorityoverlap=n1000.delta0.6.gamma1.2.majorityoverlap[,2:7]

n1000.delta0.6.gamma1.5.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.6.gamma1.5.majorityoverlap.csv',sep=''))
ci.n1000.delta0.6.gamma1.5.majorityoverlap=n1000.delta0.6.gamma1.5.majorityoverlap[,2:7]

n1000.delta0.6.gamma1.8.majorityoverlap=read.csv(paste(dir,'sim_results/results.n1000.delta0.6.gamma1.8.majorityoverlap.csv',sep=''))
ci.n1000.delta0.6.gamma1.8.majorityoverlap=n1000.delta0.6.gamma1.8.majorityoverlap[,2:7]


ci.length.n1000.delta0.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.gamma1.8.majorityoverlap=c()
ci.coverage.n1000.delta0.gamma1.8.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.2.gamma1.8.majorityoverlap=c()
ci.coverage.n1000.delta0.2.gamma1.8.majorityoverlap=c()
ci.length.n1000.delta0.4.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.4.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.4.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.4.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.4.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.4.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.4.gamma1.8.majorityoverlap=c()
ci.coverage.n1000.delta0.4.gamma1.8.majorityoverlap=c()
ci.length.n1000.delta0.6.gamma1.majorityoverlap=c()
ci.coverage.n1000.delta0.6.gamma1.majorityoverlap=c()
ci.length.n1000.delta0.6.gamma1.2.majorityoverlap=c()
ci.coverage.n1000.delta0.6.gamma1.2.majorityoverlap=c()
ci.length.n1000.delta0.6.gamma1.5.majorityoverlap=c()
ci.coverage.n1000.delta0.6.gamma1.5.majorityoverlap=c()
ci.length.n1000.delta0.6.gamma1.8.majorityoverlap=c()
ci.coverage.n1000.delta0.6.gamma1.8.majorityoverlap=c()
for (kk in 1:3){
  ci.length.n1000.delta0.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.majorityoverlap,
                                                      ci.n1000.delta0.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.majorityoverlap,
                                                        (ci.n1000.delta0.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                          (ci.n1000.delta0.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.2.majorityoverlap,
                                                        ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.2.majorityoverlap,
                                                          (ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.5.majorityoverlap,
                                                        ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.5.majorityoverlap,
                                                          (ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.gamma1.8.majorityoverlap=cbind(ci.length.n1000.delta0.gamma1.8.majorityoverlap,
                                                        ci.n1000.delta0.gamma1.8.majorityoverlap[,2*kk]-ci.n1000.delta0.gamma1.8.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.gamma1.8.majorityoverlap=cbind(ci.coverage.n1000.delta0.gamma1.8.majorityoverlap,
                                                          (ci.n1000.delta0.gamma1.8.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.gamma1.8.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  
  ci.length.n1000.delta0.2.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.majorityoverlap,
                                                        ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.majorityoverlap,
                                                          (ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.2.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.2.majorityoverlap,
                                                          ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap,
                                                            (ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.2.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.5.majorityoverlap,
                                                          ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap,
                                                            (ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.2.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.2.gamma1.8.majorityoverlap=cbind(ci.length.n1000.delta0.2.gamma1.8.majorityoverlap,
                                                          ci.n1000.delta0.2.gamma1.8.majorityoverlap[,2*kk]-ci.n1000.delta0.2.gamma1.8.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.2.gamma1.8.majorityoverlap=cbind(ci.coverage.n1000.delta0.2.gamma1.8.majorityoverlap,
                                                            (ci.n1000.delta0.2.gamma1.8.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.2.gamma1.8.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  
  ci.length.n1000.delta0.4.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.4.gamma1.majorityoverlap,
                                                        ci.n1000.delta0.4.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.4.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.4.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.4.gamma1.majorityoverlap,
                                                          (ci.n1000.delta0.4.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.4.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.4.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.4.gamma1.2.majorityoverlap,
                                                          ci.n1000.delta0.4.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.4.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.4.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.4.gamma1.2.majorityoverlap,
                                                            (ci.n1000.delta0.4.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.4.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.4.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.4.gamma1.5.majorityoverlap,
                                                          ci.n1000.delta0.4.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.4.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.4.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.4.gamma1.5.majorityoverlap,
                                                            (ci.n1000.delta0.4.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.4.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.4.gamma1.8.majorityoverlap=cbind(ci.length.n1000.delta0.4.gamma1.8.majorityoverlap,
                                                          ci.n1000.delta0.4.gamma1.8.majorityoverlap[,2*kk]-ci.n1000.delta0.4.gamma1.8.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.4.gamma1.8.majorityoverlap=cbind(ci.coverage.n1000.delta0.4.gamma1.8.majorityoverlap,
                                                            (ci.n1000.delta0.4.gamma1.8.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.4.gamma1.8.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  
  ci.length.n1000.delta0.6.gamma1.majorityoverlap=cbind(ci.length.n1000.delta0.6.gamma1.majorityoverlap,
                                                        ci.n1000.delta0.6.gamma1.majorityoverlap[,2*kk]-ci.n1000.delta0.6.gamma1.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.6.gamma1.majorityoverlap=cbind(ci.coverage.n1000.delta0.6.gamma1.majorityoverlap,
                                                          (ci.n1000.delta0.6.gamma1.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                            (ci.n1000.delta0.6.gamma1.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.6.gamma1.2.majorityoverlap=cbind(ci.length.n1000.delta0.6.gamma1.2.majorityoverlap,
                                                          ci.n1000.delta0.6.gamma1.2.majorityoverlap[,2*kk]-ci.n1000.delta0.6.gamma1.2.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.6.gamma1.2.majorityoverlap=cbind(ci.coverage.n1000.delta0.6.gamma1.2.majorityoverlap,
                                                            (ci.n1000.delta0.6.gamma1.2.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.6.gamma1.2.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.6.gamma1.5.majorityoverlap=cbind(ci.length.n1000.delta0.6.gamma1.5.majorityoverlap,
                                                          ci.n1000.delta0.6.gamma1.5.majorityoverlap[,2*kk]-ci.n1000.delta0.6.gamma1.5.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.6.gamma1.5.majorityoverlap=cbind(ci.coverage.n1000.delta0.6.gamma1.5.majorityoverlap,
                                                            (ci.n1000.delta0.6.gamma1.5.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.6.gamma1.5.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
  ci.length.n1000.delta0.6.gamma1.8.majorityoverlap=cbind(ci.length.n1000.delta0.6.gamma1.8.majorityoverlap,
                                                          ci.n1000.delta0.6.gamma1.8.majorityoverlap[,2*kk]-ci.n1000.delta0.6.gamma1.8.majorityoverlap[,2*kk-1])
  ci.coverage.n1000.delta0.6.gamma1.8.majorityoverlap=cbind(ci.coverage.n1000.delta0.6.gamma1.8.majorityoverlap,
                                                            (ci.n1000.delta0.6.gamma1.8.majorityoverlap[,2*kk-1]<=true.delta0.2.gamma1.2.majorityoverlap)&
                                                              (ci.n1000.delta0.6.gamma1.8.majorityoverlap[,2*kk]>=true.delta0.2.gamma1.2.majorityoverlap))
}


ci.coverage=cbind(ci.coverage.n1000.delta0.gamma1.majorityoverlap,
                  ci.coverage.n1000.delta0.gamma1.2.majorityoverlap,
                  ci.coverage.n1000.delta0.gamma1.5.majorityoverlap,
                  ci.coverage.n1000.delta0.gamma1.8.majorityoverlap,
                  ci.coverage.n1000.delta0.2.gamma1.majorityoverlap,
                  ci.coverage.n1000.delta0.2.gamma1.2.majorityoverlap,
                  ci.coverage.n1000.delta0.2.gamma1.5.majorityoverlap,
                  ci.coverage.n1000.delta0.2.gamma1.8.majorityoverlap,
                  ci.coverage.n1000.delta0.4.gamma1.majorityoverlap,
                  ci.coverage.n1000.delta0.4.gamma1.2.majorityoverlap,
                  ci.coverage.n1000.delta0.4.gamma1.5.majorityoverlap,
                  ci.coverage.n1000.delta0.4.gamma1.8.majorityoverlap,
                  ci.coverage.n1000.delta0.6.gamma1.majorityoverlap,
                  ci.coverage.n1000.delta0.6.gamma1.2.majorityoverlap,
                  ci.coverage.n1000.delta0.6.gamma1.5.majorityoverlap,
                  ci.coverage.n1000.delta0.6.gamma1.8.majorityoverlap)
ci.length=cbind(ci.length.n1000.delta0.gamma1.majorityoverlap,
                  ci.length.n1000.delta0.gamma1.2.majorityoverlap,
                  ci.length.n1000.delta0.gamma1.5.majorityoverlap,
                  ci.length.n1000.delta0.gamma1.8.majorityoverlap,
                  ci.length.n1000.delta0.2.gamma1.majorityoverlap,
                  ci.length.n1000.delta0.2.gamma1.2.majorityoverlap,
                  ci.length.n1000.delta0.2.gamma1.5.majorityoverlap,
                  ci.length.n1000.delta0.2.gamma1.8.majorityoverlap,
                  ci.length.n1000.delta0.4.gamma1.majorityoverlap,
                  ci.length.n1000.delta0.4.gamma1.2.majorityoverlap,
                  ci.length.n1000.delta0.4.gamma1.5.majorityoverlap,
                  ci.length.n1000.delta0.4.gamma1.8.majorityoverlap,
                  ci.length.n1000.delta0.6.gamma1.majorityoverlap,
                  ci.length.n1000.delta0.6.gamma1.2.majorityoverlap,
                  ci.length.n1000.delta0.6.gamma1.5.majorityoverlap,
                  ci.length.n1000.delta0.6.gamma1.8.majorityoverlap)
ci.coverage=apply(ci.coverage,2,mean)
ci.length=apply(ci.length,2,mean)

trydelta=c(0,0.2,0.4,0.6)
trygamma=c(1,1.2,1.5,1.8)
plot.df=data.frame(Coverage=ci.coverage,Length=ci.length,
                  Method=rep(c('RCT','OS','Combined'),length(trygamma)*length(trydelta)),
                  Delta=rep(trydelta,each=3*length(trygamma)),
                  Gamma=rep(rep(trygamma,each=3),length(trydelta)))
plot.df$Delta=as.factor(plot.df$Delta)
plot.df$Gamma=as.factor(plot.df$Gamma)

p1=ggplot(plot.df, aes(y=Delta,x=Gamma,fill=Coverage,label = Coverage))+
  geom_tile()+
  geom_text(col = "black",na.rm=T) +
  scale_fill_gradient(high = "#547298",low = "#DADEE7",na.value = "white") +
  xlab("Gamma") + 
  facet_wrap(~Method)+
  ylab("Delta")+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        legend.position="none")+
  coord_fixed(ratio = 0.6)+
  ggtitle('Coverage Probability of 95% Confidence Interval')+
  theme(plot.title = element_text(hjust = 0.5))
p2=ggplot(plot.df, aes(y=Delta,x=Gamma,fill=Length,label = round(Length,3)))+
  geom_tile()+
  geom_text(col = "black",na.rm=T) +
  scale_fill_gradient(high = "#547298",low = "#DADEE7",na.value = "white") +
  xlab("Gamma") + 
  facet_wrap(~Method)+
  ylab("Delta")+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        legend.position="none")+
  coord_fixed(ratio = 0.6)+
  ggtitle('Average Lenge of 95% Confidence Interval')+
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(p1,p2,ncol=1)



### Figure 3: power analysis
dlist=c(0,0.2,0.4,0.6)
glist=c(1,1.2,1.5,1.8)
tlist=c(0,0.2,0.4,0.6,0.8,1)
power.delta0.gamma1=c()
power.delta0.gamma1.2=c()
power.delta0.gamma1.5=c()
power.delta0.gamma1.8=c()
power.delta0.2.gamma1=c()
power.delta0.2.gamma1.2=c()
power.delta0.2.gamma1.5=c()
power.delta0.2.gamma1.8=c()
power.delta0.4.gamma1=c()
power.delta0.4.gamma1.2=c()
power.delta0.4.gamma1.5=c()
power.delta0.4.gamma1.8=c()
power.delta0.6.gamma1=c()
power.delta0.6.gamma1.2=c()
power.delta0.6.gamma1.5=c()
power.delta0.6.gamma1.8=c()
for (tau in tlist){
  print(tau)
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.gamma1.effect',tau,'.csv',sep=''))
  power.results.delta0.gamma1=read.csv(path)
  power.results.delta0.gamma1=power.results.delta0.gamma1[,2:4]
  power.delta0.gamma1=c(power.delta0.gamma1,apply(power.results.delta0.gamma1,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.gamma1.2.effect',tau,'.csv',sep=''))
  power.results.delta0.gamma1.2=read.csv(path)
  power.results.delta0.gamma1.2=power.results.delta0.gamma1.2[,2:4]
  power.delta0.gamma1.2=c(power.delta0.gamma1.2,apply(power.results.delta0.gamma1.2,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.gamma1.5.effect',tau,'.csv',sep=''))
  power.results.delta0.gamma1.5=read.csv(path)
  power.results.delta0.gamma1.5=power.results.delta0.gamma1.5[,2:4]
  power.delta0.gamma1.5=c(power.delta0.gamma1.5,apply(power.results.delta0.gamma1.5,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.gamma1.8.effect',tau,'.csv',sep=''))
  power.results.delta0.gamma1.8=read.csv(path)
  power.results.delta0.gamma1.8=power.results.delta0.gamma1.8[,2:4]
  power.delta0.gamma1.8=c(power.delta0.gamma1.8,apply(power.results.delta0.gamma1.8,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.2.gamma1.effect',tau,'.csv',sep=''))
  power.results.delta0.2.gamma1=read.csv(path)
  power.results.delta0.2.gamma1=power.results.delta0.2.gamma1[,2:4]
  power.delta0.2.gamma1=c(power.delta0.2.gamma1,apply(power.results.delta0.2.gamma1,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.2.gamma1.2.effect',tau,'.csv',sep=''))
  power.results.delta0.2.gamma1.2=read.csv(path)
  power.results.delta0.2.gamma1.2=power.results.delta0.2.gamma1.2[,2:4]
  power.delta0.2.gamma1.2=c(power.delta0.2.gamma1.2,apply(power.results.delta0.2.gamma1.2,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.2.gamma1.5.effect',tau,'.csv',sep=''))
  power.results.delta0.2.gamma1.5=read.csv(path)
  power.results.delta0.2.gamma1.5=power.results.delta0.2.gamma1.5[,2:4]
  power.delta0.2.gamma1.5=c(power.delta0.2.gamma1.5,apply(power.results.delta0.2.gamma1.5,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.2.gamma1.8.effect',tau,'.csv',sep=''))
  power.results.delta0.2.gamma1.8=read.csv(path)
  power.results.delta0.2.gamma1.8=power.results.delta0.2.gamma1.8[,2:4]
  power.delta0.2.gamma1.8=c(power.delta0.2.gamma1.8,apply(power.results.delta0.2.gamma1.8,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.4.gamma1.effect',tau,'.csv',sep=''))
  power.results.delta0.4.gamma1=read.csv(path)
  power.results.delta0.4.gamma1=power.results.delta0.4.gamma1[,2:4]
  power.delta0.4.gamma1=c(power.delta0.4.gamma1,apply(power.results.delta0.4.gamma1,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.4.gamma1.2.effect',tau,'.csv',sep=''))
  power.results.delta0.4.gamma1.2=read.csv(path)
  power.results.delta0.4.gamma1.2=power.results.delta0.4.gamma1.2[,2:4]
  power.delta0.4.gamma1.2=c(power.delta0.4.gamma1.2,apply(power.results.delta0.4.gamma1.2,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.4.gamma1.5.effect',tau,'.csv',sep=''))
  power.results.delta0.4.gamma1.5=read.csv(path)
  power.results.delta0.4.gamma1.5=power.results.delta0.4.gamma1.5[,2:4]
  power.delta0.4.gamma1.5=c(power.delta0.4.gamma1.5,apply(power.results.delta0.4.gamma1.5,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.4.gamma1.8.effect',tau,'.csv',sep=''))
  power.results.delta0.4.gamma1.8=read.csv(path)
  power.results.delta0.4.gamma1.8=power.results.delta0.4.gamma1.8[,2:4]
  power.delta0.4.gamma1.8=c(power.delta0.4.gamma1.8,apply(power.results.delta0.4.gamma1.8,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.6.gamma1.effect',tau,'.csv',sep=''))
  power.results.delta0.6.gamma1=read.csv(path)
  power.results.delta0.6.gamma1=power.results.delta0.6.gamma1[,2:4]
  power.delta0.6.gamma1=c(power.delta0.6.gamma1,apply(power.results.delta0.6.gamma1,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.6.gamma1.2.effect',tau,'.csv',sep=''))
  power.results.delta0.6.gamma1.2=read.csv(path)
  power.results.delta0.6.gamma1.2=power.results.delta0.6.gamma1.2[,2:4]
  power.delta0.6.gamma1.2=c(power.delta0.6.gamma1.2,apply(power.results.delta0.6.gamma1.2,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.6.gamma1.5.effect',tau,'.csv',sep=''))
  power.results.delta0.6.gamma1.5=read.csv(path)
  power.results.delta0.6.gamma1.5=power.results.delta0.6.gamma1.5[,2:4]
  power.delta0.6.gamma1.5=c(power.delta0.6.gamma1.5,apply(power.results.delta0.6.gamma1.5,2,function(v) mean(v<0.05)))
  
  path=paste(paste(dir,'sim_results/power.results.n1000.delta0.6.gamma1.8.effect',tau,'.csv',sep=''))
  power.results.delta0.6.gamma1.8=read.csv(path)
  power.results.delta0.6.gamma1.8=power.results.delta0.6.gamma1.8[,2:4]
  power.delta0.6.gamma1.8=c(power.delta0.6.gamma1.8,apply(power.results.delta0.6.gamma1.8,2,function(v) mean(v<0.05)))
}

power=c(power.delta0.gamma1,power.delta0.gamma1.2,power.delta0.gamma1.5,power.delta0.gamma1.8,
        power.delta0.2.gamma1,power.delta0.2.gamma1.2,power.delta0.2.gamma1.5,power.delta0.2.gamma1.8,
        power.delta0.4.gamma1,power.delta0.4.gamma1.2,power.delta0.4.gamma1.5,power.delta0.4.gamma1.8,
        power.delta0.6.gamma1,power.delta0.6.gamma1.2,power.delta0.6.gamma1.5,power.delta0.6.gamma1.8)
effect=rep(rep(tlist,each=3),length(dlist)*length(glist))
delta=rep(c('Delta=0','Delta=0.2','Delta=0.4','Delta=0.6'),each=3*length(tlist)*length(glist))
gamma=rep(rep(c('Gamma=1','Gamma=1.2','Gamma=1.5','Gamma=1.8'),each=3*length(tlist)),length(dlist))
method=rep(c('RCT','OS','Combined'),length(tlist)*length(dlist)*length(glist))
plot.df=data.frame(Power=power,
                   Effect=effect,
                   Gamma=gamma,
                   Delta=delta,
                   Method=method)
ggplot(data=plot.df, aes(x=Effect, y=Power, col=Method,linetype=Method)) +
  geom_line(stat="identity") +
  facet_grid(vars(Delta), vars(Gamma)) +
  xlab("Effect") + 
  ylab("Measure") +
  scale_fill_brewer(palette="Spectral") +
  theme_bw()+
  scale_x_continuous(breaks = sort(c(0,0.2,0.4,0.6,0.8,1)))+
  scale_y_continuous(breaks = sort(c(0,0.05,0.25,0.5,0.75,0.95,1)))+
  geom_hline(yintercept=0.95, linetype="dashed", size=0.2)+
  geom_hline(yintercept=0.05, linetype="dashed", size=0.2)



