
numKM=function(sfit,timeby=20,xlab="Time",ylab="Survival Probability",col=NULL,g.names=NULL){

  ngroup=length(sfit$strata)

  if(ngroup>5){
    message("Groups >5 is not supported by the current function!")
  }else{

    if(is.null(g.names)){g.names=names(sfit$strata)}
    if(length(g.names)!=ngroup){
      message("The length of g.names is not equal to the number of groups!")
      g.names=names(sfit$strata)
    }

    if(max(nchar(g.names))>nchar("Number at risk")){mai2=1.8}else{mai2=1.5}

    times=seq(0,max(sfit$time)+timeby,timeby)
    nrisk=summary(sfit,times=times,extend=TRUE)$n.risk
    if(max(nrisk)>100){gx=0.5}else{gx=0.3}

    if(ngroup<=3){mai=c(2,mai2,0.5,0.5)}
    if(ngroup==4){mai=c(2.2,mai2,0.5,0.5)}
    if(ngroup==5){mai=c(2.4,mai2,0.5,0.5)}

    if(is.null(col)){col=1:ngroup}

    par(mai=mai)
    plot(sfit,col=col,ylab=ylab,xlab=xlab,axe=FALSE,xlim=c(0,max(times)))
    axis(1,at=times);axis(2)

    if(ngroup==1){
      mtext("Number at risk",side=1,at=-timeby*gx,line=4,adj=1)
      mtext(g.names[1],side=1,at=-timeby*gx,line=5,adj=1)
      for(i in 1:length(times)){mtext(nrisk[i],side=1,at=times[i],line=5)}
    }
    if(ngroup==2){
      mtext("Number at risk",side=1,at=-timeby*gx,line=4,adj=1)
      mtext(g.names[1],side=1,at=-timeby*gx,line=5,adj=1)
      mtext(g.names[2],side=1,at=-timeby*gx,line=6,adj=1)
      for(i in 1:length(times)){mtext(nrisk[i],side=1,at=times[i],line=5)}
      for(i in (length(times)+1):(2*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=6)
      }
    }
    if(ngroup==3){
      mtext("Number at risk",side=1,at=-timeby*gx,line=4,adj=1)
      mtext(g.names[1],side=1,at=-timeby*gx,line=5,adj=1)
      mtext(g.names[2],side=1,at=-timeby*gx,line=6,adj=1)
      mtext(g.names[3],side=1,at=-timeby*gx,line=7,adj=1)
      for(i in 1:length(times)){mtext(nrisk[i],side=1,at=times[i],line=5)}
      for(i in (length(times)+1):(2*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=6)
      }
      for(i in (2*length(times)+1):(3*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=7)
      }
    }
    if(ngroup==4){
      mtext("Number at risk",side=1,at=-timeby*gx,line=4,adj=1)
      mtext(g.names[1],side=1,at=-timeby*gx,line=5,adj=1)
      mtext(g.names[2],side=1,at=-timeby*gx,line=6,adj=1)
      mtext(g.names[3],side=1,at=-timeby*gx,line=7,adj=1)
      mtext(g.names[4],side=1,at=-timeby*gx,line=8,adj=1)
      for(i in 1:length(times)){mtext(nrisk[i],side=1,at=times[i],line=5)}
      for(i in (length(times)+1):(2*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=6)
      }
      for(i in (2*length(times)+1):(3*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=7)
      }
      for(i in (3*length(times)+1):(4*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=8)
      }
    }
    if(ngroup==5){
      mtext("Number at risk",side=1,at=-timeby*gx,line=4,adj=1)
      mtext(g.names[1],side=1,at=-timeby*gx,line=5,adj=1)
      mtext(g.names[2],side=1,at=-timeby*gx,line=6,adj=1)
      mtext(g.names[3],side=1,at=-timeby*gx,line=7,adj=1)
      mtext(g.names[4],side=1,at=-timeby*gx,line=8,adj=1)
      mtext(g.names[5],side=1,at=-timeby*gx,line=9,adj=1)
      for(i in 1:length(times)){mtext(nrisk[i],side=1,at=times[i],line=5)}
      for(i in (length(times)+1):(2*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=6)
      }
      for(i in (2*length(times)+1):(3*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=7)
      }
      for(i in (3*length(times)+1):(4*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=8)
      }
      for(i in (4*length(times)+1):(5*length(times))){
        mtext(nrisk[i],side=1,at=rep(times,ngroup)[i],line=9)
      }
    }

    legend("bottomright",lty=1,col=1:3,legend=g.names,bty="n")

    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)
    p <- ifelse(pval < 0.001, "p < 0.001", paste("p =", round(pval, 3)))

    legend("bottomleft",lty=1,col="white",legend=p,bty="n")

  }#if(ngroup>5)

}
