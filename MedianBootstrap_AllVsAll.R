set.seed(20230226)
source("https://raw.githubusercontent.com/faulknerfalcons/Johnston-2020-Bootstrap/master/medianBootstrapToolbox.R")

# dummy data
col<-c(100,101,23,14,15,20)
nhl <- c(0,1,21,15,17,20)
pdlp <- c(0,1,2,4,1,0,0,0,1,0,0)
pdlp_nhl <-c(0,1,2,4,1,0,10,0,1,0,0)
# control vs all 
medianBootstraps(col,nhl,pdlp,pdlp_nhl)

# Edit to make an all-vs-all comparison controlled by Holm correction
medianBootstraps2 <- function(..., N=5000, alpha=0.05){
  x<-list(...)
  comparisons <- (length(x)-1)*length(x)/2
  results2<-NULL
   for (j in 1:(length(x)-1)){
    results<-c(rep(NA,j-1),1)
    reference<-x[[1]]
      for(i in 2:(length(x))){
        ## Calculate observed test statistic
        mediandiff<-median(reference)-median(x[[i]])
        ## Generate the null distribution
        boots<-replicate(N, median(sample(reference,length(reference), replace=T))-median(sample(x[[i]],length(x[[i]]), replace=T))-mediandiff)
        ## Count the number of at resampled observations which are at least as extreme
        above <- sum(abs(boots)>=abs(mediandiff))
        ## Calculate p value and confidence intervals
        mcp<-mcp_ci(above+1,N+1, alpha)
        results<-c(results, mcp[1])
      }
    x <- x[-1]
    results2 <- rbind(results2, results)
   }
  ## Calculate observed test statistics
  results2<-rbind(results2,results=c(rep(NA, dim(results2)[2]-1),1))
  return(matrix(p.adjust(results2), nrow=sqrt(length(p.adjust(results2)))))
}

# a little bit manual at the end to make the output easier to read
median(col)
median(nhl)
median(pdlp)
median(pdlp_nhl)

# to work out the letter groups, it's easiest to have in order
# pdlp
# pdlp_nhl
# nhl
# col

matrix <- medianBootstraps2(pdlp,pdlp_nhl,nhl,col)
names <- c('pdlp','pdlp_nhl','nhl','col')
dimnames(matrix)<-list(names,names)
matrix