runskmeans <- function(x, knum) {
  sk <- skmeans(as.matrix(x), k=knum, control=list(verbose=TRUE))
  skmat <- as.matrix(sk$cluster)
  row.names(skmat) <- row.names(x)
  as.matrix(skmat[order(skmat[,1]),])
}

runcluto <- function(x, knum) {
  cluto <- skmeans(x, k=knum, method="CLUTO", control=list(vcluster="/home/cyocum/cluto-2.1.2/Linux-x86_64/vcluster", colmodel="none", verbose=TRUE, control="-niter=1000 -ntrials=3000 -clmethod=rbr -crfun=h2"))
  clutomat <- as.matrix(cluto$cluster)
  row.names(clutomat) <- row.names(x)
  clutomat <- clutomat[order(clutomat[,1]),]
  as.matrix(clutomat)
}

rungmeans <- function(x, knum) {
  gmeans <- skmeans(x, k=knum, method="gmeans", control=list(gmeans="~/temp/backup/gmeans-", verbose=TRUE, control="-E 100"))
  gmeansmat <- as.matrix(gmeans$cluster)
  row.names(gmeansmat) <- row.names(x)
  gmeansmat <- gmeansmat[order(gmeansmat[,1]),]
  as.matrix(gmeansmat)
}

