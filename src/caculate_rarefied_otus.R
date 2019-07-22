#Date: 2018-10-25
#Usage1: rarefy data according to a specified depth
#Usage2: rarefy data and generate depth files for plotting

#this function comes from sourcetracker.r
rarefy <- function(x,maxdepth){
  if(is.null(maxdepth)) return(x)
  if(!is.element(class(x), c('matrix', 'data.frame','array')))
    x <- matrix(x,nrow=1)
  nr <- nrow(x)
  nc <- ncol(x)
  for(i in 1:nrow(x)){
    if(sum(x[i,]) > maxdepth){
      #根据最大的深度抽取样本
      s <- sample(nc, size=maxdepth, prob=x[i,], replace=T)
      #计算各序列长度的频数
      x[i,] <- hist(s,breaks=seq(.5,nc+.5,1), plot=FALSE)$counts
    }
  }
  return(x)
}

caculate_rarefied_otus <- function(mat, steps = 10, maxdepth = c("max", "median", "min")){
  maxdepth <- match.arg(maxdepth)
  sum.num <- apply(mat, FUN = sum, MARGIN = 1)
  if(maxdepth == 'median'){
    maxdepth <- median(sum.num)
  }else if(maxdepth == 'min'){
    maxdepth <- min(sum.num)
  }else{
    maxdepth <- max(sum.num)
  }
  evendata <- rarefy(mat, min(sum.num))
  obs_otus <- apply(rarefy(evendata, 10), 1, function(vect) sum(sign(vect)))
  for(i in 1:steps){
    depth <- (maxdepth - 10) / steps * i
    tmp <- apply(rarefy(evendata, depth), 1, function(vect) sum(sign(vect)))
    obs_otus <- rbind(obs_otus, tmp)
  }
  series <- as.integer(c(10, (maxdepth - 10) / steps * (1:steps)))
  obs_otu_data <- cbind(series, obs_otus)
  rownames(obs_otu_data) <- NULL
  as.data.frame(obs_otu_data)
}
