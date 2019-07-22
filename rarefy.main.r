args <- commandArgs(T)
if(length(args) != 3){
	cat('Usage: Rscript rarefy.main.r infile depthi[min,max,median] step\n')
	cat('Example: Rscript rarefy.main.r otu_table.txt min 10\n\n')
	quit()
}

infile <- args[1] 
depth <- args[2]
step <- as.numeric(args[3])

library(ggplot2)
library(reshape2)
#get path function
path_get <- function(){
    res <- list()
    allargs <- commandArgs(trailingOnly = FALSE)
    scr.name <- sub('--file=', '', allargs[grepl('--file=', allargs)])
    abs_path <- normalizePath(scr.name)
    res$scr.path <- dirname(abs_path)
    res$scr.name <- scr.name
    return(res)
}  


subr <- paste(path_get()$scr.path, "src/caculate_rarefied_otus.R", sep="/")

if(file.exists(subr)){
    source(subr)
}else{
    cat("NO ", subr, 'exists, check please!\n')
    quit()
} 

otu_even <- read.table(infile, sep = '\t', comment.char = '', row.names = 1, header = T)

otu_even  <- as.data.frame(t(otu_even[, -dim(otu_even)[2]]))

rarefied_dt <- caculate_rarefied_otus(otu_even, steps = step, maxdepth = depth)

out <- paste0(depth, '.rarefy.xls')

write.table(rarefied_dt, out, sep = "\t", row.names = FALSE, quote = FALSE)

melt_dt <- melt(rarefied_dt, id.vars = 'series')

p <- ggplot(melt_dt, aes(series, value, fill = variable, color = variable))+
  geom_line(size = 1, show.legend = FALSE) + 
  labs(title = 'Rarefaction curve')

ggsave(filename = paste0(depth, '_rarefaction_curve.pdf'),height = 8, width = 8, limitsize=FALSE)
