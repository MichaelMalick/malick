my.ls <- function(pos = 1){

    dat <- sapply(ls(pos = pos), function(x)class(get(x)))
    size <- sapply(ls(pos = pos), function(x)object.size(get(x)))
    dat.df <- data.frame(Object = names(dat), Class = dat, 
        Size = size)
    dat.df$Size <- formatC(dat.df$Size, big.mark=',', digits=0,
        format='f')
    row.names(dat.df) <- NULL
    dat.df <- dat.df[order(dat.df$Class), ]

    dat.df

}

