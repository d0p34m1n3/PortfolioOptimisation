rm(list=ls())
require(RCurl)
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)

con = gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)
load.packages('quantmod')

data <- new.env()

tickers<-spl("VBMFX,VTSMX,VGTSX,VGSIX")
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na', dates='1990::2013')

prices<-data$prices
ret<-na.omit(prices/mlag(prices) - 1)

weight<-matrix(1/ncol(ret),nrow=1,ncol=ncol(ret))
p.ret<-(weight) %*% t(ret)

demean = scale(coredata(ret), center=TRUE, scale=FALSE)
covm<-cov(demean)
pca<-prcomp(ret,cor=F)
evec<-pca$rotation[] #eigen vectors
eval <- pca$sdev^2 #eigen values

print(diag(t(evec) %*% covm %*% evec))
print(eval)

inv.evec<-solve(evec) #inverse of eigenvector
pc.port<-inv.evec %*% t(ret)
