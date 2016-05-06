library(foreign)
library(arules)
source("binning.R")

dataset = read.csv(file = "base/AlertasICTF08.csv")

resolucao = 16

assinaturas.qtd = length(unique(dataset$signature))
assinaturas = unique(dataset$signature)

assinaturas.signal = matrix(nrow=assinaturas.qtd, ncol = resolucao)

#assinaturas[1]
#assinaturas.signal[1,] = binning(as.numeric(dataset$signature==assinaturas[1]), resolucao)
#plot(assinaturas.signal[1,], type = "l")

### Binning Sylvio
for(i in 1:assinaturas.qtd){
  assinaturas.signal[i,] = binning(as.numeric(dataset$signature==assinaturas[i]), resolucao)
}

### Plot só binning sylvio
aux = 1
for(pagina in 1:round((assinaturas.qtd*2)/16)){
  png(file=paste(paste("resultados/alerts_",pagina),".png"), bg="white", width = 1500, height = 1024)
  par(mfrow=c(4,4))
  for(i in aux:(aux+15)){
    
    x = assinaturas.signal[i,]
    
    #binning Sylvio
    plot(x, type="l", xlab = assinaturas[i], ylab = "Alerts",
         cex.main=2, cex.lab=2,cex.axis=2, main = "Time")
    
    #binning por freq
    hist(x, breaks=20, main="frequency", xlab = assinaturas[i], ylab = "Alerts",
         cex.main=2, cex.lab=2,cex.axis=2)
    abline(v=discretize(x, method="frequency", categories=4, onlycuts=TRUE), 
           col="red", labels=c("I1","I2","I3","I4") )
    
  }
  dev.off()
  aux = aux + 8;
}


#aux = 1
#for(pagina in 1:round(assinaturas.qtd/16)){
#   png(file=paste(paste("resultados/alerts_",pagina),".png"), bg="white", width = 1500, height = 1024)
#   par(mfrow=c(4,4))
#   for(i in aux:(aux+15)){
#     plot(assinaturas.signal[i,], type="l", xlab = assinaturas[i], ylab = "Alerts",
#          cex.main=2, cex.lab=2,cex.axis=2)
#     
#     #rect(1, 5, 3, 7, col="white")
#   }
#   dev.off()
#   aux = aux + 16;
# }



