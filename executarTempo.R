library(foreign)
library(arules)

source("binning.R")

dataset = read.csv(file = "base/AlertasICTF08.csv")
aux = read.table(file = "aux.csv", header = FALSE)
#dataset = dataset[1:(nrow(dataset)/4),]

resolucao = 128 

assinaturas.qtd = length(unique(dataset$signature))
assinaturas = unique(dataset$signature)

#assinaturas.min = unique(strftime(dataset$timestamp, "%d %H:%M"))
assinaturas.min = data.frame(Timestamp=character(8), stringsAsFactors = FALSE)
for(i in (1:nrow(dataset))){
#for(i in 1:15){
  #problema na 97541
  tryCatch(
    {
      print(strftime(dataset[i,1], "%d %H:%M"))
      assinaturas.min = rbind(assinaturas.min, strftime(dataset[i,1], "%d %H:%M"))
      print(i)
    }, error = function(e){print(i)}
  )
}


print("Tempo das assinaturas encerrado")

#assinaturas.min = unique(strftime(dataset$timestamp, "%d %H:%M"))
assinaturas.min = unique(assinaturas.min)
assinaturas.min.qtd = nrow(assinaturas.min)

assinaturas.min.signal = matrix(nrow=assinaturas.qtd, ncol = assinaturas.min.qtd)
assinaturas.signal = matrix(nrow=assinaturas.qtd, ncol = resolucao)

### Binning Sylvio
ajustarPorTempo = function(x, y, assinaturas.min){
  comprimento = length(assinaturas.min)
  saida = vector(length=comprimento)
  passo = trunc(length(x)/comprimento)
  for(i in 1:comprimento){
    saida[i] = sum(x[strftime(dataset$timestamp, "%d %H:%M")==assinaturas.min[i]] )
  }
  return(saida)
}


### Organizando pelo tempo
for(i in as.numeric(aux):assinaturas.qtd){
  print(paste(i,assinaturas.qtd,sep = "/"))
  assinaturas.min.signal[i,] = ajustarPorTempo(as.numeric(dataset$signature==assinaturas[i]), dataset, assinaturas.min)
  write.table(file = "aux.csv", x = (i+1),append = FALSE, col.names = F, row.names = F)
  write.table(file="assinaturasTempo.csv", x=t(assinaturas.min.signal[i,]), row.names=c(as.character(i)), 
              sep=",",append = TRUE, col.names = F , quote = c(1), eol = "\n", na = "0", dec = "0")
}


### Binning Sylvio
for(i in 1:assinaturas.qtd){
  assinaturas.signal[i,] = binning(assinaturas.min.signal[i,], resolucao)
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


options(save.defaults = list(ascii = TRUE, safe = FALSE))
save.image(file = "mem.RData")
unlink(".RData")
