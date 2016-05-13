binning = function(x, resolucao){
  saida = vector(length=resolucao)
  passo = trunc(length(x)/length(saida))
  inicio= 1
  for(i in 1:resolucao){
    if((inicio+passo) < length(x)){
      saida[i] = sum(x[inicio:(inicio+passo)])
    }else{
      saida[i] = 0
    }
    inicio = inicio+passo
  }
  return(saida)
}

ajustarPorTempo = function(x, y, assinaturas.min){
  comprimento = length(assinaturas.min)
  saida = vector(length=comprimento)
  passo = trunc(length(x)/comprimento)
  for(i in 1:comprimento){
    saida[i] = sum(x[substr(y$timestamp, start=9, stop=16)==assinaturas.min[i]] )
  }
  return(saida)
}