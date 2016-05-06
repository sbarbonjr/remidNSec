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