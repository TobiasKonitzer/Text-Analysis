mallet.topics<- function(file, num_topics){
  start<- read.delim(file, header=F, sep ='\t', row.names=NULL)
  start<- start[-1,-ncol (start)]
  store.props<- matrix(NA, nrow=nrow(start), ncol=num_topics)
  seq1<- seq(3, 2*num_topics + 2, by = 2)
  seq2<- seq(4, 2*num_topics + 2, by = 2)
  for(z in 1:nrow(start)){
    for(k in 1:num_topics){
      store.props[z,as.numeric(as.character(start[z,seq1[k]])) +1]<- as.numeric(as.character(start[z, seq2[k]]))
    }
  }
  
  rownames(store.props)<- as.character(start[,2])
  return(store.props)
}  
