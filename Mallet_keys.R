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


##now extracting the keys

mallet.keys<- function(file, num_topics,number_stop_words){
  start<- read.delim(file, header=F, sep='', row.names=NULL)
  keys_prop<- matrix(NA, nrow=num_topics, ncol=number_stop_words+2)
  colnames(keys_prop)<- c("Topic", "Probability", rep (NA, number_stop_words))
  for(z in 1:num_topics){
    keys_prop[z,2]<- as.character(start[z,2])
    keys_prop[z,1]<- as.character(start[z,1]+1)
    for(g in 3:(ncol(keys_prop))){
      keys_prop[z,g]<- as.character(start[z,g])
    }
  }
  for (i in 3:ncol (keys_prop)){
    colnames (keys_prop)[i]<-paste ("Topword ",toString(i-2))
  }
  return(keys_prop)
}
