matlist <-
function(x){  
mylist<-list()
 for (i in 1:nrow(x)){
# mylist[[i]]<-LETTERS[1:ncol(x)] [  as.matrix(x)[i,]!=Inf  ]
 mylist[[i]]<-colnames(x)[  as.matrix(x)[i,]!=Inf  ]
 names(mylist)[i]<-rownames(as.matrix(x))[i]
}
;return(mylist)}
