listmat <-
function(x){el<-unique(unlist(x))
Mat<-matrix(rep(el,length(x)),nrow=length(x),byrow=TRUE)
 for (i in 1:nrow(Mat)){
            for (j in 1:ncol(Mat)){
            Mat[i,j]<-ifelse(Mat[i,j]%in%x[[i]],0,Inf)
                                       }
}
Mat<-apply(Mat,2,as.numeric);Mat
return(Mat)}
