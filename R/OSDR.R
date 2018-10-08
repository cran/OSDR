OSDR <-
function(M){
	
n<-length(M)
# comment: initialize OSDR
 SDR<-rep(NA,n)
#   comment: fill OSDR greedily until possible 
 for (j in 1:n){
if  (length(setdiff(M[[j]],SDR))!=0)
    SDR[j]<-setdiff(M[[j]],SDR)[1] else{ 
# initialize lists for searching unfilled Tj
A<-B<-list()
comblist<-M[j]; B[1]<-unlist(comblist)[1]; A[1]<- match(B[1],SDR)
#   find an unmatched control in combined list of previous matched treated units
     for (i in 1:j) {
      comblist <- union(unlist(comblist),unlist(M[match(B[i],SDR)]))
      if (length(setdiff(comblist,SDR))>0){
      	       B[i+1] <- setdiff(comblist,SDR)[1] ; break} 
      else {if (i==j){SDR[j]<-0; cat("Hall's condition not satisfied at step ",j,": T_",j," dropped\n",sep=""); break};
      	       B[i+1] <- setdiff(comblist,Reduce("union",B[c(1:i)]))[1]; A[i+1] <- match(B[i+1],SDR)}
                    }                        

       while (is.na(SDR[j])){

         ind    <-intersect(which(sapply(M,is.element,el=B[i+1])),unlist(A))
         k       <-max(ind)
         free   <-SDR[k]       #free SDR[k]..
         SDR[k] <-unlist(B[i+1]) # ...put B[i+1] in place 

         if((free%in%M[[j]])) {SDR[j]<-free; cat("SDR[",j,"]=",SDR[j])
         	;break} else {
    	B[i+1]<-free ; A<-A[-k]; cat("B_",i+1)
    	}

          }#close while              
                                             }#close else
                                     }#close for
                                     results<-list(which(SDR!=0), SDR[SDR!=0], which(SDR==0))
                                     names(results)<-c("matching","OSDR","unmatched")
return(results) 
}
