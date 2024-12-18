
flow.calculation = function(object.location,sample.number,time.number,subregion.width,subregion.height){

require(emdist)

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/subregion limits.RData",sep=""))

load(paste(object.location,"/image data/image data.RData",sep=""))

number.of.subregions <- dim(subregion.limits)[1]

holding.source <- image.data[sample.number,time.number,,]
holding.destination <- image.data[sample.number,time.number+1,,]

rm(image.data)

fixed.mass <- pmin(holding.source,holding.destination)

holding.source <- holding.source-fixed.mass
holding.destination <- holding.destination-fixed.mass

different.flows <- matrix(nrow=0,ncol=6)

for (i in 1:number.of.subregions){

	subregion.source <- holding.source[subregion.limits[i,1]:subregion.limits[i,2],subregion.limits[i,3]:subregion.limits[i,4]]
	subregion.destination <- holding.destination[subregion.limits[i,1]:subregion.limits[i,2],subregion.limits[i,3]:subregion.limits[i,4]]
	
	if (sum(as.vector(subregion.source)>0)>0&sum(as.vector(subregion.destination))>0){
	
		horizontal.locations <- matrix(subregion.limits[i,1]:subregion.limits[i,2],ncol=subregion.width,nrow=subregion.height)
		vertical.locations <- matrix(subregion.limits[i,3]:subregion.limits[i,4],byrow=TRUE,ncol=subregion.width,nrow=subregion.height)
	
		A <- matrix(c(as.vector(horizontal.locations),as.vector(vertical.locations)),ncol=2)
		wA <- as.vector(subregion.source)
		B <- matrix(c(as.vector(horizontal.locations),as.vector(vertical.locations)),ncol=2)
		wB <- as.vector(subregion.destination)
	
		nonzero.A <- which(wA>0)
		nonzero.B <- which(wB>0)
	
		A <- A[nonzero.A,,drop=FALSE]
		wA <- wA[nonzero.A]
		B <- B[nonzero.B,,drop=FALSE]
		wB <- wB[nonzero.B]
	
		emd.results <- emdw(A,wA,B,wB,flows=TRUE)
	
		raw.flows <- matrix(c(attributes(emd.results)$flows[[1]],attributes(emd.results)$flows[[2]],attributes(emd.results)$flows[[3]]),ncol=3)
	
		raw.flows[,c(1,2)] <- raw.flows[,c(1,2)]+1
	
		subregion.flows <- matrix(c(A[raw.flows[,1],],B[raw.flows[,2],],raw.flows[,3]),ncol=5)

		different.flows <- rbind(different.flows,cbind(subregion.flows,i))
		
	}
	
}

dir.create(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,sep=""),showWarnings=FALSE)

save(fixed.mass,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/fixed mass ",time.number,".RData",sep=""))

save(different.flows,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/different flows ",time.number,".RData",sep=""))

}