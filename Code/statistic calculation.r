
statistic.calculation = function(object.location,sample.number,time.point,subregion.width,subregion.height){

load(paste(object.location,"/image data/image data.RData",sep=""))

image.width <- dim(image.data)[3]
image.height <- dim(image.data)[4]

rm(image.data)

number.of.subregions <- floor(image.width/subregion.width)*floor(image.height/subregion.height)

load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/different flows ",time.point,".RData",sep=""))

summary.statistics <- matrix(NA,nrow=number.of.subregions,ncol=8)

number.of.flows <- dim(different.flows)[1]

intermediate.information <- matrix(NA,nrow=number.of.flows,ncol=4)

intermediate.information[,1] <- different.flows[,6]
intermediate.information[,2] <- sqrt((different.flows[,3]-different.flows[,1])^2+(different.flows[,4]-different.flows[,2])^2)*different.flows[,5]
intermediate.information[,3] <- atan2(different.flows[,4]-different.flows[,2],different.flows[,3]-different.flows[,1])

intermediate.information[which(intermediate.information[,3]<pi/8&intermediate.information[,3]>-pi/8),4] <- 1
intermediate.information[which(intermediate.information[,3]>pi/8&intermediate.information[,3]<3*pi/8),4] <- 2
intermediate.information[which(intermediate.information[,3]>3*pi/8&intermediate.information[,3]<5*pi/8),4] <- 3
intermediate.information[which(intermediate.information[,3]>5*pi/8&intermediate.information[,3]<7*pi/8),4] <- 4
intermediate.information[which(intermediate.information[,3]>7*pi/8|intermediate.information[,3]< -7*pi/8),4] <- 5
intermediate.information[which(intermediate.information[,3]> -7*pi/8&intermediate.information[,3]< -5*pi/8),4] <- 6
intermediate.information[which(intermediate.information[,3]> -5*pi/8&intermediate.information[,3]< -3*pi/8),4] <- 7
intermediate.information[which(intermediate.information[,3]> -3*pi/8&intermediate.information[,3]< -pi/8),4] <- 8

for (i in 1:number.of.subregions){

	for (j in 1:8){

		summary.statistics[i,j] <- sum(intermediate.information[which(intermediate.information[,1]==i&intermediate.information[,4]==j),2])
		
	}
	
}

save(summary.statistics,file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/summary statistics ",time.point,".RData",sep=""))

}