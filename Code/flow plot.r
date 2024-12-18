
flow.plot = function(object.location,sample.number,start.time,end.time,subregion.width,subregion.height){

load(paste(object.location,"/image data/image data.RData",sep=""))

maximum.intensity <- max(as.vector(image.data[sample.number,,,]))

image.width <- dim(image.data)[3]
image.height <- dim(image.data)[4]

horizontal.dividers <- seq(subregion.width+1/2,image.width,by=subregion.width)
vertical.dividers <- seq(subregion.height+1/2,image.height,by=subregion.height)

grey.levels <- gray((maximum.intensity:0)/maximum.intensity)

pdf(file=paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/flow plot ",start.time," ",end.time,".pdf",sep=""),width=15,height=5)

par(mfrow=c(1,3))

for (i in start.time:end.time){

	image(x=1:image.width,y=1:image.height,z=image.data[sample.number,i,,],zlim=c(0,maximum.intensity),col=grey.levels)
	
	abline(v=horizontal.dividers,lty=2)
	abline(h=vertical.dividers,lty=2)
	
	load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/different flows ",i,".RData",sep=""))

	load(paste(object.location,"/flow data ",subregion.width," ",subregion.height,"/",sample.number,"/fixed mass ",i,".RData",sep=""))
	
	different.flows <- different.flows[order(different.flows[,5]),]
	
	arrows(x0=different.flows[,1],y0=different.flows[,2],x1=different.flows[,3],y1=different.flows[,4],col=grey.levels[different.flows[,5]],length=0.025)
	
	image(x=1:image.width,y=1:image.height,z=fixed.mass,zlim=c(0,maximum.intensity),col=grey.levels)
	
	abline(v=horizontal.dividers,lty=2)
	abline(h=vertical.dividers,lty=2)	
	
	arrows(x0=different.flows[,1],y0=different.flows[,2],x1=different.flows[,3],y1=different.flows[,4],col=grey.levels[different.flows[,5]],length=0.025)
	
	image(x=1:image.width,y=1:image.height,z=image.data[sample.number,i+1,,],zlim=c(0,maximum.intensity),col=grey.levels)
	
	abline(v=horizontal.dividers,lty=2)
	abline(h=vertical.dividers,lty=2)	
	
	arrows(x0=different.flows[,1],y0=different.flows[,2],x1=different.flows[,3],y1=different.flows[,4],col=grey.levels[different.flows[,5]],length=0.025)
	
}

dev.off()

}