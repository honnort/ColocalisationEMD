
jpeg.creation = function(object.location){

require("EBImage")

load(paste(object.location,"/image data/image data.RData",sep=""))

object.dimensions <- dim(image.data)

number.of.simulations <- object.dimensions[1]
number.of.time.points <- object.dimensions[2]
image.width <- object.dimensions[3]
image.height <- object.dimensions[4]

dir.create(paste(object.location,"/jpegs",sep=""),showWarnings=FALSE)

for (i in 1:number.of.simulations){

dir.create(paste(object.location,"/jpegs/",i,sep=""),showWarnings=FALSE)

maximum.intensity <- max(as.vector(image.data[i,,,]))

	for (j in 1:number.of.time.points){
	
		holding.image.data <- image.data[i,j,,]
		
		holding.image.data <- 1-holding.image.data/maximum.intensity
		
		holding.image.data <- holding.image.data[,image.height:1]
		
		current.image <- rgbImage(red=holding.image.data,green=holding.image.data,blue=holding.image.data)
		
		writeImage(current.image,paste(object.location,"/jpegs/",i,"/",j,".jpeg",sep=""))
		
	}
	
}

}