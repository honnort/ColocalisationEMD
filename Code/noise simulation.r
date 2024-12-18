
noise.simulation = function(output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height){

noise.intensity <- 3

image.data <- array(rpois(number.of.simulations*number.of.time.points*image.width*image.height,noise.intensity),dim=c(number.of.simulations,number.of.time.points,image.width,image.height))

save(image.data,file=paste(output.folder.name,"/image data/image data.RData",sep=""))

}