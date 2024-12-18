
homogeneous.simulation = function(shared.proportion,output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height){

noise.intensity <- 3
object.intensity <- 30
number.of.objects <- 100
movement.speed <- 3

shared.objects <- round(shared.proportion*number.of.objects/100)
distinct.objects <- number.of.objects-shared.objects

image.data <- array(rpois(number.of.simulations*number.of.time.points*image.width*image.height,noise.intensity),dim=c(number.of.simulations,number.of.time.points,image.width,image.height))

centre.locations <- array(NA,dim=c(number.of.simulations,number.of.objects,number.of.time.points,2))

if (shared.objects>0){

	for (i in 1:shared.objects){

		holding.locations <- matrix(NA,nrow=number.of.time.points,ncol=2)
		
		centre.locations[,i,1,1] <- runif(1)*(image.width-1)
		centre.locations[,i,1,2] <- runif(1)*(image.height-1)
		
		for (j in 2:number.of.time.points){
		
			holding.angle <- pi/4
			
			centre.locations[,i,j,1] <- centre.locations[1,i,(j-1),1]+movement.speed*cos(holding.angle)
			centre.locations[,i,j,2] <- centre.locations[1,i,(j-1),2]+movement.speed*sin(holding.angle)
			
		}

	}
	
}

for (i in (shared.objects+1):number.of.objects){

	for (j in 1:number.of.simulations){
	
		centre.locations[j,i,1,1] <- runif(1)*(image.width-1)
		centre.locations[j,i,1,2] <- runif(1)*(image.height-1)
		
		for (k in 2:number.of.time.points){
		
			holding.angle <- pi/4
			
			centre.locations[j,i,k,1] <- centre.locations[j,i,(k-1),1]+movement.speed*cos(holding.angle)
			centre.locations[j,i,k,2] <- centre.locations[j,i,(k-1),2]+movement.speed*sin(holding.angle)
			
		}
		
	}
	
}

centre.locations <- round(centre.locations)
centre.locations[,,,1] <- centre.locations[,,,1]%%image.width
centre.locations[,,,2] <- centre.locations[,,,2]%%image.height

centre.locations <- centre.locations+1

for (i in 1:number.of.simulations){

	for (j in 1:number.of.objects){
	
		for (k in 1:number.of.time.points){
		
			holding.centre <- centre.locations[i,j,k,]
		
			image.data[i,k,holding.centre[1],holding.centre[2]] <- image.data[i,k,holding.centre[1],holding.centre[2]]+object.intensity
			
			if (holding.centre[1]>1) image.data[i,k,holding.centre[1]-1,holding.centre[2]] <- image.data[i,k,holding.centre[1]-1,holding.centre[2]]+object.intensity
			if (holding.centre[1]<image.width) image.data[i,k,holding.centre[1]+1,holding.centre[2]] <- image.data[i,k,holding.centre[1]+1,holding.centre[2]]+object.intensity
			if (holding.centre[2]>0) image.data[i,k,holding.centre[1],holding.centre[2]-1] <- image.data[i,k,holding.centre[1],holding.centre[2]-1]+object.intensity
			if (holding.centre[2]<image.height) image.data[i,k,holding.centre[1],holding.centre[2]+1] <- image.data[i,k,holding.centre[1],holding.centre[2]+1]+object.intensity
			
		}
		
	}
	
}

save(centre.locations,file=paste(output.folder.name,"/image data/centre locations.RData",sep=""))
save(image.data,file=paste(output.folder.name,"/image data/image data.RData",sep=""))

}