
image.simulation = function(simulation.type,shared.proportion,number.of.samples,time.points,output.folder.name){

number.of.simulations <- number.of.samples
number.of.time.points <- time.points+1
image.width <- 60
image.height <- 60

dir.create(output.folder.name,showWarnings=FALSE)
dir.create(paste(output.folder.name,"/image data",sep=""),showWarnings=FALSE)

if (simulation.type == "noise"){

	noise.simulation(output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height)

}

if (simulation.type == "isotropic"){

	isotropic.simulation(shared.proportion,output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height)
	
}

if (simulation.type == "homogeneous"){

	homogeneous.simulation(shared.proportion,output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height)
	
}

if (simulation.type == "symmetric"){

	symmetric.simulation(shared.proportion,output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height)

}

if (simulation.type == "bounded"){

	bounded.simulation(shared.proportion,output.folder.name,number.of.simulations,number.of.time.points,image.width,image.height)
	
}

}