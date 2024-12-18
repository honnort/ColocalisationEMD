# Contents

## Data

### Pre-processing

TIFF files for seven samples, each ranging between 47 and 57 time points.  
The files each contain two channels, interleaved: channel 1 at time 1, channel 2 at time 1, channel 1 at time 2, channel 2 at time 2, etc.

### Post-processing

TIFF files for seven samples, each ranging between 47 and 57 time points.  
There are separate files for each of the two channels (A and B).  
Images are obtained by rotating and cropping the pre-processed images, to produce a consistent image size and object orientation.

## Code

-	*image simulation wrapper.R*: Function to produce simulated images of one a number of types.
-	*noise simulation.R*: Function to simulate image data which is purely noise.
-	*isotropic simulation.R*: Function to simulate image data including objects which move isotropically -- without any direction preference.
-	*homogeneous simulation.R*: Function to simulate image data including objects which move homogeneously -- all in the same direction.
-	*bounded simulation.R*: Function to simulate image data including objects which are forced to remain within the bounds of the image.
-	*symmetric simulation.R*: Function to simulate image data including objects which move with rotational symmetry, towards the centre of the image.
-	*jpeg creation.R*: Function to generate JPEG images from simulated image data.
-	*subregion specification.R*: Function to divide the whole image space into subregions for analysis.
-	*flow calculation.R*: Function to use earth mover's distance to estimate object movements between consecutive time points.
-	*flow plot.R*: Function to produce a plot which visualises the estimated object movements.
-	*statistic calculation.R*: Function to summarise object movements in each subregion.
-	*summary plot.R*: Function to produce a plot which visualises subregion summaries.
-	*direction summary plots.R*: Function to produce a plot which visualises the degree to which subregion summaries vary over time.
-	*allowable comparisons.R*: Function to determine which subregion comparisons are acceptable for each hypothesis type.
-	*statistic comparison.R*: Function to compare two subregion summaries.
-	*permutation calculations.R*: Function to calculate subregion summary comparisons for all acceptable comparisons for a given hypothesis type.
-	*results significance.R*: Function to calculate the permutation test significance level for a given hypothesis type.
-	*images to data.R*: Function to convert existing images to data for analysis.