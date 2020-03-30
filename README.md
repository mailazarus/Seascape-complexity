This repository includes codes to calculate indices of seascape complexity from continous depth measurements (e.g, along a transect). 

The necessary data for the calculation of all indices should include 3 variables:

  * Depth values along the bottom profile
  * Their order, i.e. an acending sequence of numbers corresponding to the measurements
  * An identification variable distinct for each bottom profile

An example data is provided - 'simulation data'. 

Some indices require more data. In such cases explanations are provided within the code, and the user needs to manually add these data
(e.g the length of the transect). 

**The indices produced:**

'common indices functions' calculates established, commonly used indices: Vertical relief, Rugosity, Fractal dimension, Slope, Curvature,
and depths standard deviation. 

'structural diversity functions' calculates structural diversity indice: Functional richness, evenness, dispersion, originality, 
distinctiveness, irregularity, and singularity.

'Sky view factor functions' calculates the Sky view factor and Sky view factor standard deviation indices. 

'TPI functions' calculates the Maximum concavity and Maximum convexity indices.

'neighbors functions' calculates the Neighbors distance and Consecutive Substratum Heights index. 

'EarthMovers functions' calculates the Earth movers index, and 'FreeSpace functions' calculates the Free Space index. 

**The 'Complexity indices code' is a wrapper of all other codes.**
If you wish calculate all suggested complexity indices, you can import your own data and the output would include 21 complexity indices 
(and the mean depth value within the sampling unit). 
If you wish to only calculate specific indices, such as srtuctural diversity indices, you may use the "structural diversity functions" code. 

