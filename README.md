# dNN
Distance-based Nearest neighbors algorithm

* Distance is calculated in normalized variable space
* User specifies the volume of the hyper-dimensional "neighborhood ball", which fixes the radius of the ball by (normalized volume) = (normalized radius) ^ (number of variables)
* Variable space is normalized to the standard deviation of the distribution of each variable
* In a rural area, the population in the neighborhood may be lower than the limit set by the user and there's no prediction given
