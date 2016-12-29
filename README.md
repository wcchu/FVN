# FVN
Fixed-volume neighborhood algorithm

* Similar to kNN, predictor variable are numeric and response is either numeric (regression) or categorical (classification)
* Distance is calculated in normalized variable space
* Variable space is normalized to the standard deviation of the distribution of each variable
* User specifies the volume of the hyper-dimensional "neighborhood ball", which thus defines the radius of the ball by (normalized volume) = (normalized radius) ^ (number of variables)
* The prediction for a query point is based on the statistics of all the neighbors within this radius
* In a rural area, the population in the neighborhood may be lower than the limit set by the user and there's no prediction given

_make_ to generate dnn_test.html which demonstrates a test of dNN on Cars93 data (regression).
I'll demonstrate a classification later.
