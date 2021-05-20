# ETS (Exposure Time Scaling) operator

##### Description

`ETS operator` operator returns the results of a linear regression (with zero intercept) per cell.

##### Usage

Input projection|.
---|---
`x-axis`        | numeric, x values, per cell 
`y-axis`        | numeric, y values, per cell 

Output relations|.
---|---
`slope`        | numeric, slope of the linear regression model
`intercept`    | numeric, intercept of the linear regression model = 0
`R2`           | numeric, R2 of the linear regression model
`nPoints`      | numeric, number of points in the input data
`Result`       | numeric, did the computation work
`xFit`         | numeric, x value for the fit
`yFit`         | numeric, fitted value based on xFit

##### Details

The operator takes all the values of a cell and performs a linear regression of y against x. The intercept is always zero.
For each cell, there could be 0, 1 or more datapoints, the `lm` function is only used in the last case. Furthermore, the output contains two tables, one contains the summary statistics like slope and intercept and the other contains the predicted values. 

