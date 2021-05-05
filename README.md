# ETS (Exposure Time Scaling) operator

##### Description

`ETS operator` operator returns the results of a linear regression per cell.

##### Usage

Input projection|.
---|---
`x-axis`        | numeric, x values, per cell 
`y-axis`        | numeric, y values, per cell 

Output relations|.
---|---
`slope`        | numeric, slope of the linear regression model
`intercept`    | numeric, intercept of the linear regression model
`R2`           | numeric, R2 of the linear regression model
`nPoints`      | numeric, number of points in the input data
`Result`       | numeric, did the computation work

##### Details

The operator takes all the values of a cell and performs a linear regression of y against x.

