# Linear Markov Model for Antarctic Sea Ice Forecasting

## 1. MEOF Analysis
MEOF Analysis to Identify the leading modes. Modes are composed of spatial patterns (MEOFS) and their corresponding time series (principal components).
- MEOF analysis is done in python.
- uses the python eofs package.
- Saves the MEOF and PC files to **_/d6/bxw2101/model_files/fortran_inputs/meofs_** and **_/d6/bxw2101/model_files/fortran_inputs/pcs_** (on legolas)


## 2. Linear Markov Model
Linear Markov Model creation to form new principal components, using hindcast and take-one-year-out cross-validation.
- Linear Markov Model creation is done in fortran.
- Takes the MEOFs and PCs outputted from the MEOF analysis, and feeds into fortran files.
- /cross, /hindcast, and /imsl folders are used.
- Saves the hindcast and cross validation matrices to **_/d6/bxw2101/model_files/hcast_output/_** and **_/d6/bxw2101/model_files/cross_output/_**
