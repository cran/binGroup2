# binGroup2 1.3.1

## Major changes

* Added `GroupMembershipMatrix()` function to construct group membership matrices for hierarchical testing algorithms.

* Added a vignette to demonstrate identification through group testing.

## Minor improvements and bug fixes

* Added a `NEWS.md` file to track changes to the package.

* Added security to `informativeArrayProb()`, `halving()`, `TOD()`, `Sterrett()`, `opChar1()`, `opChar2()`, `OTC1()`, `OTC2()`, `gtSim()`, `gtReg()`, to ensure that probability, sensitivity, and specificity inputs are between 0 and 1 and the correct dimensions.
