# NMA 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Original code converted to an R package
+ HR, median, survival binary, count and binary data options
+ Tidy and standardise BUGS code
+ Removed all `fileSep` use and replaced with `file.path`
+ Vignettes written for survival dummy data and drafted for binary and count data
+ Option to run analyses within an R script or using a Reference file with all of the input parameters in
+ Automatically generates a report in pdf or Word format (#282fe4d)
+ Documented input data (#bc3f999)
+ Builds survival analysis BUGS code on the fly as combinations of HR, median and binary data (#06e8df1)
+ Started unit tests
+ `pkgdown` site created (#7d319ce)
