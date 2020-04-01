## Test environments
* local OS X install, R 3.3.2
* win-builder 
* Travis

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of factoextra. 
All packages that I could install passed.

## Update
This is a maintenance update version 1.0.7 (see NEWS.md). Adding stringsAsFactors = TRUE to the relevant calls to data.frame to anticipate compatibility with future R 4.0.0.




