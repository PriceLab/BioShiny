printf <- function(...) print(noquote(sprintf(...)))
install.packages("BiocManager", repos="https://cran.rstudio.com")
biocGet <- function(pkgs){
   library(BiocManager)
   BiocManager::install(pkgs, update=FALSE, ask=FALSE)
   }


code.pkgs <- c("MotifDb",
               "TxDb.Hsapiens.UCSC.hg38.knownGene",
               "RUnit"
               )

for(code.pkg in code.pkgs){
   suppressWarnings(
      needed <- !require(code.pkg, character.only=TRUE, lib.loc=my.user.library, quiet=TRUE)
      )
   printf("%s needed? %s", code.pkg, needed)
   if(needed)
      biocGet(code.pkg)
   } # for




