printf <- function(...) print(noquote(sprintf(...)))
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos="https://cran.rstudio.com")
BiocManager::install(version = "3.12", ask=FALSE)

biocGet <- function(pkgs){
   library(BiocManager)
   BiocManager::install(pkgs, update=FALSE)
   }


code.pkgs <- c("shiny",
               "DT",
               "RSQLite",
               "shinyWidgets",
               "GO.db",
               "GOstats",
               "R6",
               "org.Hs.eg.db",
               "shinybusy",
               "iheatmapr",
               "GenomicRanges",
               "Rsamtools",
               "rtracklayer",
               "BSgenome",
               "GenomicFeatures",
               "Rhtslib",
               "VariantAnnotation",
               "V8",
               "rtsne",
               "randomcoloR",
               "MotifDb",
               "RUnit",
               "TxDb.Hsapiens.UCSC.hg38.knownGene"
               )

for(code.pkg in code.pkgs){
   suppressWarnings(
      needed <- !require(code.pkg, character.only=TRUE, lib.loc=my.user.library, quiet=TRUE)
      )
   printf("%s needed? %s", code.pkg, needed)
   if(needed)
      biocGet(code.pkg)
   } # for




