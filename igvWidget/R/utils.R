parseChromLocString <- function(chromLocString){
    chromLocString <- gsub(",", "", chromLocString);
    tokens.0 <- strsplit(chromLocString, ":", fixed=TRUE)[[1]]
    stopifnot(length(tokens.0) == 2)
    chrom <- tokens.0[1]
    if(!grepl("chr", chrom))
        chrom <- sprintf("chr%s", chrom)
    tokens.1 <- strsplit(tokens.0[2], "-")[[1]]
    stopifnot(length(tokens.1) == 2)
    start <- as.integer(tokens.1[1])
    end <- as.integer(tokens.1[2])
    return(list(chrom=chrom, start=start, end=end, string=chromLocString))
    } # parseChromLocString

