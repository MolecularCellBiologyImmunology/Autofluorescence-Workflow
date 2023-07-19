# Script to Separate Merged FCS file from OMIQ by Column
# Author: Michael de Kok
# Date: 17-07-2022

##### USER PARAMETERS #####

# The Working Directory (Default "." means unchanged)
WORKING_DIRECTORY <- "."

# The File Name of the Merged FCS File to Split.
INPUT_FCS         <- "./Unstained (Cells).fcs"

# The File name of the Annotation Table as outputted by OMIQ.
INPUT_ANNOTATION  <- "./_FilterValuesToNames.csv"

# The Name of the Column inside the Input FCS to split said FCS by.
SPLIT_COLUMN      <- "OmiqFilter"

# IMPORTANT: The output files will be named after the named counterparts of the
# cluster numbers inside the Split Column, as determined by the OMIQ Annotation.

###########################

# Load Packages
library(flowCore)

# Load Full FCS
setwd(WORKING_DIRECTORY)
fcs_full <- read.FCS(filename = INPUT_FCS)
fcs_exprs <- as.data.frame(fcs_full@exprs)

# Load Annotation
fcs_annotation <- read.csv(file = INPUT_ANNOTATION, row.names = 1)

# Check if Annotations match Full FCS
clusters_annotation <- sort(unique(as.numeric(rownames(fcs_annotation))))
clusters_full_fcs <- sort(unique(as.data.frame(fcs_full@exprs)[,SPLIT_COLUMN]))
stopifnot(clusters_annotation == clusters_full_fcs)

# Subset and save FCS per list
fcs_list <- list()
setwd("./Subset_FCS/")
column_subset <- (!colnames(fcs_full) %in% COLUMN_TO_SPLIT)
for (cluster in clusters_annotation) {
  samplename <- fcs_annotation[cluster,1]
  fcs_subset <- fcs_exprs[,COLUMN_TO_SPLIT] == cluster
  fcs_subset <- fcs_full[fcs_subset,column_subset]
  fcs_list[[samplename]] <- fcs_subset
  write.FCS(x = fcs_subset, filename = paste(samplename, ".fcs", sep = ""))
}

# Cleanup and save Workspace
rm(cluster, samplename, fcs_subset)
save.image("./R Workspace.RData")
