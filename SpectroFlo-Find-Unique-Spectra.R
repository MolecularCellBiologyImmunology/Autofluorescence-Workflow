# Script to Define Unique Spectra from SpectroFlo Similarity Matrix
# Author: Michael de Kok
# Date: 09-08-2022
# Update: 18-10-2023

# Load SpectroFlo Halfmatrix. We assume up to 100 columns then remove empty ones.
halfmatrix <- read.csv(choose.files(), 
                       header = F, 
                       col.names = paste("V", sep = "", 1:100))
halfmatrix <- halfmatrix[,colSums(is.na(halfmatrix)) != dim(halfmatrix)[1]]

# Clean up the Half Matrix so we can mirror it into the Full Matrix.
second_to_last_row <- dim(halfmatrix)[1] - 1
colnames(halfmatrix) <- halfmatrix[second_to_last_row, ]
halfmatrix <- halfmatrix[-second_to_last_row, ]
rownames(halfmatrix) <- halfmatrix[ , 1]
halfmatrix <- halfmatrix[-second_to_last_row, -1]
halfmatrix <- as.matrix(halfmatrix)

# Utilize a custom function to copy the Half Matrix into a Full Matrix.
makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}
fullmatrix <- as.data.frame(makeSymm(halfmatrix))

# We consider two Spectra as Similar if the Similarity score is >0.98.
similarity_logical <- fullmatrix > 0.98

# Calculate the number of Identical Spectra Per Spectrum (not counting itself).
similarity_number_identical <- colSums(similarity_logical) - 1

# We also calculate the Sum of all Similarity Scores per Spectra (minus itself).
similarity_total_sum <- colSums(as.data.frame(sapply(fullmatrix, as.numeric)))-1

# Create a Data Frame Summarizing this Information. Set all to Unique for now.
summary <- data.frame("Spectrum_Name" = colnames(fullmatrix),
                      "Similarity_Sum" = similarity_total_sum,
                      "Number_Identical" = similarity_number_identical,
                      "Unique" = TRUE)

# Sort this Table by number of Identical Spectra ascending, then Sum ascending.
summary <- summary[order(summary[ , 3], summary[ , 2], decreasing = F),]

# Now, we test which Spectra will remain Unique, going from top to bottom:
for (i in 1:dim(summary)[1]) {
  keep <- 0
  spectrum1 <- summary[i,"Spectrum_Name"]
  
  # Only consider Similar Spectra Non-Unique if this Spectrum is still Unique
  if (summary[i,"Unique"] == TRUE) {
    
    # Only consider Similar Spectra Non-Unique if there is at least 1 match
    if (summary[i,"Number_Identical"] > 0) {    
      
      # In the case of identical Similarity Scores between still-unique Spectra
      if ((summary[i,"Similarity_Sum"] == summary[(i+1),"Similarity_Sum"]) &
        (summary[i,"Unique"] == TRUE & summary[i+1,"Unique"] == TRUE)) {
        
        # Print a warning and make the user choose between Spectra to proceed
        spectrum2 <- summary[i+1,"Spectrum_Name"]
        warning(paste("Identital Similarity Sums",summary[(i),"Similarity_Sum"], 
                      "for spectra", spectrum1, "and", spectrum2, "detected.", 
                      sep = " "), immediate. = TRUE)
        keep <- readline(paste("To proceed, pick a Spectrum to prioritize:",
                               "\n1:", spectrum1, "\n2:", spectrum2, 
                               "\nType either 1 or 2 and press ENTER. "))
      } 
      
      # In the case of non-idential Similarity Scores or if spectrum 1 was kept
      if (keep < 2) {
        
        # Take note which Spectra are identical to this Spectra
        ident_spectra <- similarity_logical[spectrum1,]
        ident_spectra <- ident_spectra[-which(names(ident_spectra)==spectrum1)]
        ident_spectra <- names(ident_spectra[ident_spectra])
        # Exclude those identical spectra from the list of Unique Spectra
        summary[ident_spectra,"Unique"] <- FALSE
        
        # In the case of identical Similarity Scores and spectrum 2 was kept
      } else {summary[spectrum1,"Unique"] <- FALSE}
    }
  }
}

# Whatever Spectra remain, must be Unique. Print those Spectra as Results.
unique_spectra <- rownames(summary)[summary$Unique]
cat("Unique spectra:", unique_spectra)

# Save Workspace
save.image("./R Workspace.RData")
