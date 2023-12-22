# Script to define unique spectra from SpectroFlo similarity matrices
# Author: Michael de Kok
# Date: 09-08-2022
# Last Updated: 22-12-2023

# Load SpectroFlo halfmatrix; assume up to 100 columns then remove empty ones
halfmatrix <- read.csv(choose.files(), header = F, 
                       col.names = paste("V", sep = "", 1:100))
halfmatrix <- halfmatrix[,colSums(is.na(halfmatrix)) != dim(halfmatrix)[1]]

# Clean up the half matrix so we can mirror it into the full matrix
second_to_last_row <- dim(halfmatrix)[1] - 1
colnames(halfmatrix) <- halfmatrix[second_to_last_row, ]
halfmatrix <- halfmatrix[-second_to_last_row, ]
rownames(halfmatrix) <- halfmatrix[ , 1]
halfmatrix <- halfmatrix[-second_to_last_row, -1]
halfmatrix <- as.matrix(halfmatrix)

# Utilize a custom function to copy the half matrix into a full matrix
makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}
fullmatrix <- as.data.frame(makeSymm(halfmatrix))

# We consider two spectra as similar if the similarity score is >0.98 
similarity_logical <- fullmatrix > 0.98

# Calculate the number of identical spectra per spectrum (not counting itself)
similarity_number_identical <- colSums(similarity_logical) - 1

# We also calculate the sum of all similarity scores per spectra (minus itself)
similarity_total_sum <- colSums(as.data.frame(sapply(fullmatrix, as.numeric)))-1

# Create a data frame summarizing this information; Set all to unique for now
summary <- data.frame("Spectrum_Name" = colnames(fullmatrix),
                      "Similarity_Sum" = similarity_total_sum,
                      "Number_Identical" = similarity_number_identical,
                      "Unique" = TRUE,
                      "Similar_Spectra" = NA)

# Sort this table by number of identical spectra ascending, then sum ascending
summary <- summary[order(summary[ , 3], summary[ , 2], decreasing = F),]

# Since we are always comparing, we need to do (number of spectra-1) comparisons
num_comparisons <- dim(summary)[1]-1

# Now, we test which spectra will remain unique, going from top to bottom
for (i in 1:num_comparisons) {
  keep <- 0
  spectrum1 <- summary[i,"Spectrum_Name"]
  
  # Only consider similar spectra non-unique if this spectrum is still unique
  if (summary[i,"Unique"] == TRUE) {
    
    # Only consider similar spectra non-unique if there is at least 1 match
    if (summary[i,"Number_Identical"] > 0) {    
      
      # In the case of identical similarity scores between still-unique spectra
      if ((summary[i,"Similarity_Sum"] == summary[(i+1),"Similarity_Sum"]) &
        (summary[i,"Unique"] == TRUE & summary[i+1,"Unique"] == TRUE)) {
        
        # Print a warning and make the user choose between spectra to proceed
        spectrum2 <- summary[i+1,"Spectrum_Name"]
        warning(paste("Identital Similarity Sums",summary[(i),"Similarity_Sum"], 
                      "for spectra", spectrum1, "and", spectrum2, "detected.", 
                      sep = " "), immediate. = TRUE)
        keep <- readline(paste("To proceed, pick a Spectrum to prioritize:",
                               "\n1:", spectrum1, "\n2:", spectrum2, 
                               "\nType either 1 or 2 and press ENTER. "))
      } 
      
      # In the case of non-idential similarity scores or if spectrum 1 was kept
      if (keep < 2) {
        
        # Take note which spectra are identical to this spectra
        ident_spectra <- similarity_logical[spectrum1,]
        ident_spectra <- ident_spectra[-which(names(ident_spectra)==spectrum1)]
        ident_spectra <- names(ident_spectra[ident_spectra])
        
        # Save the names of similar spectra in a dedicated column
        summary[i,"Similar_Spectra"] <- paste(ident_spectra, collapse = "/")
        
        # Exclude those identical spectra from the list of unique spectra
        summary[ident_spectra,"Unique"] <- FALSE
        
        # In the case of identical similarity scores and spectrum 2 was kept
      } else {summary[spectrum1,"Unique"] <- FALSE}
    }
  }
}

# Whatever spectra remain, must be unique. Print those spectra as results
unique_spectra <- rownames(summary)[summary$Unique]
cat("Unique spectra:", unique_spectra)

# Save summary and R work space to file
write.csv(x = summary, file = "Find_Unique_Spectra_Summary.csv", row.names = F)
save.image("./R Workspace.RData")
