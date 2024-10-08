setwd("~/Data/FA/Hyphenation/hyph2")

# Read FKW file
df <- read.delim("hyph-fkw.txt", header=FALSE)

# Remove leading and trailing spaces
df$V1 <- trimws(df$V1, "both")

# Remove empty rows
df <- subset(df, V1!="")

# Remove rows with space(s) or comma(s)
df <- subset(df, !grepl(" "  , V1))
df <- subset(df, !grepl(","  , V1))

# Retain all words with multiple syllables
df <- subset(df,  grepl("\\.", V1))

# Add a column with the words without hyphenation marks
df$V2 <- df$V1
df$V1 <- gsub("\\.", "", df$V1)

# Get the unique set of characters
all <- paste0(df$V1, collapse = "")
all <- unlist(strsplit(all, split = ""))
all <- sort(c(unique(all), "."))

# Seperate characters by spaces
for (i in 1:length(all))
{
  cat(all[i], "\n")
  df$V2 <- gsub(all[i], paste0(" ", all[i]), df$V2, fixed = T)
}

# Select a random sample of 300,000 words
df <- df[sample(nrow(df), 300000), ] # 300000 is ok

# Write result to file
write.table(df, "hyph_fkw.dat", quote = F, sep = "", row.names = F, col.names = F)

# Train model with Phonetisaurus
system("phonetisaurus train --corpus corpus.txt --model hyph.fst --casing ignore hyph_fkw.dat")
