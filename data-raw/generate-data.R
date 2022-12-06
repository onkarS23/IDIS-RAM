# Generate synthetic data

# Parameters

path_to_data <- "data"

# Generate and save data

# WARNING!!! As of 5 December 2022: This often generates an error (Gumbel dist)!
# sormas_testdata <- sormasdatagen::GenerateSORMASData()

# Export CSVs
for (ds in names(sormas_testdata)) {
  write.csv(sormas_testdata[[ds]], paste0(path_to_data, "/", ds, ".csv"))
}

# Convert the CSVs to CSVs that can be imported in SORMAS
source("R/ConvertForSORMAS.R")
sormas_case_conv(
  paste0(path_to_data, "/", "persons.csv"),
  paste0(path_to_data, "/", "symptoms.csv"),
  paste0(path_to_data, "/", "sormas_cases.csv"),
)

# Remove CSV exports that aren't used afterwards
file.remove(
  paste0(path_to_data, "/", names(sormas_testdata), ".csv")
)
