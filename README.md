Downloading a Dataset in R and Summarizing Data

To download a dataset in R:

From a URL: Use the readr package's read_csv() function for CSV files.

# Install the package
    install.packages("readr")
    library(readr)

# Load dataset from URL
    url <- "https://example.com/dataset.csv"
    dataset <- read_csv(url)

From Local Files: Use the read.csv() or read_csv() function to load files stored locally.

    dataset <- read_csv("path/to/local/file.csv")

Basic Summary Methods in R

After loading the dataset, summarize it to understand its structure and contents:

Structure: Check the data types and layout.

    str(dataset)

Summary Statistics: Get a quick statistical summary of numeric variables.

    summary(dataset)

Overview: Use the skimr package for an enhanced summary.

    install.packages("skimr")
    library(skimr)
    skim(dataset)

Head and Tail: View the first or last few rows.

    head(dataset)  # First 6 rows
    tail(dataset)  # Last 6 rows

These steps provide insights into the dataset's structure, variables, and possible cleaning requirements.
