library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)


# Macros
VAULT = "~/Dropbox/pkm/Jakubrain"
VAULT_TEMPLATE_FILE = "Data Project Template"
YAML_CHECK = "projectSummary:"
COLLECTION_CHECK = "\\[\\[Data Projects\\]\\]"
MONTH <- lubridate::month(today())
OUTPUT_DIR = paste0(VAULT, "/Efforts/Notes/Data Bytes/")


# Functions --------------------------------------------------------------------
# Check if properties/metadata/yaml in document
has_properties <- function(lines) {
  length(grep("^---$", lines[1:5])) > 0
}

# Extract yaml properties
extract_properties <- function(lines) {
  lines[2:(grep("^---$", lines)[2] - 1)]
}

grepl_any <- function(pattern, x) {
  sum(grepl(pattern, x)) > 0
}

# Check if data project file
is_data_project <- function(yml) {
  if (sum(grepl(YAML_CHECK, yml)) > 0 & sum(grepl(COLLECTION_CHECK, yml)) > 0) {
    TRUE
  } else {
    FALSE
  }
}
  

# Project hierarchy
factorize_group <- function(x) {
  factor(x, c("yls-faculty",
              "yls-student",
              "yls-admin",
              "yls-library",
              "yale-faculty",
              "yale-student",
              "yale-library",
              "data-service"))
}


# Add period
add_period <- function(x) {
  ifelse(grepl("\\.$", x), x, paste0(x, "."))
}


# Get filename from path
get_filename <- function(x) {
  gsub("\\.md$", "", gsub("^.+/", "", x))
}


# Add markdown
add_markdown <- function(df) {
  df %>%
    mutate(
      title = paste0("**", title, "**"),
      projectPeople = 
        case_when(
          grepl("yls-library|data-service", projectGroup) ~ "",
          TRUE ~ paste0("(", projectPeople, ")")
        ),
      projectStatus = paste0("*STATUS: ", projectStatus, "*"),
      lineout = paste(
        str_trim(paste0(title, " ", projectPeople)),
        str_trim(projectSummary),
        str_trim(projectStatus),
        "\n",
        sep = "\n"
      )
    )
}


# Create dataframe from file
md_to_df <- function(f, this_month_only = TRUE) {
  
  if (get_filename(f) != VAULT_TEMPLATE_FILE) {
    
    y <- readLines(f, warn = FALSE)
    
    if (has_properties(y)) {
      if (is_data_project(y)) {
        df <- data.frame(
          name = str_split_i(y, ":", 1),
          value = str_trim(str_split_i(y, ":", 2))
          ) %>%
          filter(grepl("created|updated|project", name)) %>%
          pivot_wider() %>%
          mutate(
            title = get_filename(f),
            projectGroup = factorize_group(projectGroup),
            across(
              c(created, updated), as_date
            ),
            across(
              c(projectSummary, projectStatus), add_period
            ),
            last_name = word(str_split_i(projectPeople, ",", 1), -1)
          ) %>%
          add_markdown()
        
        if (this_month_only) {
          df <- df %>%
            filter(month(updated) == MONTH)
        }
        return(df)
      }
    }
  }
}


# Write markdown header
write_header <- function() {
  paste(
    "*Updates and news from Empirical Research and Data Services*",
    paste0("`", format(Sys.Date(), format="%B %d, %Y"), "`", "\n"),
    "--",
    paste("***NOTE: The following items are stakeholder works-in-progress",  
          "and should be considered confidential.  Please do not circulate",
          "these updates outside of the Law Library.  Thank you!***\n"),
    sep = "\n"
  )
}


# Write to markdown
write_contents <- function(df) {
  
  # Header
  txt <- write_header()
  grp <- df$projectGroup
  
  # Data projects
  if (grepl_any("yale|yls", grp)) {
    txt <- paste(txt, "# Data Projects and Consultations\n", sep = "\n")
    
    # law school stakeholders
    if (grepl_any("yls-faculty", grp)) {
      txt <- paste(txt, "## YLS Faculty\n", sep = "\n")
      txt_add <- df %>%
        filter(projectGroup =="yls-faculty") %>%
        pull(lineout) %>%
        paste(., collapse = "\n")
      txt <- paste(txt, txt_add, collapse = "\n")
    }
    if (grepl_any("yls-student", grp)) {
      txt <- paste(txt, "## YLS Students and Fellows\n", sep = "\n")
      txt_add <- df %>%
        filter(projectGroup =="yls-student") %>%
        pull(lineout) %>%
        paste(., collapse = "\n")
      txt <- paste(txt, txt_add, collapse = "\n")
    }
    if (grepl_any("yls-admin", grp)) {
      txt <- paste(txt, "## YLS Administration\n", sep = "\n")
      txt_add <- df %>%
        filter(projectGroup =="yls-admin") %>%
        pull(lineout) %>%
        paste(., collapse = "\n")
      txt <- paste(txt, txt_add, collapse = "\n")
    }
    if (grepl_any("yls-library", grp)) {
      txt <- paste(txt, "## Law Library\n", sep = "\n")
      txt_add <- df %>%
        filter(projectGroup =="yls-library") %>%
        pull(lineout) %>%
        paste(., collapse = "\n")
      txt <- paste(txt, txt_add, collapse = "\n")
    }
    
    #around yale
    if (grepl_any("yale-", grp)) {
      txt <- paste(txt, "## Around Yale\n", sep = "\n")
      txt_add <- df %>%
        filter(grepl("yale-", projectGroup)) %>%
        pull(lineout) %>%
        paste(., collapse = "\n")
      txt <- paste(txt, txt_add, collapse = "\n")
    }
    
    #external
    ###pending....
  }
  
  # Data Services
  if (grepl_any("data-service", grp)) {
    txt <- paste(txt, "# Data Services\n", sep = "\n")
    txt_add <- df %>%
      filter(projectGroup =="data-service") %>%
      pull(lineout) %>%
      paste(., collapse = "\n")
    txt <- paste(txt, txt_add, collapse = "\n")
  }
  
  # Clean up multiple spaces and ensure each new line does not start with space
  gsub("[\n]{2,}", "\n\n", gsub("\n ", "\n", txt))
}


# Main
main <- function() {
  f <- list.files(VAULT, 
                  pattern = "\\.md$", 
                  recursive = TRUE, 
                  full.names = TRUE)
  
  print("Creating data frame of relevant projects and statuses...")
  df <- map(f, md_to_df) %>%
    list_rbind() %>%
    arrange(projectGroup, desc(last_name))
  
  print("Converting to markdown...")
  txt <- write_contents(df)
  
  fileout <- paste0(OUTPUT_DIR, "YLS Data Bytes ", Sys.Date(), ".md")
  write_lines(txt, file = fileout, sep = "")
  message(paste("Updates successfully written to", fileout))
}
 

# Execute ----------------------------------------------------------------------
main()
