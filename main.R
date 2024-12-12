library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)

# Macros
VAULT = "~/Dropbox/PKM/Jakubrain"
VAULT_TEMPLATE_FILE = "Efforts - Academic - Template"  #skip this file
YAML_CHECK = "acdCode:"  #start of relevant yaml
COLLECTION_CHECK = "\\[\\[Academic Efforts\\]\\]"
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

grepl_any <- function(pattern, x, i = NULL) {
  if (is.null(i)) {
    sum(grepl(pattern, x)) > 0
  } else {
    
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

make_client <- function(df) {
  txt <- 
}


# Add markdown
add_markdown <- function(df) {
  df |>
    mutate(
      title = paste0("**", title, "**"),
      projectPeople = 
        case_when(
          grepl("yls-library|data-service", projectGroup) ~ "",
          TRUE ~ paste0("(", projectPeople, ")")
        ),
      assignedTo = paste("Personnel: ", acdAssigned),
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
# add_markdown <- function(df) {
#   df %>%
#     mutate(
#       title = paste0("**", title, "**"),
#       projectPeople = 
#         case_when(
#           grepl("yls-library|data-service", projectGroup) ~ "",
#           TRUE ~ paste0("(", projectPeople, ")")
#         ),
#       projectStatus = paste0("*STATUS: ", projectStatus, "*"),
#       lineout = paste(
#         str_trim(paste0(title, " ", projectPeople)),
#         str_trim(projectSummary),
#         str_trim(projectStatus),
#         "\n",
#         sep = "\n"
#       )
#     )
# }


# Create dataframe from file
md_to_df <- function(f, checked_databytes = TRUE, this_month_only = FALSE) {
  
  if (get_filename(f) != VAULT_TEMPLATE_FILE) {
    
    y <- readr::read_file(f)
    y_chr <- readLines(f, warn = FALSE)
    yaml_pos <- str_locate_all(y, "---")
    start <- yaml_pos[[1]][1, 2] + 1
    stop <- yaml_pos[[1]][2, 1] - 1
    y <- str_trim(substr(y, start, stop))
    y <- gsub(":\n  ", ": ", z)
    y <- gsub("\n ", " ", z)
    y <- unlist(str_split(y, "\n"))
    #y <- readLines(f, warn = FALSE)
    
    if (has_properties(y_chr)) {
      if (is_data_project(y)) {
        df <- data.frame(
          name = str_split_i(y, ":", 1),
          value = str_trim(str_split_i(y, ":", 2))
          ) |>
          filter(grepl("created|updated|acd|databytes", name)) |>
          pivot_wider() |>
          mutate(
            title = get_filename(f),
            projectGroup = acdOrgType, #factorize_group(projectGroup),
            across(
              c(created, updated), as_date
            ),
            across(
              c(acdSummary, acdCurrent), add_period
            ),
            last_name = word(str_split_i(projectPeople, ",", 1), -1)
          ) %>%
          add_markdown()
        
        if (this_month_only) {
          df <- df %>%
            filter(month(updated) == MONTH)
        }
        
        if (checked_databytes) {
          df <- df %>%
            filter(databytes == "true")
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
  type <- df$acdType
  grp <- df$projectGroup
  
  # Sort by type
  if (grepl_any("Research", type)) {
    txt <- paste(txt, "# Research Projects and Consultations\n", sep = "\n")
    if (grepl_any("yls", grp)) {
      txt <- paste(txt, "## At YLS\n", sep = "\n")
    }
    else if (grepl_any("yale", grp)) {
      txt <- paste(txt, "## Around Yale\n", sep = "\n")
    }
    else {
      txt <- paste(txt, "## External\n", sep = "\n")
    }
  else if (grepl_any("Data", type)) {
    txt <- paste(txt, "# Data Projects and Services\n", sep = "\n")
  }
  else if (grepl_any("Outreach|Professional", type)) {
    txt <- paste(txt, " #Outreach and Professional Services\n", sep = "\n")
  } 
  else {
    txt <- paste(txt, "# Other Efforts\n", sep = "\n")
  }
  #   # law school stakeholders
  #   if (grepl_any("faculty|student", grp) & grepl()) {
  #     txt <- paste(txt, "## YLS Faculty\n", sep = "\n")
  #     txt_add <- df %>%
  #       filter(projectGroup =="yls-faculty") %>%
  #       pull(lineout) %>%
  #       paste(., collapse = "\n")
  #     txt <- paste(txt, txt_add, collapse = "\n")
  #   }
  #   if (grepl_any("yls-student", grp)) {
  #     txt <- paste(txt, "## YLS Students and Fellows\n", sep = "\n")
  #     txt_add <- df %>%
  #       filter(projectGroup =="yls-student") %>%
  #       pull(lineout) %>%
  #       paste(., collapse = "\n")
  #     txt <- paste(txt, txt_add, collapse = "\n")
  #   }
  #   if (grepl_any("yls-admin", grp)) {
  #     txt <- paste(txt, "## YLS Administration\n", sep = "\n")
  #     txt_add <- df %>%
  #       filter(projectGroup =="yls-admin") %>%
  #       pull(lineout) %>%
  #       paste(., collapse = "\n")
  #     txt <- paste(txt, txt_add, collapse = "\n")
  #   }
  #   if (grepl_any("yls-library", grp)) {
  #     txt <- paste(txt, "## Law Library\n", sep = "\n")
  #     txt_add <- df %>%
  #       filter(projectGroup =="yls-library") %>%
  #       pull(lineout) %>%
  #       paste(., collapse = "\n")
  #     txt <- paste(txt, txt_add, collapse = "\n")
  #   }
    
  #   #around yale
  #   if (grepl_any("yale-", grp)) {
  #     txt <- paste(txt, "## Around Yale\n", sep = "\n")
  #     txt_add <- df %>%
  #       filter(grepl("yale-", projectGroup)) %>%
  #       pull(lineout) %>%
  #       paste(., collapse = "\n")
  #     txt <- paste(txt, txt_add, collapse = "\n")
  #   }
    
  #   #external
  #   ###pending....
  # }
  
  # # Data Services/Projects
  # if (grepl_any("data-service", grp)) {
  #   txt <- paste(txt, "# Data Projects and Services\n", sep = "\n")
  #   txt_add <- df %>%
  #     filter(projectGroup =="data-service") %>%
  #     pull(lineout) %>%
  #     paste(., collapse = "\n")
  #   txt <- paste(txt, txt_add, collapse = "\n")
  # }
  
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
