library(rvest)
library(dplyr)
library(stringr)
library(here)
library(rio)

# All UO Course (web scrapping) ----

# Base URL
base_url <- "https://catalog.uoregon.edu/courses/"

# Read the main catalog page
main_page <- read_html(base_url)

#to extract html codes???
#html_content <- page %>% as.character()
#writeLines(html_content, "page_content.html")

# Extract all department/course links
main_page_lists <- main_page %>%
  html_nodes("ul li a") %>%  # Adjust selector if needed
  html_attr("href") #%>%

course_links <- main_page_lists[grepl("^/courses/crs-", main_page_lists)]

full_course_links <- paste0("https://catalog.uoregon.edu", course_links) # Append full URL


extract_courses <- function(url) {
  page <- read_html(url)
  
  # Extract course titles, codes, and descriptions
  courses <- page %>%
    html_nodes("p.courseblocktitle.noindent")  %>%  # Adjust selector if needed
    html_text(trim = TRUE)
  
  return(courses)
}

# Scrape multiple department pages
all_courses <- lapply(full_course_links, extract_courses)

course_titles <- unlist(all_courses)


parse_course_info <- function(course_vector) {
  # Create empty vectors to store parsed information
  course_code <- character(length(course_vector))
  title <- character(length(course_vector))
  credits <- character(length(course_vector))
  
  for (i in seq_along(course_vector)) {
    # Split the string by periods that are followed by spaces
    parts <- strsplit(course_vector[i], "\\.\\s+")[[1]]
    
    # Extract course code (first part)
    course_code[i] <- trimws(parts[1])
    
    # Extract title (middle part)
    # Handle cases where title might contain periods
    title_parts <- parts[2:(length(parts)-1)]
    title[i] <- trimws(paste(title_parts, collapse = ". "))
    
    # Extract credits (last part)
    credit_part <- parts[length(parts)]
    # Extract number from credit string using regex
    credits[i] <- gsub(".*?(\\d+(?:-\\d+)?)\\s*Credits.*", "\\1", credit_part)
  }
  
  # Create data frame
  result_df <- data.frame(
    course_code = course_code,
    title = title,
    credits = credits,
    stringsAsFactors = FALSE
  )
  
  return(result_df)
}

course_df <- parse_course_info(course_titles)

course_df_final <- course_df |> 
  mutate(department = str_extract(course_code, "^[A-Za-z]+"),
         course_number = str_extract(course_code, "\\d+[A-Za-z]*$"),
         course_code = paste(department,course_number)) |> 
  unique() |> 
  filter(course_number < 500) # only undergrad classes (100-499)

## Write to CSV ----
write.csv(course_df_final, "Final_project codes/datasets/all_course_uo.csv")


# All department ----
departments <- course_df_final %>%
  select(department) |> 
  unique()

## Write to CSV ----
write.csv(departments, "Final_project codes/datasets/departments.csv")



# UO req (web scrapping) ----
url <- "https://registrar.uoregon.edu/graduation/degree-requirements/areas-inquiry-cultural-literacy#list-of-courses"  

page <- read_html(url)

#to extract html codes???
#html_content <- page %>% as.character()
#writeLines(html_content, "page_content.html")


# Function to clean whitespace from strings
# clean_text <- function(x) {
#   str_trim(gsub("\\s+", " ", x))
# }

# Create a function to parse the HTML table
parse_course_table <- function(url) {
  # Read the HTML
  page <- read_html(url)
  
  # Extract data from each column
  subjects <- page %>% 
    html_nodes(".views-field-field-subject-code") %>%
    html_text() #%>%
  #clean_text()
  
  course_numbers <- page %>%
    html_nodes(".views-field-field-course-number-1") %>%
    html_text() #%>%
  #clean_text()
  
  titles <- page %>%
    html_nodes(".views-field-title") %>%
    html_text() #%>%
  #clean_text()
  
  requirements <- page %>%
    html_nodes(".views-field-nothing") %>%
    html_text() #%>%
  #clean_text()
  
  # Create dataframe
  courses_df <- data.frame(
    Subject = subjects[-1],  # Remove header row
    CourseNumber = course_numbers[-1],
    Title = titles[-1],
    Requirements = requirements[-1],
    stringsAsFactors = FALSE
  )
  
  # Clean up the dataframe
  courses_df <- courses_df %>%
    mutate(
      Subject = str_trim(Subject),
      CourseNumber = str_trim(CourseNumber),
      Title = str_trim(Title),
      Requirements = str_trim(Requirements)
    )
  
  return(courses_df)
}


# Parse the table
uoreq <- parse_course_table(url)

uoreq_wide <- uoreq |> 
  mutate(Course = paste(Subject, CourseNumber),
         Requirements2 = str_replace_all(Requirements,"SSC", "SocS"),
         AC = case_when(str_detect(Requirements2, "AC")~"AC",
                        TRUE ~ NA), #American Cultures prior to fall 2019
         AL = case_when(str_detect(Requirements2, "A&L")~"AL",
                        TRUE ~ NA), #Arts & Letters from Area of Inquiry
         GP = case_when(str_detect(Requirements2, "GP")~"GP",
                        TRUE ~ NA), #Global Perspectives from Cultural Literacy
         IC = case_when(str_detect(Requirements2, "IC")~"IC",
                        TRUE ~ NA),#International Cultures prior to fall 2019
         IP = case_when(str_detect(Requirements2, "IP")~"IP",
                        TRUE ~ NA), #Identity, Pluralism, and Tolerance prior to fall 2019
         SC = case_when(str_detect(Requirements2, "SC")~"SC",
                        TRUE ~ NA), #Science from Area of Inquiry
         SSC = case_when(str_detect(Requirements2, "SocS")~"SSC",
                         TRUE ~ NA), #Social Science  from Area of Inquiry
         US = case_when(str_detect(Requirements2, "US")~"US",
                        TRUE ~ NA) #US: Difference, Inequality, Agency from Cultural Literacy
  ) 


uoreq_long <- uoreq_wide |> 
  pivot_longer(cols = c(AC, AL, GP, IC, IP, SC, SSC, US),
               names_to = "Requirement",
               values_to = "UO_Reqirement") |> 
  filter(!is.na(UO_Reqirement)) |>
  select(Course, Title, Requirements, UO_Reqirement) 

generate_rank_lists <- function(n, name_box) {
  lapply(1:n, function(i) {
    rank_list(
      text = name_box[i],  # Use the corresponding name from name_box
      labels = list(),
      input_id = paste0("rank_list_", i + 1),  # Unique ID for each list
      options = sortable_options(group = "mygroup")
    )
  })
}
uoreq_long <- uoreq_long |> 
  rename("course_code" = "Course",
         "title" = "Title",
         "requirements" = "Requirements",
         "uo_requirements" = "UO_Reqirement")

uoreq_wide <- uoreq_wide |> 
  rename("course_code" = "Course",
         "title" = "Title",
         "requirements" = "Requirements")


# View the first few rows
head(uoreq_long)

## Write to CSV ----
write.csv(uoreq_wide, "Final_project codes/datasets/uo_req_courses_wide.csv", row.names = FALSE)
write.csv(uoreq_long, "Final_project codes/datasets/uo_req_courses_long.csv", row.names = FALSE)

# CDS typical progression ----
cds_progression <- import(here("Final_project codes_download copy/datasets/CDS undergrad typical course.xlsx"))[, 1:7] |> 
  mutate(term_code = case_when(term == "Fall" ~ 1,
                              term == "Winter" ~ 2,
                              term == "Spring" ~ 3,
                              TRUE ~ 999,
                              ),
         label = case_when(is.na(code) ~ title,
                           TRUE ~ paste(code, "\n", title)),
         color = case_when(term == "Fall" ~ "#489D46",
                           term == "Winter" ~ "#E2E11B",
                           term == "Spring" ~ "#8ABB40",
                           TRUE ~ "red"
         )) |> 
  group_by(year, term) |> 
  mutate(
  identifier = row_number(),  # Unique number per (year, term)
  identifier1 = paste0(term, "_", identifier),
  )
export(cds_progression, "Final_project codes_download copy/datasets/cds_progression.csv")




