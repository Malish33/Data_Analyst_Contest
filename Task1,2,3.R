install.packages("readxl")

library(readxl)

breach_file <- "breach_report.xls"

breach_data <- read_excel(breach_file)

fields <- c("Name of Covered Entity", "State", "Covered Entity Type",
            "Individuals Affected", "Breach Submission Date", "Type of Breach",
            "Location of Breached Information", "Business Associate Present",
            "Web Description")

extracted_data <- breach_data[, fields]

print(extracted_data)

metadata <- list(
  Date_Scraped = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  Category = "OCR_Breaches_Archive"
)
json_data <- list(
  metadata = metadata,
  breach_data = extracted_data
)
json_string <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)

output_file <- "output.json"

cat(json_string, file = output_file)

cat("JSON file created successfully:", output_file, "\n")

validate_json <- function(json_data) {
  
  expected_fields <- c(
    "metadata" = "list",
    "breach_data" = "data.frame"
  )
  

  for (field_name in names(expected_fields)) {
    if (!field_name %in% names(json_data)) {
      stop(paste("Field", field_name, "is missing in the JSON data."))
    }
    
    if (class(json_data[[field_name]]) != expected_fields[field_name]) {
      stop(paste("Incorrect value type for field", field_name, ". Expected:", expected_fields[field_name], "Actual:", class(json_data[[field_name]])))
    }
  }
  

  return(TRUE)
}

json_file <- "path/to/output.json"  # Update with the actual JSON file path
json_data <- jsonlite::fromJSON(file = json_file)


tryCatch(
  {
    validation_result <- validate_json(json_data)
    cat("JSON data validation passed.\n")
  },
  error = function(e) {
    cat("JSON data validation failed:\n", e$message, "\n")
  }
)

library(ggplot2)


json_file <- "output.json"

breach_data <- json_data$breach_data

state_counts <- table(breach_data$State)

state_counts_df <- data.frame(State = names(state_counts), Breach_Count = as.numeric(state_counts))

state_counts_df <- state_counts_df[order(state_counts_df$Breach_Count, decreasing = TRUE), ]

bar_plot <- ggplot(data = state_counts_df, aes(x = State, y = Breach_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("State") +
  ylab("Number of Reported Breaches") +
  ggtitle("Number of Reported Breaches by State") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
print(bar_plot)

breach_type_counts <- table(breach_data$`Type of Breach`)

breach_type_counts_df <- data.frame(Breach_Type = names(breach_type_counts), Count = as.numeric(breach_type_counts))

breach_type_counts_df <- breach_type_counts_df[order(breach_type_counts_df$Count, decreasing = TRUE), ]

bar_plot <- ggplot(data = breach_type_counts_df, aes(x = reorder(Breach_Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Breach Type") +
  ylab("Frequency") +
  ggtitle("Most Common Breach Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

library(ggplot2)
library(dplyr)
library(lubridate)

json_file <- "output.json"

breach_data <- json_data$breach_data

breach_data$`Breach Submission Date` <- lubridate::mdy(breach_data$`Breach Submission Date`)

breach_counts <- breach_data %>%
  group_by(`Breach Submission Date`) %>%
  summarize(Count = n())

line_plot <- ggplot(data = breach_counts, aes(x = `Breach Submission Date`, y = Count)) +
  geom_line() +
  xlab("Breach Submission Date") +
  ylab("Number of Breaches") +
  ggtitle("Number of Breaches Over Time")
print(line_plot)

breach_data <- json_data$breach_data

entity_type_counts <- table(breach_data$`Covered Entity Type`)

entity_type_counts_df <- data.frame(Entity_Type = names(entity_type_counts), Count = as.numeric(entity_type_counts))

entity_type_counts_df <- entity_type_counts_df[order(entity_type_counts_df$Count, decreasing = TRUE), ]

bar_plot <- ggplot(data = entity_type_counts_df, aes(x = reorder(Entity_Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Entity Type") +
  ylab("Frequency") +
  ggtitle("Most Common Entity Types Involved in Breaches") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
print(bar_plot)
