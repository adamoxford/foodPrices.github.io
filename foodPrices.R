# R Script to Generate chart_state_1.json from newCPIdata.csv
# This version builds the JSON spec directly as an R list,
# bypassing ggplot and ggvega to avoid conversion errors.

# --- 1. Install Packages ---
# Install packages if you don't have them
#if(!require(tidyverse)) install.packages("tidyverse")
#if(!require(lubridate)) install.packages("lubridate")
#if(!require(jsonlite)) install.packages("jsonlite")

# --- 2. Load Libraries ---
library(tidyverse)
library(lubridate)
library(jsonlite)
library(vegawidget) 

# --- 3. Configuration ---
input_file <- "newCPIdata.csv"
output_file <- "chart_state_1.json"

# --- 4. Load and Process Data ---
# Load the raw data
df_raw <- read_csv(input_file, show_col_types = FALSE)

# Pivot to long format and clean
df_clean <- df_raw %>%
  pivot_longer(
    cols = -Description,
    names_to = "Date_str",
    values_to = "Value_str"
  ) %>%
  mutate(
    Value = as.numeric(str_replace(Value_str, "%", "")) / 100,
    Date = parse_date_time(Date_str, "m/y")
  ) %>%
  drop_na(Value, Date) %>%
  # Format Date as "YYYY-MM-DD" for JSON compatibility
  mutate(Date = format(Date, "%Y-%m-%d"))

# --- 5. Define Chart Elements ---
# Define the color mapping
all_indicators <- unique(df_clean$Description)
green_indicators <- c("All Items", "Food")

color_domain <- all_indicators
color_range <- ifelse(
  all_indicators %in% green_indicators,
  ifelse(all_indicators == "All Items", "#006400", "#990091"), # Dark greens
  "lightgrey"                                                  # Grey for all others
)

# Define axis limits
min_date <- min(df_clean$Date)
end_date <- "2025-11-01" # As a string

# --- 6. Build the Vega-Lite Specification (as an R List) ---

# This time, we build each layer as a complete object
# to avoid ambiguity in the R previewer.

# Layer 1: The lines
layer_lines <- list(
  data = list(values = df_clean),
  mark = list(type = "line", point = FALSE), # No points
  encoding = list(
    x = list(
      field = "Date", 
      type = "temporal", 
      title = "Date",
      axis = list(format = "%Y-%m", grid = FALSE), # No vertical gridlines
      scale = list(domain = list(min_date, end_date)) 
    ),
    y = list(
      field = "Value", 
      type = "quantitative", 
      title = "Year-on-Year Inflation",
      axis = list(format = ".1%")
    ),
    color = list(
      field = "Description",
      type = "nominal",
      scale = list(domain = color_domain, range = color_range),
      legend = FALSE # No legend
    ),
    tooltip = list(
      list(field = "Description", type = "nominal"),
      list(field = "Date", type = "temporal", title = "Date", format = "%B %Y"),
      list(field = "Value", type = "quantitative", title = "Inflation", format = ".1%")
    )
  )
)

# Layer 2: The labels
layer_labels <- list(
  data = list(values = df_clean),
  mark = list(
    type = "text",
    align = "left",
    dx = 5,
    fontSize = 14,
    dy = list(expr = "datum.Description == 'All Items' ? -25 : 25")
  ),
  encoding = list(
    # Find the last point in the full dataset
    x = list(field = "Date", type = "temporal", aggregate = "max"), 
    y = list(field = "Value", type = "quantitative", aggregate = list(argmax = "Date")),
    # Color the text to match the lines
    color = list(
      field = "Description",
      type = "nominal",
      scale = list(domain = color_domain, range = color_range),
      legend = "null"
    ),
    # Set the text content
    text = list(field = "Description", type = "nominal")
  ),
  # Filter this layer to ONLY show the green labels
  transform = list(
    list(filter = "datum.Description == 'All Items' || datum.Description == 'Food'")
  )
)

# Combine the layers into the final specification
vl_spec <- list(
  `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
  description = "South Africa Monthly CPI Inflation",
  width = "container",
  height = "container",
  background = "white",
  title = list(text = "Monthly CPI Inflation Timeline"),
  view = list(stroke = NULL),
  
  # Add the two layers
  layer = list(
    layer_lines,
    layer_labels
  )
)

print("Displaying chart preview...")
print(vegawidget(vl_spec))


# --- 7. Write JSON File ---
write_json(
  vl_spec,
  output_file,
  auto_unbox = TRUE, # CRITICAL for Vega-Lite compatibility
  pretty = TRUE
)

print(paste("Successfully generated", output_file))