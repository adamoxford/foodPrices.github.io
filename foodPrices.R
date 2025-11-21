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
output_file <- "chart_state_6.json"

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
    Date = parse_date_time(Date_str, "m/y"),
    Description = ifelse(Description == "Milk, other dairy products and eggs", "Dairy", Description)
  ) %>%
  drop_na(Value, Date) %>%
  # Format Date as "YYYY-MM-DD" for JSON compatibility
  mutate(Date = format(Date, "%Y-%m-%d"))

# --- 5. Define Chart Elements ---
# Define the color mapping
all_indicators <- unique(df_clean$Description)
green_indicators <- c("Meat", "Vegetables", "Dairy")

color_domain <- all_indicators
color_range <- ifelse(
  all_indicators == "Meat", "#006400", 
  ifelse(all_indicators == "Vegetables", "#990091",
  ifelse(all_indicators == "Dairy", "#000064",
  "lightgrey"                                                  # Grey for all others
)))

# Define axis limits
min_date <- min(df_clean$Date)
end_date <- "2025-11-01" # As a string

# --- NEW: Pre-calculate the label positions ---
# This is more robust than asking Vega-Lite to do it.
df_labels <- df_clean %>%
  group_by(Description) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  # Filter for only the labels we want to show
  filter(Description %in% green_indicators)
# --- NEW: Pre-calculate the dot positions (Dec 2021) ---
df_dots <- df_clean %>%
  filter(Date == "2025-08-01") # Filter for Dec 2021


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
      axis = list(format = "%Y-%m", 
                  grid = FALSE,
                  titleFontSize = 14,
                  labelFontSize = 12), # No vertical gridlines
      scale = list(domain = list(min_date, end_date)) 
    ),
    y = list(
      field = "Value", 
      type = "quantitative", 
      title = "Year-on-Year Inflation",
      axis = list(format = ".1%",
                  titleFontSize = 14,
                  labelFontSize = 12,
                  domainOpacity = 0)
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
    dy = list(expr = "datum.Description == 'Meat' ? 0 : 0 || datum.Description == 'Vegetables' ? 30 : 10")
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
    list(filter = "datum.Description == 'Vegetables' || datum.Description == 'Meat' || datum.Description == 'Dairy'")
  )
)

# --- NEW: Layer 3, The Dots ---
layer_dots <- list(
  data = list(values = df_dots),
  mark = list(type = "point", size = 100, filled = TRUE, stroke = "black", strokeWidth = 0.5),
  encoding = list(
    x = list(field = "Date", type = "temporal"),
    y = list(field = "Value", type = "quantitative"),
    color = list(
      field = "Description",
      type = "nominal",
      scale = list(domain = color_domain, range = color_range),
      legend = FALSE
    ),
    tooltip = list(
      list(field = "Description", type = "nominal"),
      list(field = "Date", type = "temporal", title = "Date", format = "%B %Y"),
      list(field = "Value", type = "quantitative", title = "Inflation", format = ".1%")
    )
  )
)


# Combine the layers into the final specification
vl_spec <- list(
  `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
  description = "South Africa Monthly CPI Inflation",
  width = "container",
  height = "container",
  background = "white",
  title = list(text = "Monthly CPI Inflation Timeline",
               fontSize = 18),
  view = list(stroke = "transparent"),
  
  # Add the two layers
  layer = list(
    layer_lines,
    layer_labels,
    layer_dots
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



## Food basket chart ##
## This is a new bit of code for the food basket chart only ##

input_file <- "foodBasketprice.csv"
output_file <- "chart_food_basket.json"

# --- 3. Load and Process Data ---
df_raw <- read_csv(input_file, show_col_types = FALSE)

# Clean data
df_clean <- df_raw %>%
  rename(
    Date_str = `Year Month`,
    Value = `Monthly price of a THFB`
  ) %>%
  mutate(
    # Parse "01/04/21" format (Day/Month/Year)
    Date = dmy(Date_str),
    # Create the label text: "2021: R2942"
    LabelText = paste0(year(Date), ": R", Value),
    # --- FIX: Create a pre-formatted price for the tooltip ---
    TooltipPrice = paste0("R", format(Value, big.mark = ",", scientific = FALSE))
  ) %>%
  drop_na(Value, Date) %>%
  mutate(Date_fmt = format(Date, "%Y-%m-%d")) # For JSON

# --- 4. Identify Start and End Points ---
# We filter for the minimum and maximum dates to get the start/end points
df_endpoints <- df_clean %>%
  filter(Date == min(Date) | Date == max(Date))

# --- 5. Build Vega-Lite Specification ---

# Layer 1: The Line
layer_line <- list(
  data = list(values = df_clean),
  mark = list(type = "line", point = FALSE),
  encoding = list(
    x = list(
      field = "Date_fmt",
      type = "temporal",
      title = "Date",
      axis = list(
        format = "%Y",       # Show Years on X axis
        grid = FALSE,
        titleFontSize = 14,
        labelFontSize = 12
      )
    ),
    y = list(
      field = "Value",
      type = "quantitative",
      title = "Cost of Food Basket (Rands)",
      axis = list(
        # Hide the Y-axis line and ticks, keep the text
        domain = FALSE,      
        ticks = FALSE,       
        titleFontSize = 16,  
        labelFontSize = 0,  
        domainOpacity = 0
      ),
      # Ensure the Y axis doesn't start at 0 to show the trend better
      scale = list(zero = FALSE) 
    ),
    color = list(value = "#228B22"), # "Food" Green color
    tooltip = list(
      list(field = "Date_fmt", type = "temporal", title = "Date", format = "%B %Y"),
      list(field = "TooltipPrice", type = "nominal", title = "Price")
    )
  )
)

# Layer 2: The Dots (Start and End only)
layer_dots <- list(
  data = list(values = df_endpoints),
  mark = list(
    type = "point",
    size = 100,
    filled = TRUE,
    color = "#228B22", # Match line color
    stroke = "black",
    strokeWidth = 0.5
  ),
  encoding = list(
    x = list(field = "Date_fmt", type = "temporal"),
    y = list(field = "Value", type = "quantitative")
  )
)

# Layer 3: The Labels (Start and End only)
layer_labels <- list(
  data = list(values = df_endpoints),
  mark = list(
    type = "text",
    align = "left",
    fontSize = 18,
    fontWeight = "bold",
    # Conditional alignment:
    # If date is '2021-04-01', align 'right' (push text to left of dot)
    # Otherwise, align 'left' (push text to right of dot)
    align = list(expr = "datum.Date_fmt == '2021-04-01' ? 'right' : 'left'"),
    
    # Conditional horizontal nudge (dx):
    # If date is '2021-04-01', nudge -10 (left)
    # Otherwise, nudge 10 (right)
    dx = list(expr = "datum.Date_fmt == '2021-04-01' ? -10 : 10"),
    
    # --- NEW: Conditional vertical nudge (dy) ---
    # If date is '2021-04-01', nudge -10 (UP)
    # Otherwise, nudge 0 (default)
    dy = list(expr = "datum.Date_fmt == '2021-04-01' ? -10 : -35")
  ),
  
  encoding = list(
    x = list(field = "Date_fmt", type = "temporal"),
    y = list(field = "Value", type = "quantitative"),
    text = list(field = "LabelText", type = "nominal")
  )
)

layer_labels_halo <- layer_labels
layer_labels_halo$mark$stroke <- "white"
layer_labels_halo$mark$strokeWidth <- 4


# Combine Layers
vl_spec <- list(
  `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
  description = "Food Basket Price Trend",
  width = "container",
  height = "container",
  background = "white",
  title = list(
    text = "Household Food Basket Price",
    fontSize = 18
  ),
  view = list(stroke = "transparent"),
  
  # Padding to ensure labels fit
  padding = list(top = 10, bottom = 20, left = 40, right = 80),
  
  layer = list(
    layer_line,
    layer_dots,
    layer_labels_halo,
    layer_labels
  )
)

# --- 6. Save ---
write_json(
  vl_spec,
  output_file,
  auto_unbox = TRUE,
  pretty = TRUE
)

print(paste("Successfully generated", output_file))