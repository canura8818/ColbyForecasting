# Tatar Anurakwongsri
# Assignment 1
# Use the Brickman tutorial to extract data from the location of Buoy M01 for 
# RCP4.5 2055. Make a plot of SST (y-axis) as a function of month (x-axis)

#set up
source("setup.R")

# Load data
DB = brickman_database()
buoys = gom_buoys()

# Filter data
db = DB |>
  dplyr::filter(scenario == "RCP45", 
                year == 2055,
                interval == "mon",
                var == "SST")
x = read_brickman(db)

buoy_M01 = buoys |>
  dplyr::filter(id == "M01")

M01_SST_data = extract_brickman(x, buoy_M01)
M01_SST_data = M01_SST_data |>
  mutate(month = factor(month, levels = month.abb))

ggplot(data = M01_SST_data) +
  geom_point(mapping = aes(x = month, y = value)) +
  ggtitle("RCP45 2055 SST at Buoy M01") +
  xlab("Month") +
  ylab("SST (C)")
