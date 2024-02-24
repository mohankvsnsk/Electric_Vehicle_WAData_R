#Venkata Satya Nagendra Sai Krishna Mohan Kocherlakota 10/14/2023 ALY6000: -Project-4
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

# Reading the data set into a data frame.
elec_vehicles <- read_csv("Electric_Vehicle_Population_Data.csv")
# Cleaning the column names using janitor package.
elec_vehicles <- clean_names(elec_vehicles)
elec_vehicles
#Checking Data types
elec_vehicles <- str(elec_vehicles)
#Removing unwanted column and rows
elec_vehicles_clean <- slice(elec_vehicles, 1: 2845)
elec_vehicles_clean
elec_vehicles_clean = subset(elec_vehicles_clean, 
                       select = -c(vin_1_10, state, postal_code, base_msrp,
                                   legislative_district, dol_vehicle_id, vehicle_location , x2020_census_tract ))
elec_vehicles_clean
glimpse(elec_vehicles_clean)
#Creating a new data frame using make and model_year
elec_vehicles_count <- elec_vehicles_clean |> 
  group_by(make, model_year) |> 
  summarise(num_vehicles = n()) |> 
  mutate(car_model = paste(make, model_year, sep="_")) |>
  arrange(desc(num_vehicles)) |>
  filter(num_vehicles > 30)
elec_vehicles_count = subset(elec_vehicles_count, select = -c(make, model_year))
elec_vehicles_count
#Bar Plot-1
bar_plot_count <-  ggplot(elec_vehicles_count, aes(x = car_model, y = num_vehicles)) + 
  geom_bar(stat = "Identity", fill = "red") +
  labs(x = "Car Model", y = " Number of Vehicles", title = "Bar Plot of Car vs Vehicle Count") + 
  easy_rotate_x_labels(angle = 40, side = c("right"), teach = FALSE)
bar_plot_count
#Creating a new data frame using city
elec_vehicles_city <- elec_vehicles_clean |> 
  group_by(city) |>
  summarise(num_vehicles = n()) |>
  arrange(desc(num_vehicles)) |>
  filter(num_vehicles > 50)
elec_vehicles_city
#Bar Plot-2
bar_plot_city <-  ggplot(elec_vehicles_city, aes(x = city, y = num_vehicles)) + 
  geom_bar(stat = "Identity", fill = "green") +
  labs(x = "City", y = " Number of Vehicles", title = "Bar Plot of City vs Num of Vehicles") + 
  easy_rotate_x_labels(angle = 40, side = c("right"), teach = FALSE)
bar_plot_city
#Creating a new data frame using city
elec_vehicles_pie <- elec_vehicles_clean |> 
  group_by(city) |>
  summarise(num_vehicles = n()) |>
  arrange(desc(num_vehicles)) |>
  filter(num_vehicles > 50) |>
  mutate(percent=piepercent<- (100* elec_vehicles_pie$num_vehicles/sum(elec_vehicles_pie$num_vehicles)))
elec_vehicles_pie
#Pie Chart
pie = ggplot(elec_vehicles_pie, aes(x="", y=num_vehicles, fill=city)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(elec_vehicles_pie$percent), "%")), position = position_stack(vjust = 0.5))
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "City - Number of Vehicles")
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

pie
#Creating a new data frame using multiple functions
by_type <- elec_vehicles_clean %>% group_by(model) %>% 
  summarise(count = n(), avg_range = mean(electric_range, na.rm = TRUE)) |> 
  arrange(desc(avg_range)) |>
  filter(count > 50)
by_type

#Line graph
line_plot <- ggplot(by_type, aes(x = model, y = count, color = avg_range)) +
  geom_line() + 
  geom_point(size = 2) +
  labs(x = "Model", y = "Average Range") +
  ggtitle("Types of Model and Average Range.") + theme_excel_new() +
    easy_rotate_x_labels(angle = 80, side = c("right"), teach = FALSE)
line_plot

#Statistics analysis

# Set the seed
set.seed(123)

sample_size <- 100

sample1 <- sample(elec_vehicles_clean$electric_range, size = sample_size)
sample2 <- sample(elec_vehicles_clean$electric_range, size = sample_size)
sample3 <- sample(elec_vehicles_clean$electric_range, size = sample_size)

# Compute sample statistics
sample_mean1 <- mean(sample1)
sample_variance1 <- var(sample1)
sample_sd1 <- sd(sample1)

sample_mean2 <- mean(sample2)
sample_variance2 <- var(sample2)
sample_sd2 <- sd(sample2)

sample_mean3 <- mean(sample3)
sample_variance3 <- var(sample3)
sample_sd3 <- sd(sample3)

sample_data1 <- tibble(sample_mean1, sample_variance1, sample_sd1)
sample_data2 <- tibble(sample_mean2, sample_variance2, sample_sd2)
sample_data3 <- tibble(sample_mean3, sample_variance3, sample_sd3)
mean= mean(elec_vehicles_clean$electric_range)
var = var(elec_vehicles_clean$electric_range)
sd = sd(elec_vehicles_clean$electric_range)
stat_data <- tibble(mean, var, sd)
stat_data
sample_data1
sample_data2
sample_data3

stat_mean <- data.frame(mean1 = stat_data$mean, 
                        mean2 = sample_data1$sample_mean1, 
                        mean3 = sample_data2$sample_mean2, 
                        mean4 = sample_data3$sample_mean3)

stat_var <- data.frame(var1 = stat_data$var, 
                       var2 = sample_data1$sample_variance1, 
                       var3 = sample_data2$sample_variance2, 
                       var4 = sample_data3$sample_variance3)

stat_sd <- data.frame(sd1 =stat_data$sd, 
                      sd2 = sample_data1$sample_sd1, 
                      sd3 = sample_data2$sample_sd2, 
                      sd4 = sample_data3$sample_sd3)

data_stat <- data.frame(stat_mean, stat_sd)

# Create a bar plot with different colors for means and variances
barplot(as.matrix(data_stat),
        beside = TRUE,
        col = c("blue", "red"),  # Set different colors for means and variances
        main = "Means and Variances",
        ylab = "Value"
)

