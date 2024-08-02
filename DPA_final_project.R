library(tidyverse)
library(readr)

# Read CSV files into dataframes
df_enriched_1209 <- read_csv("./data_sets/2022-12-09-enriched.csv")
df_enriched_0704 <- read_csv("./data_sets/2022-07-04-enriched.csv")
df_enriched_0627 <- read_csv("./data_sets/2022-06-27-enriched.csv")
df_enriched_0609 <- read_csv("./data_sets/2022-06-09-enriched.csv")
df_enriched_0608 <- read_csv("./data_sets/2022-06-08-enriched.csv")

# Combine all dataframes into one
combined_df <- rbind(df_enriched_0608, df_enriched_0609, df_enriched_0627, df_enriched_0704, df_enriched_1209)

# Display the structure of the combined dataframe
str(combined_df)

head(combined_df)

dim(combined_df)

summary(combined_df)

# Check the number of NA values for each column in the combined dataframe and Display the result
na_counts <- sapply(combined_df, function(column) sum(is.na(column)))
na_counts

# Remove the "notes" column and check the number of NA values for each remaining column
combined_df <- subset(combined_df, select = -c(notes))
na_counts_after_removal <- sapply(combined_df, function(column) sum(is.na(column)))
na_counts_after_removal

str(combined_df)

# Imputation
# Group the data by cve_id and count the number of unique values in each column
unique_grouped_df <- combined_df %>%
  group_by(cve_id) %>%
  summarize_all(n_distinct)

# Check if all columns have the same number of unique values for each cve_id
columns_identical_check <- identical(unique_grouped_df[-1], colSums(!is.na(unique_grouped_df[-1]))[1])
columns_identical_check

# Check which columns have different numbers of unique values
which(!identical(unique_grouped_df[-1], colSums(!is.na(unique_grouped_df[-1]))[1]))

# Check the number of NA values for each column in the combined dataframe
na_counts <- sapply(combined_df, function(column) sum(is.na(column)))
na_counts

# Impute missing values in all columns
imputed_df <- combined_df %>%
  group_by(cve_id) %>%
  mutate(across(everything(), ~if_else(is.na(.), first(.[!is.na(.)]), .)))

imputed_df

# Check the number of NA values for each column in the imputed dataframe
na_counts_imputed <- sapply(imputed_df, function(column) sum(is.na(column)))
na_counts_imputed

# Create a copy of the imputed dataframe for the new numeric one
numeric_encoded_df <- imputed_df

# Encode categorical variables using label encoding
numeric_encoded_df$product <- as.numeric(as.factor(imputed_df$product))
numeric_encoded_df$vulnerability_name <- as.numeric(as.factor(imputed_df$vulnerability_name))
numeric_encoded_df$short_description <- as.numeric(as.factor(imputed_df$short_description))
numeric_encoded_df$required_action <- as.numeric(as.factor(imputed_df$required_action))
numeric_encoded_df$cwe <- as.numeric(as.factor(imputed_df$cwe))
numeric_encoded_df$vector <- as.numeric(as.factor(imputed_df$vector))
numeric_encoded_df$complexity <- as.numeric(as.factor(imputed_df$complexity))
numeric_encoded_df$severity <- as.numeric(as.factor(imputed_df$severity))
numeric_encoded_df$vendor_project <- as.numeric(as.factor(imputed_df$vendor_project))
numeric_encoded_df$cve_id <- as.numeric(as.factor(imputed_df$cve_id))
numeric_encoded_df$date_added <- as.numeric(as.factor(imputed_df$date_added))
numeric_encoded_df$due_date <- as.numeric(as.factor(imputed_df$due_date))
numeric_encoded_df$pub_date <- as.numeric(as.factor(imputed_df$pub_date))

# Remove rows with NA values
numeric_encoded_df <- na.omit(numeric_encoded_df)

library(ggplot2)
library(reshape2)

# Select the relevant columns for correlation analysis
columns_of_interest <- c("severity", "complexity", "vector", "cvss")

# Subset the dataframe with the selected columns
subset_df <- numeric_encoded_df[, columns_of_interest]

# Compute the correlation matrix
correlation_matrix <- cor(subset_df)

# Convert the correlation matrix to a format suitable for ggplot
melted_correlation_matrix <- melt(correlation_matrix)

# Plot the correlation matrix using ggplot2
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

# Compute the correlation matrix for the entire numeric encoded dataframe
correlation_matrix_full <- cor(numeric_encoded_df)

# Convert the correlation matrix to a format suitable for ggplot
melted_correlation_matrix_full <- melt(correlation_matrix_full)

# Plot the correlation matrix using ggplot2
ggplot(data = melted_correlation_matrix_full, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix of Numeric Encoded Dataframe")

ggplot(numeric_encoded_df, aes(x = cvss, y = severity)) +
  geom_point(shape = 1, size = 3, color = "black") + # Scatter plot with customized points
  labs(x = "CVSS Score", y = "Severity") + # Labels for x and y axes
  scale_x_continuous(breaks = seq(0, 10, 1)) + # Define breaks for x-axis
  theme_minimal() + # Use minimal theme
  ggtitle("Scatter Plot of CVSS Score vs Severity") # Add title to the plot

# Impute missing values in 'product' and 'short_description' columns
imputed_df$product[is.na(imputed_df$product)] <- "fuel cms"
imputed_df$short_description[is.na(imputed_df$short_description)] <- "na"

# Impute 'pub_date' for NA values based on 'cve_id'
imputed_df <- imputed_df %>%
  group_by(cve_id) %>%
  mutate(pub_date = ifelse(is.na(pub_date), max(pub_date, na.rm = TRUE), pub_date))

# Check if NA values in 'cvss' and 'severity' columns are in the same rows
same_na_rows_cvss_severity <- all(is.na(imputed_df$cvss) == is.na(imputed_df$severity))
same_na_rows_cvss_severity

# Check the number of NA values in each column after imputation
na_counts_after_imputation <- colSums(is.na(imputed_df))
na_counts_after_imputation

# Remove rows with any NA values and Check the number of NA values in each column after removing rows with NA values
cleaned_df <- na.omit(imputed_df)
na_counts_cleaned <- colSums(is.na(cleaned_df))
na_counts_cleaned

# Create a copy of the imputed dataframe for the new numeric one
numeric_clean_df <- imputed_df

# Encode categorical variables using label encoding
numeric_clean_df$product <- as.numeric(as.factor(imputed_df$product))
numeric_clean_df$vulnerability_name <- as.numeric(as.factor(imputed_df$vulnerability_name))
numeric_clean_df$short_description <- as.numeric(as.factor(imputed_df$short_description))
numeric_clean_df$required_action <- as.numeric(as.factor(imputed_df$required_action))
numeric_clean_df$cwe <- as.numeric(as.factor(imputed_df$cwe))
numeric_clean_df$vector <- as.numeric(as.factor(imputed_df$vector))
numeric_clean_df$complexity <- as.numeric(as.factor(imputed_df$complexity))
numeric_clean_df$severity <- as.numeric(as.factor(imputed_df$severity))
numeric_clean_df$vendor_project <- as.numeric(as.factor(imputed_df$vendor_project))
numeric_clean_df$cve_id <- as.numeric(as.factor(imputed_df$cve_id))
numeric_clean_df$date_added <- as.numeric(as.factor(imputed_df$date_added))
numeric_clean_df$due_date <- as.numeric(as.factor(imputed_df$due_date))
numeric_clean_df$pub_date <- as.numeric(as.factor(imputed_df$pub_date))

# Verify the structure of the new data frame
str(numeric_clean_df)

# Check the number of NA values in each column of the numeric encoded dataframe
na_counts_numeric_clean <- colSums(is.na(numeric_clean_df))
na_counts_numeric_clean

# Ensure there are no NA values in numeric_clean_df
numeric_clean_df <- na.omit(numeric_clean_df)

# Compute the correlation matrix for the numeric encoded dataframe
correlation_matrix_numeric_clean <- cor(numeric_clean_df, use = "complete.obs")

# Convert the correlation matrix to a format suitable for ggplot
melted_correlation_matrix_numeric_clean <- melt(correlation_matrix_numeric_clean, na.rm = TRUE)

# Plot the correlation matrix using ggplot2
ggplot(data = melted_correlation_matrix_numeric_clean, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") + # Add a white border to tiles for clarity
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + # Rotate x-axis labels for better readability
  coord_fixed() + # Ensure the plot has a fixed aspect ratio
  labs(title = "Correlation Matrix of Numeric Encoded Dataframe", x = "", y = "") # Add title and remove axis labels

# Compute the counts of each severity level
severity_counts <- as.data.frame(table(imputed_df$severity))

# Create a pie chart to visualize the distribution of vulnerability severity levels
ggplot(severity_counts, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Vulnerability Severity Levels", fill = "Severity") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Remove x-axis text
        axis.ticks = element_blank(), # Remove x-axis ticks
        panel.grid = element_blank()) # Remove grid lines

# Convert date_added column to year-month format
library(zoo)
head(imputed_df$date_added)
imputed_df$date_added <- as.Date(imputed_df$date_added, format = "%Y-%m-%d")
imputed_df$date_added <- as.yearmon(imputed_df$date_added)
vuln_counts <- as.data.frame(table(imputed_df$date_added))
colnames(vuln_counts) <- c("Date", "Count")
vuln_counts$Date <- as.Date(as.yearmon(vuln_counts$Date))
ggplot(vuln_counts, aes(x = Date, y = Count)) +
  geom_line(color = "blue", size = 1) + # Add a line plot
  geom_point(color = "red", size = 2) + # Add points on the line plot
  labs(title = "Trends in Vulnerability Counts", x = "Date", y = "Number of Vulnerabilities") + 
  theme_minimal() # Use a minimal theme

# Convert date_added column to Date format (assuming current format is "%Y-%m-%d")
imputed_df$date_added <- as.Date(imputed_df$date_added, format = "%Y-%m-%d")
imputed_df$date_added <- as.yearmon(imputed_df$date_added)
severity_counts <- imputed_df %>%
  group_by(date_added, severity) %>%
  summarize(count = n(), .groups = 'drop')
severity_counts$date_added <- as.Date(as.yearmon(severity_counts$date_added))
ggplot(severity_counts, aes(x = date_added, y = count, color = severity)) +
  geom_line(size = 1) + # Add a line plot
  geom_point(size = 2) + # Add points on the line plot
  labs(title = "Trends in Vulnerability Counts by Severity", x = "Date", y = "Number of Vulnerabilities", color = "Severity") + 
  theme_minimal() + # Use a minimal theme
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") + # Adjust x-axis labels and breaks
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Calculate the total number of vulnerabilities
total_vulnerabilities <- nrow(imputed_df)
vulnerabilities_with_due_dates <- sum(!is.na(imputed_df$due_date))
due_date_met_proportion <- (vulnerabilities_with_due_dates / total_vulnerabilities) * 100
message("Proportion of vulnerabilities with due dates met: ", round(due_date_met_proportion, 2), "%")

# Group vulnerabilities by attack vector and count the number of occurrences
vuln_by_vector <- imputed_df %>%
  group_by(vector) %>%
  summarise(vuln_count = n(), .groups = 'drop') %>%
  drop_na()

# Display the grouped data
print(vuln_by_vector)

# Create a pie chart to visualize the distribution of vulnerabilities by attack vector
vector_chart <- ggplot(vuln_by_vector, aes(x = "", y = vuln_count, fill = vector)) +
  geom_bar(stat = "identity", width = 1) + # Create a bar chart
  coord_polar("y", start = 0) + # Convert the bar chart to a pie chart
  labs(fill = "Vector", title = "Distribution of Vulnerabilities by Vector") + # Add labels and title
  theme_void() # Use a void theme for a clean look

# Display the chart
print(vector_chart)


# Calculate the counts of each complexity level
complexity_counts <- table(imputed_df$complexity)

# Convert the table to a data frame for ggplot2
complexity_counts_df <- as.data.frame(complexity_counts)
colnames(complexity_counts_df) <- c("Complexity", "Count")

# Create a bar plot to visualize the distribution of vulnerabilities by complexity level
complexity_barplot <- ggplot(complexity_counts_df, aes(x = Complexity, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") + # Create a bar chart
  labs(title = "Distribution of Vulnerabilities by Complexity Level", x = "Complexity Level", y = "Count") + # Add labels and title
  theme_minimal() # Use a minimal theme

# Display the bar plot
print(complexity_barplot)

# Identify the top 10 vendors with the highest number of vulnerabilities
top_10_vendors <- imputed_df %>%
  group_by(vendor_project) %>%
  summarize(total_vulnerabilities = n(), .groups = 'drop') %>%
  arrange(desc(total_vulnerabilities)) %>%
  head(10)

# Create a bar chart to visualize the top 10 vendors with the highest number of vulnerabilities
top_10_vendors_chart <- ggplot(top_10_vendors, aes(x = reorder(vendor_project, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) + # Create a bar chart
  geom_text(aes(label = total_vulnerabilities), vjust = -0.3) + # Add labels to the bars
  labs(title = "Top 10 Vendors with the Highest Number of Vulnerabilities",
       x = "Vendor",
       y = "Total Vulnerabilities") + # Add labels and title
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Display the bar chart
print(top_10_vendors_chart)



# Identify the top 10 products with the highest number of vulnerabilities
top_10_products <- imputed_df %>%
  group_by(product) %>%
  summarize(total_vulnerabilities = n(), .groups = 'drop') %>%
  arrange(desc(total_vulnerabilities)) %>%
  head(10)

# Create a bar chart to visualize the top 10 products with the highest number of vulnerabilities
top_10_products_chart <- ggplot(top_10_products, aes(x = reorder(product, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "lightgreen", width = 0.5) + # Create a bar chart
  geom_text(aes(label = total_vulnerabilities), vjust = -0.3) + # Add labels to the bars
  labs(title = "Top 10 Products with the Highest Number of Vulnerabilities",
       x = "Product",
       y = "Total Vulnerabilities") + # Add labels and title
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Display the bar chart
print(top_10_products_chart)

# Identify the top 10 most common CWE categories among vulnerabilities
top_10_cwe <- imputed_df %>%
  group_by(cwe) %>%
  summarize(total_vulnerabilities = n(), .groups = 'drop') %>%
  arrange(desc(total_vulnerabilities)) %>%
  head(10)

# Create a bar chart to visualize the most common CWE categories among vulnerabilities
top_10_cwe_chart <- ggplot(top_10_cwe, aes(x = reorder(cwe, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.5) + # Create a bar chart
  geom_text(aes(label = total_vulnerabilities), vjust = -0.3) + # Add labels to the bars
  labs(title = "Most Common CWE Categories among Vulnerabilities",
       x = "CWE Category",
       y = "Total Vulnerabilities") + # Add labels and title
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
print(top_10_cwe_chart)

# Calculate the patching time as the difference between due_date and pub_date
imputed_df$patching_time <- as.numeric(imputed_df$due_date - imputed_df$pub_date)
patching_time_summary <- summary(imputed_df$patching_time)
print(patching_time_summary)

# Calculate the patching time as the difference between due_date and pub_date
imputed_df$patching_time <- as.numeric(imputed_df$due_date - imputed_df$pub_date)
patching_time_hist <- ggplot(imputed_df, aes(x = patching_time)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", bins = 20) + # Create a histogram
  labs(title = "Distribution of Patching Time", x = "Patching Time (Days)", y = "Frequency") + # Add labels and title
  theme_minimal() # Use a minimal theme
print(patching_time_hist)


# Calculate the patching time as the difference between due_date and pub_date
imputed_df$patch_duration <- imputed_df$due_date - imputed_df$pub_date

# Convert the patching time to numeric
imputed_df$patch_duration <- as.numeric(imputed_df$patch_duration)

# Provide a summary of the patching time
patch_duration_summary <- summary(imputed_df$patch_duration)

# Print the summary of patching time
print(patch_duration_summary)

# Calculate the patching time as the difference between due_date and pub_date
imputed_df$patch_duration <- imputed_df$due_date - imputed_df$pub_date
imputed_df$patch_duration <- as.numeric(imputed_df$patch_duration)

# Remove non-finite values from patch_duration
imputed_df_clean <- imputed_df[is.finite(imputed_df$patch_duration), ]

# Plot the histogram
ggplot(imputed_df_clean, aes(x = patch_duration)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(x = "Patching Time (Days)", title = "Distribution of Patching Time") +
  theme_minimal()

# Subset the data to match the number of rows
subset_size <- min(nrow(imputed_df), nrow(numeric_clean_df))
imputed_df_subset <- imputed_df[1:subset_size, ]
numeric_clean_df_subset <- numeric_clean_df[1:subset_size, ]

# Combine the dataframes
aligned_df <- imputed_df_subset
aligned_df$severity <- numeric_clean_df_subset$severity

# Filter out rows with missing values
aligned_df <- aligned_df[!is.na(aligned_df$severity) & !is.na(aligned_df$patch_duration), ]

# Plot the relationship between severity and patching time
ggplot(data = aligned_df, aes(x = severity, y = patch_duration)) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Severity", y = "Patching Time (Days)", title = "Severity vs. Patching Time") +
  theme_minimal()

# Calculate the correlation between severity and patch_duration
correlation_value <- cor(numeric_clean_df_subset$severity, imputed_df_subset$patch_duration, use = "complete.obs")
print(correlation_value)



library(igraph)

# Create an edge list from vendor_project to product
edge_list <- data.frame(source = imputed_df$vendor_project, target = imputed_df$product)

# Remove missing values or empty strings
edge_list <- na.omit(edge_list)
edge_list <- edge_list[edge_list$source != "", ]
edge_list <- edge_list[edge_list$target != "", ]

# Create a graph object from the edge list
network_graph <- graph_from_data_frame(edge_list, directed = FALSE)

# Calculate degree centrality for each node in the graph
degree_centrality <- degree(network_graph)

# Get the nodes with the highest degree centrality
most_central_nodes <- names(degree_centrality)[degree_centrality == max(degree_centrality)]

# Plot the graph using igraph's plotting functions
plot(network_graph, 
     vertex.size = 10, 
     vertex.label.cex = 0.6, 
     edge.arrow.size = 0.5, 
     edge.width = 0.5)





library(igraph)

# Create the graph object from the imputed_df dataframe
network_graph <- graph_from_data_frame(imputed_df, directed = FALSE)

# Calculate the degree centrality for each node in the graph
degree_centrality <- degree(network_graph, mode = "all")

# Sort the degree centrality in decreasing order
sorted_degree_centrality <- sort(degree_centrality, decreasing = TRUE)

# Get the top 10 nodes with the highest degree centrality
top_vendors <- names(sorted_degree_centrality)[1:10]

# Print the top 10 vendors
print(top_vendors)

# Plot the graph using igraph's plotting functions
plot(network_graph, 
     vertex.size = 10, 
     vertex.label.cex = 0.6, 
     edge.arrow.size = 0.5, 
     edge.width = 0.5, 
     vertex.color = ifelse(V(network_graph)$name %in% top_vendors, "red", "lightblue"),
     vertex.label = ifelse(V(network_graph)$name %in% top_vendors, V(network_graph)$name, NA))







library(ggrepel)
library(ggraph)

# Create an edge list from vendor_project to product
network_data <- imputed_df[, c("vendor_project", "product")]

# Create edge list matrix
edge_list <- as.matrix(network_data)

# Create the graph object from the edge list
network_graph <- graph_from_edgelist(edge_list, directed = FALSE)

# Calculate degree centrality for each node in the graph
degree_centrality <- degree(network_graph)

# Add centrality attribute to the graph
V(network_graph)$centrality <- degree_centrality

# Get the top 20 most central nodes for labeling
top_central_nodes <- names(sort(degree_centrality, decreasing = TRUE))[1:20]

# Create a new attribute to label only the top central nodes
V(network_graph)$label <- ifelse(V(network_graph)$name %in% top_central_nodes, V(network_graph)$name, "")

# Create the layout for ggraph
graph_layout <- create_layout(network_graph, layout = "fr")

# Create a data frame for nodes to be labeled
label_df <- data.frame(x = graph_layout$x, 
                       y = graph_layout$y, 
                       label = V(network_graph)$label, 
                       centrality = V(network_graph)$centrality)

# Filter out rows where labels are empty
label_df <- label_df[label_df$label != "", ]

# Plot the graph using ggraph
graph_plot <- ggraph(graph_layout) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality), shape = 21, fill = "#005c87", color = "gray") +
  geom_text_repel(data = label_df, aes(x = x, y = y, label = label), box.padding = 0.5, size = 2.5, max.overlaps = 50) +
  theme_void()

# Display the graph plot
print(graph_plot)


library(igraph)

# Initialize an empty graph
network_graph2 <- graph.empty(directed = FALSE)

# Loop through each row of the data frame to add vertices and edges
for (i in 1:nrow(numeric_clean_df)) {
  vendor <- as.character(numeric_clean_df$vendor_project[i])
  product <- as.character(numeric_clean_df$product[i])
  
  # Add the vendor and product as vertices if they don't already exist
  if (!(vendor %in% V(network_graph2)$name)) {
    network_graph2 <- add_vertices(network_graph2, 1, name = vendor)
  }
  if (!(product %in% V(network_graph2)$name)) {
    network_graph2 <- add_vertices(network_graph2, 1, name = product)
  }
  
  # Add an edge between the vendor and product
  network_graph2 <- add_edges(network_graph2, c(vendor, product))
}

# Calculate degree centrality for each node in the graph
degree_centrality2 <- degree(network_graph2)

# Plot the degree centrality distribution as a histogram
hist(degree_centrality2, main = "Degree Centrality Distribution", xlab = "Degree Centrality")


library(igraph)
network_graph <- graph.empty(directed = FALSE)
unique_nodes <- unique(c(imputed_df$vendor_project, imputed_df$product))
network_graph <- add.vertices(network_graph, length(unique_nodes), name = unique_nodes)
edge_list <- cbind(imputed_df$vendor_project, imputed_df$product)
network_graph <- add.edges(network_graph, as.vector(t(edge_list)))
degree_centrality <- degree(network_graph)
top_interconnected_nodes <- names(degree_centrality)[degree_centrality == max(degree_centrality)]
top_interconnected_nodes


library(visNetwork)
nodes_df <- data.frame(id = unique(c(imputed_df$vendor_project, imputed_df$product)), 
                       label = unique(c(imputed_df$vendor_project, imputed_df$product)))
print(head(nodes_df))
edges_df <- data.frame(from = imputed_df$vendor_project, to = imputed_df$product)
print(head(edges_df))
vis_network <- visNetwork(nodes_df, edges_df)
vis_network <- visOptions(vis_network, width = "100%", height = "500px")
vis_network <- visInteraction(vis_network, hover = TRUE, hoverConnectedEdges = TRUE)
vis_network <- visPhysics(vis_network, enabled = TRUE)
# Display the network
vis_network





# Load the cluster package
library(cluster)

# Select the columns for clustering
clustering_data <- numeric_clean_df[, c("vendor_project", "product")]

# Initialize an empty vector to store the WCSS values
wcss_values <- numeric(10)

# Calculate WCSS for different values of k
for (k in 1:10) {
  kmeans_model <- kmeans(clustering_data, centers = k)
  wcss_values[k] <- kmeans_model$tot.withinss
}

# Plot the WCSS values against the number of clusters
plot(1:10, wcss_values, type = "b", xlab = "Number of Clusters (k)", ylab = "WCSS", main = "Elbow Method for Optimal k")



# Select the columns for clustering
clustering_data <- numeric_clean_df[, c("vendor_project", "product")]

# Initialize an empty list to store the average silhouette values
silhouette_avg_values <- list()

# Calculate average silhouette coefficient for different values of k
for (k in 2:10) {
  # Perform k-means clustering
  kmeans_model <- kmeans(clustering_data, centers = k)
  
  # Calculate silhouette coefficients
  silhouette_obj <- silhouette(kmeans_model$cluster, dist(clustering_data))
  
  # Store the average silhouette coefficient
  silhouette_avg_values[[k]] <- mean(silhouette_obj[, 3])
}

# Plot the average silhouette coefficients against the number of clusters
plot(2:10, unlist(silhouette_avg_values), type = "b", xlab = "Number of Clusters (k)", ylab = "Average Silhouette Coefficient", main = "Silhouette Analysis for Optimal k")








library(cluster)

# Select the columns for clustering
cluster_data <- numeric_clean_df[, c("vendor_project", "product")]

# Number of clusters
num_of_clusters <- 3

# Perform k-means clustering
kmeans_result <- kmeans(cluster_data, centers = num_of_clusters)

# Get the cluster assignments for each data point
clusters <- kmeans_result$cluster

# Get the count of data points in each cluster
cluster_distribution <- table(clusters)

# Print the cluster counts
print(cluster_distribution)

# Plot the clustered data
plot(cluster_data, col = clusters, pch = 16, main = "K-means Clustering", xlab = "Vendor Project", ylab = "Product")












# Select the columns for clustering
cluster_data <- numeric_clean_df[, c("vendor_project", "product")]

# Number of clusters
num_of_clusters <- 3

# Perform k-means clustering
kmeans_result <- kmeans(cluster_data, centers = num_of_clusters)

# Get the cluster assignments for each data point
clusters <- kmeans_result$cluster

# Get the count of data points in each cluster
cluster_distribution <- table(clusters)

# Print the cluster counts
print(cluster_distribution)

# Plot the clustered data
plot(cluster_data, col = clusters, pch = 16, main = "K-means Clustering", xlab = "Vendor Project", ylab = "Product")

# Plot the barplot for cluster counts
barplot(cluster_distribution, main = "Cluster Distribution", xlab = "Cluster", ylab = "Number of Data Points")




# Load necessary libraries
library(cluster)

# Select the columns for clustering
cluster_data <- numeric_clean_df[, c("vendor_project", "product")]

# Number of clusters
num_of_clusters <- 3

# Perform k-means clustering
kmeans_result <- kmeans(cluster_data, centers = num_of_clusters)

# Get the cluster assignments for each data point
clusters <- kmeans_result$cluster

# Get the count of data points in each cluster
cluster_distribution <- table(clusters)

# Print the cluster counts
print(cluster_distribution)

# Plot the clustered data
plot(cluster_data, col = clusters, pch = 16, main = "K-means Clustering", xlab = "Vendor Project", ylab = "Product")

# Plot the barplot for cluster counts
barplot(cluster_distribution, main = "Cluster Distribution", xlab = "Cluster", ylab = "Number of Data Points")

# Create a heatmap for vendor_project and product
heatmap_data <- table(numeric_clean_df$vendor_project, numeric_clean_df$product)
heatmap(heatmap_data, main = "Heatmap of Vendor Project vs Product", xlab = "Product", ylab = "Vendor Project")









library(dplyr)
library(ggplot2)

# Calculate the number of unique collaborations per vendor project
vendor_collaboration <- imputed_df %>%
  group_by(vendor_project) %>%
  summarise(collaborations = n_distinct(product)) %>%
  filter(collaborations > 1) %>%
  arrange(desc(collaborations))

# Plot the number of collaborations per vendor project
ggplot(vendor_collaboration, aes(x = reorder(vendor_project, collaborations), y = collaborations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Vendor Project", y = "Number of Collaborations") +
  ggtitle("Vendors with Multiple Collaborations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Compute the distance matrix for vendor_project
distance_matrix <- dist(numeric_clean_df$vendor_project)

# Perform hierarchical clustering on the distance matrix
hierarchical_clustering <- hclust(distance_matrix)

# Cut the dendrogram to obtain clusters, specifying k = 3
cluster_assignments <- cutree(hierarchical_clustering, k = 3) 

# Create a heatmap of the distance matrix with cluster assignments
heatmap(as.matrix(distance_matrix), Rowv = as.dendrogram(hierarchical_clustering), Colv = NA, 
        col = heat.colors(256), main = "Clustered Heatmap", labRow = "", labCol = cluster_assignments)








# Subset the data to include only the relevant columns
subset_data <- numeric_clean_df[, c("severity", "cwe", "vector")]

# Print the first few rows of the subset data to verify
head(subset_data)







library(dplyr)
library(ggplot2)
library(ggrepel)

# Subset the data to include only the relevant columns
subset_data <- numeric_clean_df[, c("severity", "cwe", "vector")]

# Create a plot for CWE vs. Severity with labels
cwe_severity_labels_plot <- subset_data %>%
  group_by(cwe, severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = cwe, y = severity, label = count, fill = count)) +
  geom_tile() +
  geom_text_repel() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "CWE (Common Weakness Enumeration)", y = "Severity") +
  ggtitle("Vulnerability Clusters: CWE vs. Severity (with Labels)") +
  theme_bw()

# Display the plot
cwe_severity_labels_plot



# Subset the data to include only the relevant columns
subset_data <- numeric_clean_df[, c("severity", "cwe", "vector")]

# Group the data by vector and severity and count the occurrences
vector_severity_labels_plot_data <- subset_data %>%
  group_by(vector, severity) %>%
  summarise(count = n(), .groups = 'drop')

# Print the grouped data
print(vector_severity_labels_plot_data)







# Subset the data to include only the relevant columns
subset_data <- numeric_clean_df[, c("severity", "vector")]

# Group the data by vector and severity and count the occurrences
vector_severity_labels_plot_data <- subset_data %>%
  group_by(vector, severity) %>%
  summarise(count = n(), .groups = 'drop')

# Create a plot for Vector vs. Severity with labels
vector_severity_labels_plot <- vector_severity_labels_plot_data %>%
  ggplot(aes(x = vector, y = severity, label = count, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), size = 3, vjust = 0.5, hjust = 0.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Vector", y = "Severity") +
  ggtitle("Vulnerability Clusters: Vector vs. Severity (with Labels)") +
  theme_bw()

# Display the plot
vector_severity_labels_plot

# Calculate the counts of each vulnerability name
vulnerability_counts <- table(imputed_df$vulnerability_name)

# Identify vulnerabilities that appear more than once
super_spreader_vulnerabilities <- names(vulnerability_counts[vulnerability_counts > 1])

# Filter the data for super spreader vulnerabilities
super_spreader_data <- imputed_df[imputed_df$vulnerability_name %in% super_spreader_vulnerabilities, ]

# Calculate the counts of super spreader vulnerabilities
super_spreader_counts <- table(super_spreader_data$vulnerability_name)

# Identify the top 25 super spreader vulnerabilities
top_spreaders <- head(names(super_spreader_counts), 25)

# Filter the data for the top 25 super spreader vulnerabilities
top_spreader_data <- super_spreader_data[super_spreader_data$vulnerability_name %in% top_spreaders, ]

# Print the top spreader data
print(top_spreader_data)






library(igraph)

# Create a graph from the top spreader data
network_graph <- graph_from_data_frame(top_spreader_data[, c("vendor_project", "vulnerability_name")], directed = FALSE)

# Set node colors based on whether the node is a vendor project or a vulnerability name
node_colors <- ifelse(V(network_graph)$name %in% top_spreader_data$vendor_project, "#564c53", "#baaf9b")

# Set node sizes based on whether the node is a vendor project or a vulnerability name
node_sizes <- ifelse(V(network_graph)$name %in% top_spreader_data$vulnerability_name, 6, 5)

# Create the layout for the graph
graph_layout <- layout_with_fr(network_graph)

# Plot the graph
plot(network_graph, 
     vertex.color = node_colors, 
     vertex.size = node_sizes, 
     edge.arrow.size = 1.5, 
     layout = graph_layout, 
     vertex.label.dist = 1, 
     vertex.label.cex = 0.7,
     main = "Network Graph of Top Spreader Vulnerabilities and Vendor Projects")










library(lubridate)

# Ensure the date columns are in Date format
filtered_data <- imputed_df[!is.na(imputed_df$due_date), ]

# Convert the decimal year format to Date format
convert_decimal_year <- function(decimal_year) {
  year <- floor(decimal_year)
  days <- (decimal_year - year) * 365.25
  as.Date(paste(year, "01-01", sep = "-")) + days
}

filtered_data$date_added <- convert_decimal_year(filtered_data$date_added)
filtered_data$due_date <- as.Date(filtered_data$due_date, format = "%Y-%m-%d")

# Calculate the time taken by subtracting date_added from due_date
filtered_data$time_taken <- as.numeric(filtered_data$due_date - filtered_data$date_added)

# Calculate the average time taken for each vendor_project and product
avg_time_taken <- aggregate(time_taken ~ vendor_project + product, filtered_data, FUN = mean)

# Sort the average time taken in ascending order
sorted_avg_time_taken <- avg_time_taken[order(avg_time_taken$time_taken), ]

# Display the top 5 entries with the shortest average time taken
head(sorted_avg_time_taken, 5)









# Load necessary libraries
library(lubridate)
library(ggplot2)

# Ensure the date columns are in Date format
filtered_data <- imputed_df[!is.na(imputed_df$due_date), ]

# Convert the decimal year format to Date format
convert_decimal_year <- function(decimal_year) {
  year <- floor(decimal_year)
  days <- (decimal_year - year) * 365.25
  as.Date(paste(year, "01-01", sep = "-")) + days
}

filtered_data$date_added <- convert_decimal_year(filtered_data$date_added)
filtered_data$due_date <- as.Date(filtered_data$due_date, format = "%Y-%m-%d")

# Calculate the time taken by subtracting date_added from due_date
filtered_data$time_taken <- as.numeric(filtered_data$due_date - filtered_data$date_added)

# Calculate the average time taken for each vendor_project and product
avg_time_taken <- aggregate(time_taken ~ vendor_project + product, filtered_data, FUN = mean)

# Sort the average time taken in ascending order
sorted_avg_time_taken <- avg_time_taken[order(avg_time_taken$time_taken), ]

# Select the top 25 and bottom 10 entries based on average time taken
top_25 <- head(sorted_avg_time_taken, 25)
bottom_10 <- tail(sorted_avg_time_taken, 10)

# Combine the top 25 and bottom 10 data
combined_data <- rbind(top_25, bottom_10)

# Create a lollipop chart
ggplot(combined_data, aes(x = reorder(product, time_taken), y = time_taken)) +
  geom_segment(aes(x = product, xend = product, y = 0, yend = time_taken), color = "#093a53") +
  geom_point(color = "#093a53", size = 2) +
  coord_flip() +
  labs(x = "Product", y = "Average Time Taken (days)") +
  ggtitle("Top 25 and Bottom 10 Vendors/Products by Average Time Taken to Address Vulnerabilities") +
  theme_minimal()









library(e1071)  # Load the Naive Bayes classifier package

set.seed(42)  # Set seed for reproducibility

# Split the data into training (80%) and testing (20%) sets
train_indices <- sample(1:nrow(imputed_df), nrow(imputed_df) * 0.8)
train_data <- numeric_clean_df[train_indices, ]
test_data <- numeric_clean_df[-train_indices, ]

# Build the Naive Bayes model
model <- naiveBayes(severity ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, test_data)

# Calculate the accuracy of the model
accuracy <- sum(predictions == test_data$severity) / nrow(test_data)

# Print the accuracy
accuracy





# Load necessary libraries
library(bnlearn)

# Check if BiocManager is installed and install it if necessary
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# Install Rgraphviz using BiocManager
BiocManager::install("Rgraphviz")

# Load Rgraphviz
library(Rgraphviz)

# Set seed for reproducibility
set.seed(42)

# Split the data into training (80%) and testing (20%) sets
train_indices <- sample(1:nrow(imputed_df), nrow(imputed_df) * 0.8)
train_data <- numeric_clean_df[train_indices, ]
test_data <- numeric_clean_df[-train_indices, ]

# Learn the structure of the Bayesian network from the training data
bn_structure <- hc(train_data)

# Fit the parameters of the Bayesian network
bn_fit <- bn.fit(bn_structure, train_data)

# Predict the severity using the fitted Bayesian network on the test data
predictions <- predict(bn_fit, node = "severity", data = test_data)

# Calculate the accuracy of the model
accuracy <- sum(predictions == test_data$severity) / nrow(test_data)

# Print the accuracy
print(accuracy)

# Plot the learned Bayesian network
graphviz.plot(bn_structure)

# Install the necessary packages if not already installed
if (!requireNamespace("bnlearn", quietly = TRUE)) install.packages("bnlearn")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("Rgraphviz", quietly = TRUE)) BiocManager::install("Rgraphviz")

# Load the necessary libraries
library(bnlearn)
library(Rgraphviz)

# Set seed for reproducibility
set.seed(42)

# Learn the structure of the Bayesian network using the dataset
bn_structure <- hc(numeric_clean_df)

# Convert the Bayesian network structure to a graphNEL object for Rgraphviz
graphNEL_bn <- bnlearn::as.graphNEL(bn_structure)

# Layout the graph for visualization
graph_layout <- Rgraphviz::layoutGraph(graphNEL_bn)

# Set the node render info to adjust the font size
graph::nodeRenderInfo(graph_layout) <- list(fontsize = 100)

# Render the graph
Rgraphviz::renderGraph(graph_layout)