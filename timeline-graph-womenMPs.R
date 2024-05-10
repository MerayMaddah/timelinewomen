# Install all the needed packages
install.packages(c("dplyr",
                   "ggplot2",
                   "viridis",
                   "timelineS"))


# Load the data set of the first woman in parliament (source: IPU's Parline).
first <- read.csv("/Users/user1/Downloads/ipu-first-woman.csv",
                  fileEncoding = "UTF-8")
head(first)

# Convert 'First.woman.in.parliament' to Date format
first$First.woman.in.parliament <- as.character(first$First.woman.in.parliament)
first$First.woman.in.parliament <- as.Date(first$First.woman.in.parliament, "%Y")
class(first$First.woman.in.parliament)


# Count occurrences of first woman MPs for each year in each country
library(dplyr)
timeline <- first %>%
  group_by(country, First.woman.in.parliament) %>%
  summarize(count = n()) %>%
  ungroup()



# Plot the timeline of the first women in parliament for continental European countries
# This creates a scatter plot of the countries' abbreviations and the year that a women MP entered parliament for the first time
library(ggplot2)
ggplot(timeline, aes(x = First.woman.in.parliament, y = country, color = country)) +
  geom_point(aes(size = count), size = 4.5) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Timeline of First Woman MP (in European Countries)",
       x = "Year",
       y = "Country",
       color = "Country") +
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))



# Visualizing the timeline in a different way::::::

# Create a summary data frame with the earliest year for each country
timeline_summary <- first %>%
  group_by(country_name) %>%
  summarize(First_year = min(First.woman.in.parliament))

head(timeline_summary)

# Generate color palette
library(viridis)
palette <- viridis(48)

# Create the new plot with the custom palette
ggplot(timeline_summary, aes(x = First_year, y = country_name)) +
  geom_segment(aes(xend = First_year, yend = country_name), color = "darkblue", size = 2) +
  geom_point(aes(color = country_name), size = 3) +  # Map color to Country variable
  scale_color_manual(values = palette) +  # Use custom color palette
  geom_text(aes(label = country_name), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +
  labs(title = "Timeline of First Woman MPs in European Countries",
       x = "Year",
       y = "",
       caption = "IPU") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


#github's Lee repository
first <- read.csv("/Users/user1/Downloads/ipu-first-woman.csv",
                  fileEncoding = "UTF-8")

head(first)
library(timelineS)
subset_first <- first[, c("country", "First.woman.in.parliament", "country_name")]
head(subset_first)

# Convert 'First.woman.in.parliament' to Date format
subset_first$First.woman.in.parliament <-
  as.Date(subset_first$First.woman.in.parliament, "%Y.%m.%d")

# Convert 'Country' to factor
subset_first$country <- as.factor(subset_first$country)
class(subset_first)

# Create the timeline using the designated package
timelineS(subset_first, main = "When did Women Enter Parliament for the First Time?", buffer.days = 600, 
          label.direction = "up", label.cex = 0.4, label.angle = 35,
          scale.tickwidth = 1.5, label.position = 3, label.color = "skyblue",
          line.color = "darkblue", point.cex = 0.5, point.color = "black",
          label.length = seq(0.1, 1.7, length.out = nrow(subset_first)))



legend_text <- c("AL-Albania",
                 "AD-Andorra",
                 "AM-Armenia",
                 "AT-Austria",
                 "AZ-Azerbaijan", 
                 "BY-Belarus",
                 "BE-Belgium",
                 "BA-Bosnia and Herzegovina",
                 "BG-Bulgaria", 
                 "HR-Croatia",
                 "CY-Cyprus",
                 "CZ-Czechia",
                 "DK-Denmark",
                 "EE-Estonia", 
                 "FI-Finland",
                 "FR-France",
                 "GE-Georgia",
                 "DE-Germany",
                 "GR-Greece", 
                 "HU-Hungary",
                 "IS-Iceland",
                 "IE-Ireland",
                 "IT-Italy",
                 "LV-Latvia", 
                 "LI-Liechtenstein",
                 "LT-Lithuania",
                 "LU-Luxembourg",
                 "MT-Malta", 
                 "MC-Monaco",
                 "ME-Montenegro",
                 "NL-Netherlands",
                 "MK-North Macedonia",
                 "NO-Norway",
                 "PL-Poland",
                 "PT-Portugal",
                 "MD-Republic of Moldova", 
                 "RO-Romania",
                 "RU-Russian Federation",
                 "SM-San Marino",
                 "RS-Serbia", 
                 "SK-Slovakia",
                 "SI-Slovenia",
                 "ES-Spain",
                 "SE-Sweden",
                 "CH-Switzerland", 
                 "TR-Türkiye",
                 "UA-Ukraine",
                 "GB-United Kingdom")

# Add the caption of the country abbreviations to the graph
caption_text <- paste("Legend: ", paste(legend_text, collapse = ", "), sep = "")
mtext(caption_text, side = 1, line = -2, adj = 0, cex = 0.25)
