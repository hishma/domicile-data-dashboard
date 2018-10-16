f <- factor(c("b", "b", "a", "c", "c", "c"))
f
fct_inorder(f)
fct_infreq(f)
fct_inorder(f, ordered = TRUE)


ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  geom_point(alpha = 0.5, size = 0.5, shape = 16) +
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1, 10)) +
  scale_y_log10(expression(log[10](Price)), limits = c(100, 100000)) +
  scale_color_brewer(palette = "YlOrRd") +
  coord_equal() +
  theme_classic()


#plot using hist and density
fun_args <- list(mean = mean(test_data$norm), sd = sd(test_data$norm))

# Finish the ggplot
ggplot(test_data, aes(x = norm)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red") +
  stat_function(fun = dnorm, args = fun_args)
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

# Unweighted violin plot
ggplot(mammals, aes(x = vore, y = sleep_total, fill = vore)) +
  geom_violin()

# Calculate weighting measure
library(dplyr)
mammals2 <- mammals %>%
  group_by(vore) %>%
  mutate(n = n() / nrow(mammals)) -> mammals

# Weighted density plot
ggplot(mammals, aes(x = sleep_total, fill = vore)) +
  geom_density(aes(weight = n), col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))

library(ggplot2)
library(reshape2)

cor_list <- function(x) {
  L <- M <- cor(x)
  
  M[lower.tri(M, diag = TRUE)] <- NA
  M <- melt(M)
  names(M)[3] <- "points"
  
  L[upper.tri(L, diag = TRUE)] <- NA
  L <- melt(L)
  names(L)[3] <- "labels"
  
  merge(M, L)
}

# Calculate xx with cor_list
library(dplyr)
xx <- iris %>%
  group_by(Species) %>%
  do(cor_list(.[1:4])) 

# Finish the plot
ggplot(xx, aes(x = Var1, y = Var2)) +
  geom_point(aes(col = points, size = abs(points)), shape = 16) +
  geom_text(aes(col = labels,  size = abs(labels), label = round(labels, 2))) +
  scale_size(range = c(0, 6)) +
  scale_color_gradient("r", limits = c(-1, 1)) +
  scale_y_discrete("", limits = rev(levels(xx$Var1))) +
  scale_x_discrete("") +
  guides(size = FALSE) +
  geom_abline(slope = -1, intercept = nlevels(xx$Var1) + 1) +
  coord_fixed() +
  facet_grid(. ~ Species) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank())

# pop and all required packages are available

# Use map_data() to create state
state <- map_data("state")

# Map of states
ggplot(state, aes(x = long, y = lat, fill = region, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_nothing()

# Merge state and pop: state2
state2 <- merge(state, pop)

# Map of states with populations
ggplot(state2, aes(x = long, y = lat, fill = Pop_est, group = group)) +
  geom_polygon(col = "white") +
  coord_map() +
  theme_map()

# Import shape information: germany
germany <- readOGR(dsn = "shapes", layer = "DEU_adm1")

# fortify germany: bundes
bundes <- fortify(germany)

# Plot map of germany
ggplot(bundes, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "blue", col = "white") +
  coord_map() +
  theme_nothing()
# germany, bundes and unemp are available

# re-add state names to bundes
bundes$state <- factor(as.numeric(bundes$id))
levels(bundes$state) <- germany$NAME_1

# Merge bundes and unemp: bundes_unemp
bundes_unemp <- merge(bundes, unemp, by = "state")

# Update the ggplot call
ggplot(bundes_unemp, aes(x = long, y = lat, group = group, fill = unemployment)) +
  geom_polygon() +
  coord_map() +
  theme_map()

# Add a location column to xx
xx$location <- sub(", London", "", london_sites)

# Add a geom_points layer
ggmap(london_ton_13) +
  geom_point(data = xx, aes(col = location), size = 6)

str(japan)

# Finish the code inside saveGIF
saveGIF({
  
  # Loop through all time points
  for (i in unique(japan$time)) {
    
    # Subset japan: data
    data <- subset(japan, time == i)
    
    # Finish the ggplot command
    p <- ggplot(data, aes(x = AGE, y = POP, fill = SEX, width = 1)) +
      coord_flip() +
      geom_bar(data = data[data$SEX == "Female",], stat = "identity") +
      geom_bar(data = data[data$SEX == "Male",], stat = "identity") +
      ggtitle(i)
    
    print(p)
    
  }
  
}, movie.name = "pyramid.gif", interval =  0.1)

#rea dataset and replace column names.specify widths for columns.
weather <- read.fwf("NYNEWYOR.txt",
                    header = FALSE,
                    col.names = c("month", "day", "year", "temp"),
                    widths = c(14, 14, 13, 4))
#mutating dataset and ungrouping.  Case study GgPLOT3
#reate new version of past
past_summ <- past %>%
  group_by(year) %>%
  mutate(yearday = 1:length(day)) %>%
  ungroup %>%
  filter(temp != -99) %>%
  group_by(yearday) %>%
  mutate(max = max(temp),
         min = min(temp),
         avg = mean(temp),
         CI_lower = Hmisc::smean.cl.normal(temp)[2],
         CI_upper = Hmisc::smean.cl.normal(temp)[3]) %>%
  ungroup()

# Step 6: Efficiently calculate record highs and lows
## To also add the record lows to the plot, you could do the same things as in the previous e
## xercise: create a data frame past_lows, join it with present, 
## figure out the record lows, and add yet another layer, with a blueish color.
# This is not really the ggplot2 way to do things. Instead of adding two layers, 
# and manually assigning a color, you can do something else: you can map a variable 
# denoting a record high or low onto the color aesthetic!
#   Here you'll combine the previous two exercises to identify the record highs and lows in one step, 
#   assign them to a new data frame called extremes, and use this to map a color aesthetic. This will 
#   make both your data munging and plotting code more efficient.
#   
# Create past_extremes
past_extremes <- past %>%
  group_by(yearday) %>%
  summarise(past_low = min(temp),
            past_high = max(temp))

# Create record_high_low
record_high_low <- present %>%
  left_join(past_extremes) %>%
  mutate(record = ifelse(temp < past_low, 
                         "#0000CD",
                         ifelse(temp > past_high, 
                                "#CD2626", 
                                "#00000000"))) #transparent

# Structure of record_high_low
str(record_high_low)

# Add point layer of record_high_low
ggplot(past, aes(x = yearday, y = temp)) + 
  geom_point(col = "#EED8AE", alpha = 0.3, shape = 16) +
  geom_linerange(aes(ymin = CI_lower, ymax = CI_upper), col = "#8B7E66") +
  geom_line(data = present) +
  geom_point(data = record_high_low, aes(col = record)) +
  scale_color_identity()

# Step 7: Custom legend
## Although you have a lot of information on your plot, the only aesthetic that you used was color, so the legend won't reflect all 
## geoms and color attributes that you've used.   This means you'll have to create your own legend. You'll do this with the grid package plotting 
## functions that you can call after generating the ggplot itself. # We've set up a new function, draw_pop_legend(), that takes 5 arguments. 
## Your task it to complete the rest. The function will push a viewport using pushViewport(viewport()). Code to position the points, rectangle and black line has been provided for you. 
# ou should be able to understand what's happening here from the previous chapter on grid graphics. Feel free to play around with the arguments, 
# but the defaults should work fine.
# Great! Note: There are a couple other ways you could have done this. You could have used ggplot2::annotate(), which is rather 
# tedious and not as reproducible. You could have also used a layout when making the viewport, filling each segment in the viewport layout 
# with specific elements. You could have also named each viewport so as to access the graphics objects, grobs, later on. Here, you're also 
# neglecting checks and error messages which would ensure your function always works properly - so it's not quite ready for release into the wild. 
# For our purposes, this working version will suffice, but in a package you'd probably want something more robust.
# Finish the function draw_pop_legend
draw_pop_legend <- function(x = 0.6, y = 0.2, width = 0.2, height = 0.2, fontsize = 10) {
  
  # Finish viewport() function
  pushViewport(viewport(x = x, y = y, width = width, height = height, just = "center"))
  
  legend_labels <- c("Past record high",
                     "95% CI range",
                     "Current year",
                     "Past years",
                     "Past record low")
  
  legend_position <- c(0.9, 0.7, 0.5, 0.2, 0.1)
  
  # Finish grid.text() function
  grid.text(label = legend_labels, x = 0.12, y = legend_position, 
            just = "left", 
            gp = gpar(fontsize = fontsize, col = "grey20"))
  
  # Position dots, rectangle and line
  point_position_y <- c(0.1, 0.2, 0.9)
  point_position_x <- rep(0.06, length(point_position_y))
  grid.points(x = point_position_x, y = point_position_y, pch = 16,
              gp = gpar(col = c("#0000CD", "#EED8AE", "#CD2626")))
  grid.rect(x = 0.06, y = 0.5, width = 0.06, height = 0.4,
            gp = gpar(col = NA, fill = "#8B7E66"))
  grid.lines(x = c(0.03, 0.09), y = c(0.5, 0.5),
             gp = gpar(col = "black", lwd = 3))
  
  # Add popViewport() for bookkeeping
  popViewport()
}
# Plotting object p, from previous exercise
p
# Call draw_pop_legend()
draw_pop_legend(p)
#function to read and clean data
clean_weather <- function(file) {
  weather <- read.fwf(file,
                      header = FALSE,
                      col.names = c("month", "day", "year", "temp"),
                      widths = c(14, 14, 13, 4))
  weather %>%
    filter(!(month == 2 & day == 29)) %>%
    group_by(year) %>%
    mutate(yearday = 1:length(day)) %>%
    ungroup() %>%
    filter(temp != -99)
}

# Import NYNEWYOR.txt: my_data
my_data <- clean_weather("NYNEWYOR.txt")
#build own geom object, stat_historical()
StatHistorical <- ggproto("StatHistorical", Stat,
                          compute_group = function(data, scales, params) {
                            data <- data %>%
                              filter(year != max(year)) %>%
                              group_by(x) %>%
                              mutate(ymin = Hmisc::smean.cl.normal(y)[3],
                                     ymax = Hmisc::smean.cl.normal(y)[2]) %>%
                              ungroup()
                          },
                          required_aes = c("x", "y", "year"))

# Create the layer
stat_historical <- function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  list(
    layer(
      stat = "identity", data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, col = "#EED8AE", alpha = 0.3, shape = 16, ...)
    ),
    layer(
      stat = StatHistorical, data = data, mapping = mapping, geom = "linerange",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, col = "#8B7E66", ...)
    )
  )
}

# Build the plot
my_data <- clean_weather("NYNEWYOR.txt")
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present()

# Create the stats object
StatPresent <- ggproto("StatPresent", Stat,
                       compute_group = function(data, scales, params) {
                         data <- filter(data, year == max(year))
                       },
                       required_aes = c("x", "y", "year"))

# Create the layer
stat_present <- function(mapping = NULL, data = NULL, geom = "line",
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ...) {
  layer(
    stat = StatPresent, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
# The last step is to create a stat_extreme() layer. This takes advantage of the calculations you did earlier, which combined the definitions 
# of new highs and lows into one variable, record. This variable contains the colors of the dots: deep blue for record lows, dark red for record 
# highs, and transparent dots otherwise. Because this record variable is 'internally computed', you'll have to refer to it as ..record.. when 
# you're creating the plot.
# Create the stats object
StatExtremes <- ggproto("StatExtremes", Stat,
                        compute_group = function(data, scales, params) {
                          
                          present <- data %>%
                            filter(year == max(year)) 
                          
                          past <- data %>%
                            filter(year != max(year)) 
                          
                          past_extremes <- past %>%
                            group_by(x) %>%
                            summarise(past_low = min(y),
                                      past_high = max(y))
                          
                          # transform data to contain extremes
                          data <- present %>%
                            left_join(past_extremes) %>%
                            mutate(record = ifelse(y < past_low, 
                                                   "#0000CD", 
                                                   ifelse(y > past_high, 
                                                          "#CD2626", 
                                                          "#00000000")))
                        },
                        required_aes = c("x", "y", "year"))

# Create the layer
stat_extremes <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatExtremes, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Build the plot
my_data <- clean_weather("NYNEWYOR.txt")
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present() +
  stat_extremes(aes(col = ..record..)) +
  scale_color_identity() # Colour specification

#list of data files for each city.  Turn into one big DF
my_files <- c("NYNEWYOR.txt","FRPARIS.txt", "ILREYKJV.txt", "UKLONDON.txt")

# Build my_data with a for loop. create a new DF called my_data that is NULL to land the data.  For each file in the list, run the 
# cleaning script on the file, create a new column called ID.
my_data <- NULL
for (file in my_files) {
  temp <- clean_weather(file)
  temp$id <- sub(".txt", "", file)
  my_data <- rbind(my_data, temp)
}
ggplot(my_data, aes(x = yearday, y = temp, year = year)) +
  stat_historical() +
  stat_present() +
  stat_extremes(aes(col = ..record..)) +
  scale_color_identity() +  # specify color here
  facet_wrap(~id, ncol = 2)