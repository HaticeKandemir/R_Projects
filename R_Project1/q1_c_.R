dev.new()
par(mfrow = c(1, 2))  # Divide the output screen for multiple histograms
custom_barplot(data, "Group", xlab = "Group", ylab = "Count", main = "Group Counts", xlim = c(0, 6), ylim = c(0, 50), fill_color = "navy")
custom_barplot(data, "Gender", xlab = "Gender", ylab = "Count", main = "Gender Counts", xlim = c(0, 6), ylim = c(0, 80), fill_color = "maroon")
par(mfrow = c(1, 1))  # Reset the plotting layout