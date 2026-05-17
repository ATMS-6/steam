# ============================================
# TASK 4: Merchandise Sales - Histogram, Boxplot, Stem-and-Leaf
# ============================================

# READ DATASET
# ⚠️ Change filename to match what is given in exam
merch <- read.csv("MerchandiseSales.csv")

# ⚠️ Change "Sales" to the numeric column name from your dataset
# Run str(merch) first to see available column names
col <- merch$Sales

# --- Histogram ---
hist(col,
     main  = "Histogram of Sales",
     xlab  = "Sales",
     col   = "steelblue",
     border= "white")

# --- Box-and-Whisker Plot ---
boxplot(col,
        main = "Boxplot of Sales",
        ylab = "Sales",
        col  = "lightgreen")

# --- Stem-and-Leaf Display ---
stem(col)
