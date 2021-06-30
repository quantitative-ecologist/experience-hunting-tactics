
# ===========================================================
# 2. Create experience variables
# ===========================================================

library(data.table)

data <- fread("./data/merged-data.csv")



# solution 2 (better as I dont copy and merge)
# ------------------------------------------------------------
# Order timestamps
setorder(data, mirrors_id, timestamp)

data[, first_login := ave(timestamp, mirrors_id, FUN = min)]
data[, for_cumsum := timestamp %in% first_login]
data[, for_cumsum := ifelse(data$for_cumsum == TRUE, 0, 1)]




# -----------------------------------------------
# Total cumulative sum of matches
# -----------------------------------------------

data[, xp_total := cumsum(for_cumsum), by = mirrors_id]

# -----------------------------------------------



# -----------------------------------------------
# Cumulative sum of matches by role and by map 
# -----------------------------------------------

# Total experience in the focal role
data1[, xp.focal.role := cumsum(for_cumsum), by = .(mirrors_id, role)]

# Experience in the focal map or realm
data1[, xp.focal.map := cumsum(for_cumsum), by = .(mirrors_id, map_name)]

# -----------------------------------------------



# -----------------------------------------------
# Prior experience in the other role
# -----------------------------------------------

# data1[, priorxp.other.role := xp.total - xp.focal.role]


# -----------------------------------------------