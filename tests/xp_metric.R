
# ===========================================================
# 2. Create experience variables
# ===========================================================

library(data.table)

data <- fread("./data/merged-data.csv")





# Vector for cumsum calculation must start at 0 (first match)
# -------------------------------------------------------------
for.cumsum <- data[, .SD[which.min(timestamp)], by = mirrors_id]
for.cumsum[, for_cumsum := 0]
data <- merge(for.cumsum[,c(2:3,59)], data, all.y=T, by =c("timestamp", "match_id"))
for (col in "for_cumsum") data[is.na(get(col)), (col) := 1] # replace all NAs with 1


# solution 2 (better as I dont copy and merge)
# ------------------------------------------------------------
# Order timestamps
setorder(data, mirrors_id, timestamp)

data[, first_login := ave(timestamp, mirrors_id, FUN = min)]
data[, for_cumsum := timestamp %in% first_login]
data[, for_cumsum := ifelse(data$for_cumsum == TRUE, 0, 1)]

View(data[,.(mirrors_id, timestamp, first_login, for_cumsum)])
View(data[,.(mirrors_id, timestamp, first_login, for_cumsum, xp_total)])







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

# Prior experience in other maps in the focal role, variables no.3 and no.4 for Julien
# data1[, priorxp.other.maps := xp.focal.role - xp.focal.rolemap]
# data1[, priorxp.other.realms := xp.focal.role - xp.focal.rolerealm]

# -----------------------------------------------