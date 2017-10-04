basque$Treatment <- 0
for (i in unique(basque$regionno)) {
    current_group_indices <- which(basque$regionno == i)
    treatment_size <- round(length(current_group_indices) / 2)
    group_sample <- sample(current_group_indices, size=treatment_size, replace=FALSE)
    basque[group_sample,'Treatment'] <- 1
}

all_data$Treatment <- 0

create_treatment_groups <- function(seed) {
    set.seed(seed);
    all_data$Treatment <- 0
    for (manager in unique(all_data$Area.Manager)) {
        print(manager)
        current_group_indices <- which(all_data$Area.Manager == manager)
        print(current_group_indices)
        treatment_size <- round(length(current_group_indices) / 2)
        group_sample <- sample(current_group_indices, size=treatment_size, replace=FALSE)
        all_data[group_sample,'Treatment'] <- 1
    }    
    
    set.seed(seed);
    # Do GenMatch and MatchBalance stuff here
}

create_treatment_groups(5)


# https://stackoverflow.com/questions/34983118/how-do-i-create-a-copy-of-a-data-frame-in-r



for (i in all_data$Area.Manager){
    group <- all_data[all_data$Area.Manager == i,]
    s1 <- sample(nrow(group),size = (nrow(group)/2),replace=FALSE)
    ss <- group[s1,]
    con1 <- group[-s1,]
    con1 <- cbind(con1,rep(0, dim(con1)[1]))
    names(con1)[ncol(con1)] <- c("Treatment")          
    ss$Treatment <- 1
    area1 <- rbind(ss,con1)
    final_sample1[n,] <- area1
    n <- n + nrow(area1)
}