grade <- read.table(file = "data/grades.txt", header = TRUE)
head(grade)

grade$avg <- (grade$e1 + grade$e2)/2
head(grade)

boxplot(avg ~ mf, data = grade)

# grade_perm <- perm_test(x = grade$avg, nA = sum(grade$mf == "F"))
# grade_perm[[1]]

# boxplot(x ~ gr, data = grade_perm[[5]])

out_perm <- perm_test(x = grade$avg, g = grade$mf, label = "F", fun = mean)
hist(out_perm$t_perm)
out_perm$p_value


