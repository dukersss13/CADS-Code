#Problem 1)

#setting up datasets
m1 = c(79,66,57,91,42,59)
m2 = c(71,43,58,78,20,56)
norm_alpha = 0.05

#testing m1 and m2 for normality:
shap_m1 = shapiro.test(m1) 
p_val_shap_m1 = shap_m1$p.value 
if (p_val_shap_m1 <= norm_alpha){paste("Reject the null - Normality Is Not Met")}
if (p_val_shap_m1 > norm_alpha){paste("Cannot reject the null - We Can Assume Normality")}

shap_m2 = shapiro.test(m2) 
p_val_shap_m2 = shap_m2$p.value 
if (p_val_shap_m2 <= norm_alpha){paste("Reject the null - Normality Is Not Met")}
if (p_val_shap_m2 > norm_alpha){paste("Cannot reject the null - We Can Assume Normality")}

#setting given alpha value 
alpha <- 0.01

#a - independent samples, use Wilcoxon Mann-Whitney Test
#Ho: u1 = u2
#Ha: u1 < u2

ind_result <- t.test(m1,m2,conf.int=TRUE,conf.level=0.99)
p_val_ind <- ind_result$p.value

if (p_val_ind <= alpha){paste("Reject the null")}
if (p_val_ind > alpha){paste("Cannot reject the null")}

#b - same samples, use Wilcoxon Paired Design Test
#Ho: u1 = u2
#Ha: u1 < u2

same_result <- t.test(m1,m2,conf.int=TRUE,conf.level=0.99,paired = TRUE) #alternative = "two.sided"
p_val_same <- same_result$p.value

if (p_val_same <= alpha){paste("Reject the null")}
if (p_val_same > alpha){paste("Cannot reject the null")}

#c - testing avg score w CI for method 1
#setting up CI
sd <- sd(m1)
margin_error <- abs(qnorm(0.9))*(sd/sqrt(6))
LB <- mean(m1)-margin_error #lower bound
UB <- mean(m1)+margin_error #upper bound
paste0("The 90% CI is: (",LB,", ",UB,")")

u1 <- mean(m1) #true m1 mean
u2 <- 80 #given expected mean

#Ho: u1 = u2
#Ha: u1 < u2

#d - comparing a and b.  


#Problem 2)
#install.packages("foreign")
library(foreign)

#use wcgs.dta file
wcgs <- read.dta("data/wcgs.dta")
g0 <- wcgs[1,]
g1 <- wcgs[2,]
g2 <- wcgs[3,]
g3 <- wcgs[4,]

#a - testing groups for normality:

shap_g0 <- shapiro.test(g0) 
p_val_shap_g0 <- shap_g0$p.value 
if (p_val_shap_g0 <= norm_alpha){paste("G0) Reject the null - Normality Is Not Met")}
if (p_val_shap_g0 > norm_alpha){paste("G0) Cannot reject the null - We Can Assume Normality")}

shap_g1 <- shapiro.test(g1) 
p_val_shap_g1 <- shap_g1$p.value 
if (p_val_shap_g1 <= norm_alpha){paste("G1) Reject the null - Normality Is Not Met")}
if (p_val_shap_g1 > norm_alpha){paste("G1) Cannot reject the null - We Can Assume Normality")}

shap_g2 <- shapiro.test(g2) 
p_val_shap_g2 <- shap_g2$p.value 
if (p_val_shap_g2 <= norm_alpha){paste("G2) Reject the null - Normality Is Not Met")}
if (p_val_shap_g2 > norm_alpha){paste("G2) Cannot reject the null - We Can Assume Normality")}

shap_g3 <- shapiro.test(g3) 
p_val_shap_g3 <- shap_g3$p.value 
if (p_val_shap_g3 <= norm_alpha){paste("G3) Reject the null - Normality Is Not Met")}
if (p_val_shap_g3 > norm_alpha){paste("G3) Cannot reject the null - We Can Assume Normality")}

#b - testing groups for homogeneity of variances:

bart_g0 <- bartlett.test(g0) 
p_val_bart_g0 <- bart_g0$p.value 
if (p_val_bart_g0 <= norm_alpha){paste("G0) Reject the null - Homogeneity Is Not Met")}
if (p_val_bart_g0 > norm_alpha){paste("G0) Cannot reject the null - We Can Assume Homogeneity")}

bart_g1 <- bartlett.test(g1) 
p_val_bart_g1 <- bart_g1$p.value 
if (p_val_bart_g1 <= norm_alpha){paste("G1) Reject the null - Homogeneity Is Not Met")}
if (p_val_bart_g1 > norm_alpha){paste("G1) Cannot reject the null - We Can Assume Homogeneity")}

shap_g2 <- bartlett.test(g2) 
p_val_bart_g2 <- bart_g2$p.value 
if (p_val_bart_g2 <= norm_alpha){paste("G2) Reject the null - Homogeneity Is Not Met")}
if (p_val_bart_g2 > norm_alpha){paste("G2) Cannot reject the null - We Can Assume Homogeneity")}

bart_g3 <- bartlett.test(g3) 
p_val_bart_g3 <- bart_g3$p.value 
if (p_val_bart_g3 <= norm_alpha){paste("G3) Reject the null - Homogeneity Is Not Met")}
if (p_val_bart_g3 > norm_alpha){paste("G3) Cannot reject the null - We Can Assume Homogeneity")}

#c - testing for equality using ANOVA:

anov <- aov(g0,g1,g2,g3)
p_val_anov <- anov$p.value 
if (p_val_anov <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_anov > norm_alpha){paste("Cannot reject the null - We Can Assume Equality of Means")}

#if rejects null, do bonferroni adj pairwise t-test)
bonf_pw_tt <- pairwise.t.test(g0,g1,g2,g3,p.adj = "bonf")
p_val_bonf_tt <- bonf_pw_tt$p.value
if (p_val_bonf_tt <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_bonf_tt > norm_alpha){paste("Cannot reject the null - Equality of Means Is Not Met")}

#d - if ANOVA fails, test for equality of medians using Kruskal-Wallis:

kw <- kruskal.test(g0,g1,g2,g3)
p_val_kw <- kw$p.value 
if (p_val_kw <= norm_alpha){paste("Reject the null - Equality of Medians Is Not Met")}
if (p_val_kw > norm_alpha){paste("Cannot reject the null - We Can Assume Equality of Medians")}

#if rejects null, do bonferroni adj wilcoxon signed rank test)
bonf_w <- wilcox.test(g0,g1,g2,g3,p.adj = "bonf",paired=TRUE)
p_val_bonf_w <- bonf_w$p.value
if (p_val_bonf_w <= norm_alpha){paste("Reject the null - Equality of Medians Is Not Met")}
if (p_val_bonf_w > norm_alpha){paste("Cannot reject the null - We can Assume Equality of Medians")}


#Problem 3)

#use diets.txt file
diets <- read.delim("data/diets.txt")
d0 <- wcgs[1,]
d1 <- wcgs[2,]
d2 <- wcgs[3,]
d3 <- wcgs[4,]

#a - hypotheses
#Ho: u1 = u2
#Ha: u1 < u2

#b- ANOVA prelim tests
paste("idk")

#c - ANOVA using only mean() and tapply() functions in R
paste("idk but use tapply() and mean() somewhere in it and call it my_aov with p-value of p_val_my_aov")

#d - bonferroni adj pairwise t-test)
bonf_pw_tt_diet <- pairwise.t.test(d0,d1,d2,d3,p.adj = "bonf")
p_val_bonf_tt_diet <- bonf_pw_tt_diet$p.value
if (p_val_bonf_tt_diet <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_bonf_tt_diet > norm_alpha){paste("Cannot reject the null - Equality of Means Is Not Met")}

#e - tukey's HSD test)
tuk_diet <- TukeyHSD(d0,d1,d2,d3)
p_val_tuk_diet <- tuk_diet$p.value
if (p_val_tuk_diet <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_tuk_diet > norm_alpha){paste("Cannot reject the null - Equality of Means Is Not Met")}

#f - aov test and compare with c)
anov_diet <- aov(d0,d1,d2,d3)
p_val_anov_diet <- anov_diet$p.value 
if (p_val_anov_diet <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_anov_diet > norm_alpha){paste("Cannot reject the null - We Can Assume Equality of Means")}
paste0("Comparing my ANOVA test to aov test, the p-value for my ANOVA was ",p_val_my_aov," and the p_value for aov was ",p_val_anov_diet)

#g - pw t-test and compare with d)
pw_tt_diet <- pairwise.t.test(d0,d1,d2,d3)
p_val_pw_tt_diet <- pw_tt_diet$p.value
if (p_val_pw_tt_diet <= norm_alpha){paste("Reject the null - Equality of Means Is Not Met")}
if (p_val_pw_tt_diet > norm_alpha){paste("Cannot reject the null - Equality of Means Is Not Met")}
paste0("Comparing the bonf adjusted pw t-test to pw t-test, the p-value for bonf adjusted was ",p_val_bonf_tt_diet," and the p_value for pw t-test was ",pw_tt_diet)


#Problem 4)

seq <- "aabbaaaabbbaababbaaabbbbbababbbabbb"
seq_split <- strsplit(seq,"")[[1]]
runs.test(seq_split)
A <- sum(gregexpr("a",seq)[[1]] != -1)
B <- sum(gregexpr("b",seq)[[1]] != -1)

count = 0
old_char <- "a"
for (char in seq_split){
  if (char == old_char){next}
  if (char != old_char){count <- count + 1}
  old_char <- char}
runs <- count
paste(runs) # runs is odd, therefore I will use the runs value for the odd equation and runs-1 value for the even equation 

perm <- function(n,r){
  perm_ans <- factorial(n)/(factorial(r)*factorial(n-r))
  return(perm_ans)
}

odd_run <- ((perm(A-1,(runs-1)/2)*perm(B-1,(runs-3)/2)) + (perm(A-1,(runs-3)/2)*perm(B-1,(runs-1)/2)))/(perm(A+B,A))
runs <- runs - 1
even_run <- (2*(perm(A-1,(runs/2)-1))*perm(B-1,(runs/2)-1))/(perm(A+B,A))
p_val_runs <- odd_run + even_run

if (p_val_runs <= norm_alpha){paste("Reject the null - Not Random")}
if (p_val_runs > norm_alpha){paste("Cannot reject the null - Can Assume Random")}
