library(easyRasch)
library(grateful)
library(ggrepel)
library(car)
library(kableExtra)
library(readxl)
library(eRm)
library(iarm)
library(mirt)
library(psych)
library(ggplot2)
library(psychotree)
library(matrixStats)
library(reshape)
library(knitr)
library(patchwork)
library(formattable) 
library(glue)
library(foreach)

select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

raschlmu = read.csv2("raschlmu.csv")
raschnon = read.csv2("raschnon.csv")

#rasch = raschnon

rasch = rbind(raschlmu,raschnon )
rasch = rasch[,2:14]

glimpse(rasch)

rasch$sex <- as.factor(rasch$sex)
rasch$Population <- as.factor(rasch$Population)
  
rasch <- rasch %>%
  mutate(across(c(age, starts_with("ES_")), as.double))

itemlabels <- rasch %>%
  dplyr::select(starts_with("ES_")) %>%
  names() %>%
  tibble(item = .) %>%
  mutate(itemnr = paste0("ES_", 1:10), .before = "item")

rasch %>% 
  dplyr::select(sex,age,Population) %>% 
  glimpse()

rasch <- rasch %>% 
  filter(sex %in% c("female","male")) # --> removed two divers

df = rasch

summary(df$age)

ggplot(df) +
  geom_histogram(aes(x = age), 
                 fill = "#009ca6",
                 col = "black") +
  # add the average as a vertical line
  geom_vline(xintercept = median(df$age), 
             linewidth = 1.5,
             linetype = 2,
             col = "orange") +
  # add a light grey field indicating the standard deviation
  annotate("rect", ymin = 0, ymax = Inf, 
           xmin = summary(df$age)[2], 
           xmax = summary(df$age)[5], 
           alpha = .2) +
  labs(title = "",
       x = "Age in years",
       y = "Number of respondents",
       caption = str_wrap(glue("Note. Median age is {round(median(df$age, na.rm = T),1)}, shown with the dashed vertical line. Age range is {min(df$age)} to {max(df$age)}. Interquartile range ({summary(df$age)[2]} to {summary(df$age)[5]}) is indicated with the grey area.")
       )) +
  scale_x_continuous(limits = c(18,75)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) 

dif <- df %>% 
  select(sex,age,Population) %>% 
  rename(Age = age) # just for consistency

df <- df %>% 
  select(!c(sex,age,Population))

gtsummary::tbl_summary(dif)

names(df) <- itemlabels$itemnr

RImissing(df)

RIallresp(df)

RIrawdist(df)

RIheatmap(df) +
  theme(axis.text.x = element_blank())

RItileplot(df)

simfit1 <- RIgetfit(df, iterations = 200, cpu = 8, ) 
RIitemfit(df, simfit1)

df

# nononly = RIitemfit(df, simfit1)

#df <- df %>% 
  #select(!("ES_6"))

# split 

# df1 = df[,1:5]
# df2 = df[, 6:10]
# 
# simfit2 <- RIgetfit(df1, iterations = 200, cpu = 8, ) # save simulation output to object `simfit1`
# RIitemfit(df1, simfit2)
# 
# simfit3 <- RIgetfit(df2, iterations = 200, cpu = 8, ) # save simulation output to object `simfit1`
# RIitemfit(df2, simfit3)

#eigenvalue
RIpcmPCA(df)
pcasim <- RIbootPCA(df, iterations = 500, cpu = 8)
hist(pcasim$results, breaks = 50)

pcasim$p99

pcasim$max

# residual correlations / local independence

simcor1 <- RIgetResidCor(df, iterations = 400, cpu = 8)
RIresidcorr(df, cutoff = simcor1$p99)


RIdifTable(df, dif$Age)
RIdifTable(df, dif$Population)
RIdifTable2(df, dif$sex, dif$Age)

RIdifTableLR(df, dif$Population)

RIciccPlot(df, dif = "yes", dif_var = dif$sex)

RIpfit(df)

RIrestscore(df)

fa.parallel(df, fa = "fa", quant = .95, n.iter = 1000, show.legend = TRUE)


