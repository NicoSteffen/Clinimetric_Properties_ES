library(RISEkbmRasch) # devtools::install_github("pgmj/RISEkbmRasch")
library(grateful)
library(ggrepel)
library(car)
library(kableExtra)
library(readxl)
library(tidyverse)
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

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

df.all <- read_csv("https://osf.io/download/6fbr5/")
# if you have issues with the link, please try downloading manually using the same URL as above
# and read the file from your local drive.

# subset items and demographic variables
df <- df.all %>% 
  select(starts_with("PANASD2_1"),
         starts_with("PANASD2_20"),
         age,Sex,Group) %>% 
  select(!PANASD2_10_Active) %>% 
  select(!PANASD2_1_Attentive)

itemlabels <- df %>% 
  select(starts_with("PAN")) %>% 
  names() %>% 
  as_tibble() %>% 
  separate(value, c(NA, "item"), sep ="_[0-9][0-9]_") %>% 
  mutate(itemnr = paste0("PANAS_",c(11:20)), .before = "item")

df <- df %>% 
  filter(Sex %in% c("Female","Male"))

dif.sex <- factor(df$Sex)
df$Sex <- NULL
RIdemographics(dif.sex, "Sex")
### simpler version of the ggplot below using base R function hist()
# hist(df$age, col = "#009ca6")
# abline(v = mean(age, na.rm = TRUE))
# 
# df %>% 
#   summarise(Mean = round(mean(age, na.rm = T),1),
#             StDev = round(sd(age, na.rm = T),1)
#             )

ggplot(df) +
  geom_histogram(aes(x = age), 
                 fill = "#009ca6",
                 col = "black") +
  # add the average as a vertical line
  geom_vline(xintercept = mean(df$age), 
             linewidth = 1.5,
             linetype = 2,
             col = "orange") +
  # add a light grey field indicating the standard deviation
  annotate("rect", ymin = 0, ymax = Inf, 
           xmin = (mean(df$age, na.rm = TRUE) - sd(df$age, na.rm = TRUE)), xmax = (mean(df$age, na.rm = TRUE) + sd(df$age, na.rm = TRUE)), 
           alpha = .2) +
  labs(title = "",
       x = "Age in years",
       y = "Number of respondents",
       caption = glue("Note. Mean age is {round(mean(df$age, na.rm = T),1)} years with a standard deviation of {round(sd(df$age, na.rm = T),1)}. Age range is {min(df$age)} to {max(df$age)}.")
  ) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

dif.age <- df$age
df$age <- NULL

dif.group <- factor(df$Group)
df$Group <- NULL
RIdemographics(dif.group, "Group")

names(df) <- itemlabels$itemnr

df <- df %>% 
  mutate(across(everything(), ~ car::recode(.x, "1=0;2=1;3=2;4=3;5=4", as.factor = F)))

# always check that your recoding worked as intended.
RIallresp(df)


simfit1 <- RIgetfit(df, iterations = 10, cpu = 8) # save simulation output to object `simfit1`
RIitemfit(df, simfit1)

View(RItargeting)


