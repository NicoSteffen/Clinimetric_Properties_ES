#Rasch dichotom

data = read.csv2("clean.csv")



select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

#ES_likert

#select data

df <- data %>% 
  select(starts_with("ES_"),
         age,sex)  %>% 
  select(!starts_with("ES_likert_")) %>% 
  select(!ES_total)

glimpse(df)

itemlabels <- df %>% 
  select(starts_with("ES_likert_")) %>% 
  names() %>% 
  as_tibble() %>% 
  separate(value, c(NA, "item"), sep ="_[0-9][0-9]_") %>% 
  mutate(itemnr = paste0("ES_likert_",c(1:10)), .before = "item")

#remove non-binär

df <- df %>% 
  filter(sex %in% c("männlich","weiblich"))

#Vektor for sex
dif.sex <- factor(df$sex)

#and remove from df
df$sex <- NULL

#schöne tabelle mit percent
RIdemographics(dif.sex, "Sex")

#weiter mit age
glimpse(df$age)

#Erstelle ggplot für Altersverteilung
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

#remove age from df
dif.age <- df$age
df$age <- NULL

#check for missing data
RImissing(df)

#check overall responses
RIallresp(df)

#check for floor / ceiling effects
RIrawdist(df)

#While not really necessary, it could be interesting to see whether the response patterns follow a 
#Guttman-like structure. Items and persons are sorted based on lower->higher responses, 
#and we should see the color move from yellow in the lower left corner to blue in the upper right corner.

RIheatmap(df) +
  theme(axis.text.x = element_blank())

#It is usually recommended to have at least ~10 responses 
#in each category for psychometric analysis, no matter which methodology is used.

RItileplot(df)

# item fit 
simfit1 <- RIgetfit(df, iterations = 1000, cpu = 8) 
RIitemfit(df, simfit1)

# residual correlations
simcor1 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor1$p99)

#PCA of residuals
RIpcmPCA(df)

#loadings on first residuals contrast
RIloadLoc(df, model = "rm")
