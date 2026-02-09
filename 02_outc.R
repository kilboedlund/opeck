install.packages('tidyverse')
install.packages('kidney.epi')
library('tidyverse')

opeck_o1 <- read_csv('data.csv', 
                     col_types = paste0(rep('c', 208), 
                                        collapse = '')) %>% 
  mutate(opeck_id = seq(nrow(.))) %>% 
  filter(!is.na(p22599)) %>% 
  select(opeck_id,
         sex = p31,
         age = p21022,
         yob = p34,
         esrd = p42026,
         n03 = p132004,
         crea = p30700_i0,
         cys = p30720_i0,
         ualb = p30500_i0,
         ucre = p30510_i0,
         n07 = p132012,
         smo = p20116_i0,
         bmi = p21001_i0,
         alc = p1558_i0,
         dep = p22189,
         eth = p21000_i0,
         qua = p6138_i0,
         inc = p738_i0) %>% 
  mutate(across(c(sex, age, yob), as.integer),
         across(c(esrd, n03, n07), \(x) as.Date.character(x, format =  '%Y-%m-%d')),
         across(c(crea, cys, ualb, ucre, bmi, dep), as.double),
         across(c(smo, alc, eth, qua, inc), as.factor))

opeck_o2 <- opeck_o1 %>% 
  mutate(uacr = ualb / (ucre / 1000),
         egfr = kidney.epi::egfr.ckdepi.cr_cys.2021(
           creatinine = crea,
           cystatin = cys,
           age = age,
           sex = sex,
           creatinine_units = "micromol/l",
           cystatin_units = "mg/L",
           label_sex_male = c("Male", 1),
           label_sex_female = c("Female", 0),
           max_age = 100
         ))

