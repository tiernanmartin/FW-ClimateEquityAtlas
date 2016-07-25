library(grDevices)      # boxplot.stats()
library(operator.tools) # %!in% logical operator
library(tmap)           # 'metro' data set
library(magrittr)       # piping
library(dplyr)          # exploratory data analysis verbs
library(purrr)          # recursive mapping of functions 
library(tibble)         # improved version of a data.frame
library(ggplot2)        # dot plot
library(ggrepel)        # avoid label overlap

options(scipen=999)
set.seed(888)

data("metro")

m_spdf <- metro


# Take a sample
m <-  
        metro@data %>% 
        as_tibble %>% 
        select(-name_long,-iso_a3) %>% 
        sample_n(50)

# Calculate the quintile groups for one variable (e.g., `pop1990`)
m_all <- 
        m %>% 
        mutate(qnt_1990_all = dplyr::ntile(pop1990,5))

# Find the outliers for a different variable (e.g., 'pop1950')
# and subset the df to exlcude these outlier records
m_out <- boxplot.stats(m$pop1950) %>% .[["out"]]
        
m_trim <- 
        m %>% 
        filter(pop1950 %!in% m_out) %>% 
        mutate(qnt_1990_trim = dplyr::ntile(pop1990,5))

# Assess whether the outlier trimming impacted the first quintile group
m_comp <- 
        m_trim %>% 
        select(name,dplyr::contains("qnt")) %>% 
        left_join(m_all,.,"name") %>% 
        select(name,dplyr::contains("qnt"),everything()) %>% 
        mutate(qnt_1990_chng_lgl = !is.na(qnt_1990_trim) & qnt_1990_trim != qnt_1990_all,
               qnt_1990_chng_dir = if_else(qnt_1990_chng_lgl,
                                           paste0(qnt_1990_all," to ",qnt_1990_trim),
                                           "No change"))
# Plot the result
gg <- ggplot(data = m_comp,aes(x = pop1990,y = pop1950))
gg <- gg + geom_point(color = "gray45",fill = "gray45",alpha = .25)
gg <- gg + geom_point(data = m_comp[m_comp$qnt_1990_chng_lgl,], 
                      aes(x = pop1990,y = pop1950, color = qnt_1990_chng_dir))
gg <- gg + geom_text_repel(data = subset(m_comp,m_comp$qnt_1990_chng_lgl),
                     aes(pop1990,pop1950, label = name, color = qnt_1990_chng_dir),
                     nudge_x = 1000000,nudge_y = -100000,
                     show.legend=FALSE)
gg <- gg + theme_light() 
gg <- gg + theme(panel.border = element_blank(),
                 legend.title = element_blank(),
                 legend.background = element_blank()
                 )
gg
