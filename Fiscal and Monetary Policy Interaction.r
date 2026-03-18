############################################################
# FISCAL–MONETARY POLICY INTERACTION IN NEPAL
# Khaiyam Khalid - MA Thesis
############################################################




#     Description: The interaction is assessed following the
#                  poly-rule assessment (no regression model is used)

#     Fiscal-sdie variables: Tex rvenue, Fiscal balance, GFCE
#     Monetary Side Variables: M2, Interest rate, Credit to private sector
#     Macrovaribales varibales: GDP, Housecold Consumption, Inflation

#note: policy variables are %  GDP 



#-----------------------------------------------------------
# 1. Install and Load Packages
#-----------------------------------------------------------

install.packages(c("tidyverse","readxl","janitor","ggplot2","corrplot"))
install.packages(c("ggthemes","scales","patchwork"))

library(ggthemes)
library(scales)
library(patchwork)
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(corrplot)

#-----------------------------------------------------------
# 2. Load Data
#-----------------------------------------------------------

Thesis_data <- Data_of_Thesis_0_exl

# Rename variables

colnames(Thesis_data) <- c(
  "Year",
  "N_GDP",
  "Debt_GDP",
  "R_GDP",
  "GDP_growth",
  "GDP_percapita",
  "GFCE",
  "Population_growth",
  "Population",
  "Inflation",
  "Interest_rate",
  "Fiscal_balance",
  "Household_consumption",
  "Capital_formation",
  "Tax_revenue",
  "Remittance",
  "GDP_deflator",
  "Inflation_deflator",
  "Credit_private",
  "Year2",
  "M2"
)

#-----------------------------------------------------------
# 3. Data Cleaning
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  arrange(Year)

# Check structure
str(Thesis_data)

# Remove missing rows if any
Thesis_data <- na.omit(Thesis_data)

#-----------------------------------------------------------
# 4. Define Variables
#-----------------------------------------------------------

# Fiscal variables
fiscal_vars <- Thesis_data %>%
  select(Year, GFCE, Fiscal_balance, Tax_revenue)

# Monetary variables
monetary_vars <- Thesis_data %>%
  select(Year, M2, Interest_rate, Credit_private)

# Outcome variables
outcomes <- Thesis_data %>%
  select(Year, Inflation, GDP_growth, Household_consumption)




#-----------------------------------------------------------
# 7. Fiscal Policy Trend
#-----------------------------------------------------------

fiscal_long <- fiscal_vars %>%
  pivot_longer(-Year, names_to="Fiscal_variable", values_to="Value")

ggplot(fiscal_long,
       aes(x=Year,y=Value,color=Fiscal_variable))+
  geom_line(size=1.2)+
  geom_point(size=2)+
  theme_minimal()+
  labs(
    title="Fiscal Policy Indicators in Nepal",
    y="% of GDP"
  )



#-----------------------------------------------------------
# 8. Monetary Policy Trend
#-----------------------------------------------------------

monetary_long <- monetary_vars %>%
  pivot_longer(-Year, names_to="Monetary_variable", values_to="Value")

ggplot(monetary_long,
       aes(x=Year,y=Value,color=Monetary_variable))+
  geom_line(size=1.2)+
  geom_point(size=2)+
  theme_minimal()+
  labs(
    title="Monetary Policy Indicators in Nepal",
    y="Value"
  )



#-----------------------------------------------------------
# 9. Create Policy Trends
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    
    # Fiscal trends
    GFCE_change = GFCE - lag(GFCE),
    Fiscal_balance_change = Fiscal_balance - lag(Fiscal_balance),
    
    # Monetary trends
    M2_change = M2 - lag(M2),
    Interest_change = Interest_rate - lag(Interest_rate)
  )



#-----------------------------------------------------------
# 10. Fiscal Policy Classification
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    
    Fiscal_policy = case_when(
      GFCE_change > 0 & Fiscal_balance_change < 0 ~ "Expansionary",
      GFCE_change < 0 & Fiscal_balance_change > 0 ~ "Contractionary",
      TRUE ~ "Neutral"
    )
  )



#-----------------------------------------------------------
# 11. Monetary Policy Classification
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    
    Monetary_policy = case_when(
      M2_change > 0 & Interest_change <= 0 ~ "Expansionary",
      M2_change < 0 & Interest_change > 0 ~ "Contractionary",
      TRUE ~ "Neutral"
    )
  )



#-----------------------------------------------------------
# 12. Fiscal–Monetary Interaction
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    
    Interaction = case_when(
      Fiscal_policy=="Expansionary" & Monetary_policy=="Expansionary" ~ "Coordinated Expansion",
      Fiscal_policy=="Contractionary" & Monetary_policy=="Contractionary" ~ "Coordinated Tightening",
      Fiscal_policy=="Expansionary" & Monetary_policy=="Contractionary" ~ "Policy Conflict",
      Fiscal_policy=="Contractionary" & Monetary_policy=="Expansionary" ~ "Policy Conflict",
      TRUE ~ "Mixed"
    )
  )



#-----------------------------------------------------------
# 13. Interaction Table
#-----------------------------------------------------------

interaction_table <- Thesis_data %>%
  select(Year,Fiscal_policy,Monetary_policy,Interaction)

print(interaction_table)



#-----------------------------------------------------------
# 14. Interaction Visualization
#-----------------------------------------------------------

ggplot(Thesis_data,
       aes(x=factor(Year),fill=Interaction))+
  geom_bar()+
  theme_minimal()+
  labs(
    title="Fiscal–Monetary Policy Interaction in Nepal",
    x="Year",
    y="Count"
  )

#-----------------------------------------------------------
# 15. Impact on Inflation
#-----------------------------------------------------------

ggplot(Thesis_data,
       aes(x=Year,y=Inflation,color=Interaction))+
  geom_line(size=1.2)+
  geom_point(size=2)+
  theme_minimal()+
  labs(
    title="Inflation under Fiscal–Monetary Interaction",
    y="Inflation (%)"
  )

#-----------------------------------------------------------
# 16. Impact on GDP Growth
#-----------------------------------------------------------

ggplot(Thesis_data,
       aes(x=Year,y=GDP_growth,color=Interaction))+
  geom_line(size=1.2)+
  geom_point(size=2)+
  theme_minimal()+
  labs(
    title="GDP Growth and Policy Interaction",
    y="GDP Growth (%)"
  )


#-----------------------------------------------------------
# 18. Inflation Model
#-----------------------------------------------------------

model2 <- lm(
  Inflation ~ M2 + Interest_rate +
    GFCE + Fiscal_balance,
  data = Thesis_data
)

summary(model2)





#-----------------------------------------------------------
#               REFINED GRAPHS AND TABLES: 

#Journal theme
#-----------------------------------------------------------


theme_journal <- theme_classic() +
  theme(
    text = element_text(family="serif", size=12),
    plot.title = element_text(size=14, face="bold"),
    axis.title = element_text(size=12),
    axis.text = element_text(size=11),
    legend.position = "bottom",
    legend.title = element_blank()
  )





#-----------------------------------------------------------
# Fiscal Side
#-----------------------------------------------------------


fiscal_long <- Thesis_data %>%
  select(Year, GFCE, Fiscal_balance, Tax_revenue) %>%
  pivot_longer(-Year,
               names_to="Fiscal_variable",
               values_to="Value")



#-----------------------------------------------------------
# Fiscal Policy Regimes
#-----------------------------------------------------------


fig_fiscal <- ggplot(
  Thesis_data,
  aes(x = Year, y = GFCE)
) +
  
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  geom_point(
    aes(color = Fiscal_policy),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Expansionary" = "#1b9e77",
      "Neutral" = "#7570b3",
      "Contractionary" = "#d95f02"
    )
  ) +
  
  labs(
    title = "Fiscal Policy Regimes",
    x = "Year",
    y = "Government Final Consumption (% of GDP)",
    color = "Fiscal Policy"
  ) +
  
  theme_journal

fig_fiscal



ggsave(
  "fig_fiscal_policy.pdf",
  fig_fiscal,
  width = 7,
  height = 5
)





#-----------------------------------------------------------
# Monetary Side
#-----------------------------------------------------------

#-----------------------------------------------------------
# Monetary Policy Regimes
#-----------------------------------------------------------

fig_monetary <- ggplot(
  Thesis_data,
  aes(x = Year, y = M2)
) +
  
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  geom_point(
    aes(color = Monetary_policy),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Expansionary" = "#1b9e77",
      "Neutral" = "#7570b3",
      "Contractionary" = "#d95f02"
    )
  ) +
  
  labs(
    title = "Monetary Policy Regimes",
    x = "Year",
    y = "Broad Money (M2)",
    color = "Monetary Policy"
  ) +
  
  theme_journal

fig_monetary


ggsave(
  "fig_monetary_policy.pdf",
  fig_monetary,
  width = 7,
  height = 5
)


#-----------------------------------------------------------
# TABLE OF CLASSIFICATION
#-----------------------------------------------------------

table(Thesis_data$Fiscal_policy)

table(Thesis_data$Monetary_policy)

table(Thesis_data$Interaction)


Classifiedtableofinteraction <- data.frame(Thesis_data$Fiscal_policy, 
                                           Thesis_data$Monetary_policy, 
                                           Thesis_data$Interaction             
)
Classifiedtableofinteraction


#-----------------------------------------------------------
# Regimes and Interaction Matrix
#-----------------------------------------------------------

library(dplyr)
library(tidyr)

policy_long <- Thesis_data %>%
  select(Year, Fiscal_policy, Monetary_policy, Interaction) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Policy_Type",
    values_to = "Policy_State"
  )





fig_policy_timeline <- ggplot(
  policy_long,
  aes(
    x = Year,
    y = Policy_Type,
    fill = Policy_State
  )
) +
  
  geom_tile(
    color = "white",
    height = 0.6
  ) +
  
  scale_fill_manual(
    values = c(
      "Expansionary" = "#1b9e77",
      "Neutral" = "#7570b3",
      "Contractionary" = "#d95f02",
      "Coordinated" = "#66a61e",
      "Mixed" = "#e7298a"
    )
  ) +
  
  labs(
    title = "Fiscal and Monetary Policy Regimes in Nepal",
    x = "Year",
    y = ""
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

fig_policy_timeline


ggsave(
  "fig_policy_regime_timeline.pdf",
  fig_policy_timeline,
  width = 7,
  height = 3
)



policy_long$Policy_Type <- factor(
  policy_long$Policy_Type,
  levels = c("Fiscal_policy","Monetary_policy","Interaction")
)


#-----------------------------------------------------------
# MATRIX GRAPH
#-----------------------------------------------------------
# Ensure interaction labels are clean
Thesis_data$Interaction <- recode(
  Thesis_data$Interaction,
  "Coordinated Expansion" = "Coordinated"
)

# Consistent color palette used across thesis
interaction_colors <- c(
  "Coordinated" = "#1b9e77",
  "Mixed" = "#d95f02"
)

fig_interaction <- ggplot(
  Thesis_data,
  aes(x = Year, y = Inflation)
) +
  
  # main time-series line
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  # regime markers
  geom_point(
    aes(color = Interaction),
    size = 3
  ) +
  
  # consistent colors
  scale_color_manual(
    values = interaction_colors,
    drop = FALSE
  ) +
  
  # nicer axis formatting
  scale_x_continuous(
    breaks = Thesis_data$Year
  ) +
  
  labs(
    title = "Fiscal–Monetary Policy Interaction",
    subtitle = "Inflation under Coordinated and Mixed Policy Regimes",
    x = "Year",
    y = "Inflation (%)"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

fig_interaction










#-----------------------------------------------------------
# Inflation Graph
#-----------------------------------------------------------

fig_inflation <- ggplot(
  Thesis_data,
  aes(x = Year, y = Inflation)
) +
  
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  geom_point(
    aes(color = Interaction),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Coordinated Expansion" = "#1b9e77",
      "Mixed" = "#d95f02"
    )
  ) +
  
  labs(
    title = "Inflation under Fiscal–Monetary Policy Interaction",
    x = "Year",
    y = "Inflation (%)"
  ) +
  
  theme_journal





fig_inflation


ggsave(
  "fig_inflation_interaction.png",
  fig_inflation,
  width=8,
  height=5,
  dpi=600
)












#-----------------------------------------------------------
# GDP under interaction 
#-----------------------------------------------------------

fig_gdp <- ggplot(
  Thesis_data,
  aes(x = Year, y = GDP_growth)
) +
  
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  geom_point(
    aes(color = Interaction),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Coordinated Expansion" = "#1b9e77",
      "Mixed" = "#d95f02"
    )
  ) +
  
  labs(
    title = "GDP under Fiscal–Monetary Policy Interaction",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  
  theme_journal


fig_gdp



ggsave(
  "fig_gdp_interaction.png",
  fig_gdp,
  width=8,
  height=5,
  dpi=600
)



#-----------------------------------------------------------
# House Hold Consumption Expenditure
#-----------------------------------------------------------

fig_consumption <- ggplot(
  Thesis_data,
  aes(x = Year, y = Household_consumption)
) +
  
  geom_line(
    color = "black",
    linewidth = 1
  ) +
  
  geom_point(
    aes(color = Interaction),
    size = 3
  ) +
  
  scale_color_manual(
    values = c(
      "Coordinated Expansion" = "#1b9e77",
      "Mixed" = "#d95f02"
    )
  ) +
  
  labs(
    title = "Household Consumption under Policy Interaction",
    x = "Year",
    y = "Household Consumption (% of GDP)"
  ) +
  
  theme_journal


fig_consumption



ggsave(
  "fig_consumption_fiscal.png",
  fig_consumption,
  width=8,
  height=5,
  dpi=600
)


#-----------------------------------------------------------
# 3-Panel figure
#-----------------------------------------------------------

install.packages("patchwork")
library(patchwork)


fig_macro <- (
  fig_inflation /
    fig_gdp /
    fig_consumption
) +
  plot_annotation(tag_levels = "A")

fig_macro





fig_macro <- (fig_inflation /
                fig_gdp /
                fig_consumption) +
  plot_annotation(tag_levels = "A")


ggsave(
  "fig_macro_policy_outcomes.pdf",
  fig_macro,
  width = 8,
  height = 12
)



#-----------------------------------------------------------
# TABLE
#-----------------------------------------------------------

interaction_table <- Thesis_data %>%
  select(
    Year,
    Fiscal_policy,
    Monetary_policy,
    Interaction
  )

interaction_table








interaction_summary <- Thesis_data %>%
  count(Interaction) %>%
  mutate(
    Percentage = round(100 * n / sum(n),2)
  )

interaction_summary





policy_matrix <- table(
  Thesis_data$Fiscal_policy,
  Thesis_data$Monetary_policy
)

policy_matrix



install.packages("knitr")
install.packages("kableExtra")

library(knitr)
library(kableExtra) 



kable(
  interaction_summary,
  caption = "Fiscal–Monetary Policy Interaction in Nepal",
  align = "c"
) %>%
  kable_styling(
    latex_options = c("hold_position","striped")
  )


kable(
  interaction_summary,
  format = "latex",
  caption = "Fiscal–Monetary Policy Interaction in Nepal"
)




write.csv(
  interaction_summary,
  "policy_interaction_summary.csv",
  row.names = FALSE
)



ggplot(interaction_summary,
       aes(x=Interaction,y=n,fill=Interaction)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(
    title="Frequency of Fiscal–Monetary Policy Interaction",
    x="Policy Interaction Type",
    y="Number of Years"
  ) +
  theme(legend.position="none")




#-----------------------------------------------------------
# Descriptive TABLE
#-----------------------------------------------------------

desc_data <- Thesis_data %>%
  select(
    GFCE,
    Fiscal_balance,
    Tax_revenue,
    M2,
    Interest_rate,
    Credit_private,
    Inflation,
    GDP_growth,
    Household_consumption
  )



desc_table <- data.frame(
  Variable = colnames(desc_data),
  
  Mean = sapply(desc_data, mean, na.rm = TRUE),
  SD = sapply(desc_data, sd, na.rm = TRUE),
  Min = sapply(desc_data, min, na.rm = TRUE),
  Max = sapply(desc_data, max, na.rm = TRUE),
  N = sapply(desc_data, function(x) sum(!is.na(x)))
)






desc_table$CV <- desc_table$SD / desc_table$Mean






desc_table <- desc_table %>%
  select(Variable, Mean, SD, CV, Min, Max, N)






desc_table <- desc_table %>%
  mutate(
    Mean = round(Mean,2),
    SD = round(SD,2),
    CV = round(CV,2),
    Min = round(Min,2),
    Max = round(Max,2)
  )




install.packages("kableExtra")
library(kableExtra)


knitr::kable(
  desc_table,
  caption = "Table 1: Descriptive Statistics of Fiscal, Monetary and Macroeconomic Variables",
  align = "c",
  digits = 2
) %>%
  kable_styling(
    latex_options = c("hold_position","striped"),
    font_size = 11
  )






knitr::kable(
  desc_table,
  format = "latex",
  caption = "Descriptive Statistics"
)






############################################################
# Fiscal–Monetary Policy Quadrant Diagram
# Khaiyam Khalid – MA Thesis
############################################################

#-----------------------------------------------------------
# 1. Install and load required packages
#-----------------------------------------------------------

install.packages("ggrepel")

library(ggplot2)
library(dplyr)
library(ggrepel)

#-----------------------------------------------------------
# 2. Construct fiscal and monetary stance variables
#-----------------------------------------------------------

Thesis_data <- Thesis_data %>%
  mutate(
    
    # Fiscal stance proxy
    Fiscal_stance = GFCE_change - Fiscal_balance_change,
    
    # Monetary stance proxy
    Monetary_stance = M2_change - Interest_change
  )

#-----------------------------------------------------------
# 3. Create quadrant diagram
#-----------------------------------------------------------

fig_quadrant <- ggplot(
  Thesis_data,
  aes(
    x = Monetary_stance,
    y = Fiscal_stance,
    label = Year
  )
) +
  
  # quadrant lines
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  # data points
  geom_point(
    size = 3,
    color = "steelblue"
  ) +
  
  # repel labels to avoid overlap
  geom_text_repel(
    size = 3.5,
    max.overlaps = 20
  ) +
  
  # titles
  labs(
    title = "Fiscal–Monetary Policy Interaction in Nepal",
    subtitle = "Quadrant Representation of Policy Stances",
    x = "Monetary Policy Stance",
    y = "Fiscal Policy Stance"
  ) +
  
  theme_classic()

#-----------------------------------------------------------
# 4. Add quadrant descriptions
#-----------------------------------------------------------

fig_quadrant <- fig_quadrant +
  
  annotate(
    "text",
    x = max(Thesis_data$Monetary_stance)*0.8,
    y = max(Thesis_data$Fiscal_stance)*0.9,
    label = "Coordinated Expansion",
    size = 4,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = min(Thesis_data$Monetary_stance)*0.8,
    y = max(Thesis_data$Fiscal_stance)*0.9,
    label = "Fiscal Tight\nMonetary Expansion",
    size = 4
  ) +
  
  annotate(
    "text",
    x = min(Thesis_data$Monetary_stance)*0.8,
    y = min(Thesis_data$Fiscal_stance)*0.9,
    label = "Coordinated Tightening",
    size = 4,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = max(Thesis_data$Monetary_stance)*0.8,
    y = min(Thesis_data$Fiscal_stance)*0.9,
    label = "Fiscal Expansion\nMonetary Tight",
    size = 4
  )

#-----------------------------------------------------------
# 5. Display the figure
#-----------------------------------------------------------

fig_quadrant

#-----------------------------------------------------------
# 6. Save high-quality version
#-----------------------------------------------------------

ggsave(
  "fig_policy_quadrant_nepal.pdf",
  fig_quadrant,
  width = 7,
  height = 6
)

ggsave(
  "fig_policy_quadrant_nepal.png",
  fig_quadrant,
  width = 7,
  height = 6,
  dpi = 600
)




fig_quadrant










table(Thesis_data$Fiscal_policy)






Thesis_data


write_csv(Thesis_data, "FMData.csv")





