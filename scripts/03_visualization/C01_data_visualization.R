# Data Visualization
## Preparation
packages = c(
	"ggplot2", "tidylog", "showtext", "ggtext", 
       "colorBlindness", "scales", "sf", "paletteer",
       "viridis")

check_pacakges <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

showtext_auto()
font_add_google("Roboto", "roboto_thin", regular.wt = 100)
font_add_google("Roboto", "roboto_light", regular.wt = 300)
font_add_google("Roboto", "roboto_regular", regular.wt = 400)
font_add_google("Roboto", "roboto_med", regular.wt = 500)
font_add_google("Roboto", "roboto_bold", regular.wt = 700)
font_add_google("Roboto", "roboto_black", regular.wt = 900)
font_add_google("Roboto Condensed", "roboto_c_light", regular.wt = 300)
font_add_google("Roboto Condensed", "roboto_c_regular", regular.wt = 400)
font_add_google("Roboto Condensed", "roboto_c_bold", regular.wt = 700)
### Fonts:
font_families()

#We can see our cleaned data with this:
list.files("Data/cleaned_data")

### THE FOLLOWING CODE IS FOR THE FIRST CHART ONLY:
#Loading the data for the first graph:
final_income <- readRDS("Data/cleaned_data/final_income.rds")
overall_all_general <- readRDS("Data/cleaned_data/overall_all_general.rds")
#Gender gap percentage: 
# overall_all_general %>% 
       # summarise(percentage = (median_income_m - median_income_w)/median_income_m * 100)

# Testing for color blindness:
  # displayAllColors(c("#95a7b1", "#64596c", "#a66472"))
  # displayColors(c("#E69F00", "#b2b5b8"), type = "protan")
  # displayAllColors(c("#E69F00", "#0072B2"))

#transparent background for ggplot:
theme_transparent <- function(){
  theme(panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.y = element_blank())
}

#Function to trasnform PEN into usd:
sol_usd_labels <- function(x){
       sol_part <- paste0("S/.", formatC(x, format = "f", big.mark = ",", digits = 0))
       usd_part <- paste0("\n", " (", dollar(x / 3.26, prefix = "$", accuracy = 1), ")")
       paste0(sol_part, "\n", usd_part)
}

#Creating the chart:
p1 <- ggplot(final_income) + 
       geom_density(
              aes(x = income_monthly, 
                        fill = sex,
                        alpha = sex,
                        weight = fac500a),
              linewidth = 0.2, 
              color = NA,
              key_glyph = draw_key_point
       ) +
       scale_x_continuous(
              limits = c(0, 3500),
              breaks = seq(0, 3000, 500),
              labels = sol_usd_labels
       ) +
       scale_y_continuous(
              expand = expansion(mult = c(0.01, 0.05))
       ) +
       scale_fill_manual(
              values = c("hombre" = "#9b2d46", "mujer" = "#17546a"),
              labels = c("hombre" = "Women", "mujer" = "Men")
       ) +
       scale_alpha_manual(
              values = c("hombre" = 0.8, "mujer" = 0.55),
              guide = "none"
       ) +
       labs(
              title = "Women earn 20.7% less than men (S/.280|85$ per month)",
              subtitle = "National median monthly income distribution, Peru (2010-2024)",
              x = "MONTHLY INCOME IN PEN (S/.) AND USD ($)",
       ) +
       theme_transparent() +
       theme(
              axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(margin = margin(t = 20, unit = "pt")),
              axis.text.x = element_text(lineheight = 0.9, margin = margin(t = 5), size = 9)
       ) +
       theme(
              legend.position = "top",
              legend.title.position = "top",
              legend.justification = "left",
              legend.spacing.x = unit(0.15, "cm"),
              legend.text = element_text(margin = margin(t = 2.7))
       ) +
       theme(
              plot.caption = element_text(hjust = 0),
              plot.subtitle = element_text(margin = margin(t = 4, unit = "pt"))
       ) +
       theme(
              plot.title = element_markdown(family = "roboto_bold", 
                                          size = 16.5),
              plot.subtitle = element_markdown(family = "roboto_regular"),
              legend.title = element_blank(),
              legend.text = element_markdown(family = "roboto_med",
                                          size = 9.5),
              axis.text.x = element_markdown(family = "roboto_regular"),
              axis.title.x = element_markdown(family = "roboto_c_regular")
       ) +
       guides(fill = guide_legend(
              override.aes = list(
                     shape = 21, 
                     size = 5,
                     fill = c("#17546a", "#9b2d46"),
                     color = NA,
                     alpha = 1
              )
       )) +
       annotate(
              "richtext",
              x = 0, y = 0.00070,
              label = "More <span style='color:#17546a'>women</span> than <br> 
                <span style='color:#9b2d46'>men </span> are found at <br> 
                the  lowest monthly <br> earnings",
              fill = NA, 
              label.color = NA, 
              size = 3.8, 
              hjust = 0, 
              alpha = 0.9,
              family = "roboto_regular"
       ) +
       annotate(
              geom = "curve",
              x = 300,
              y = 0.00059,
              xend = 430,
              yend = 0.00049, 
              curvature = 0.2, 
              arrow = arrow(length = unit(2, "mm"))
       ) +
       annotate(
              "richtext",
              x = 1900, y = 0.00048,
              label = "Fewer <span style='color:#17546a'>women</span>
                <br> earn higher incomes",
              fill = NA, 
              label.color = NA, 
              size = 3.8, 
              hjust = 0, 
              alpha = 0.8,
              family = "roboto_regular"
       )+
       annotate(
              geom = "curve",
              x = 2200,
              y = 0.00042,
              xend = 2000,
              yend = 0.00032, 
              curvature = -0.4, 
              arrow = arrow(length = unit(2, "mm"))
       ) 


#CREATING THE SECOND CHART:
ggsave("charts/plot1.pdf",plot =p1, device = cairo_pdf, width = 7, height = 5.8)
#Removing last files from the environment 
rm(list = ls())

#Loading our cleaned our data
# list.files("Data/cleaned_data")
pay_gap_department <- readRDS("Data/cleaned_data/pay_gap_department.rds")
dep_per <- readRDS("Data/cleaned_data/dep_per.rds")


pay_gap_department %>% 
  left_join(., dep_per, by = "department") %>% 
  mutate(percentage_gap = ((median_income_m-median_income_w)/median_income_m)*100) %>% 
  select(department, gap, percentage_gap, geom) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(
    aes(fill = percentage_gap),
    linewidth = 0
  ) +
  scale_fill_viridis(
    option = "mako",
    direction = -1,
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(10, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    )
  ) +
  theme_transparent() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(hjust = 0.5)
  )
