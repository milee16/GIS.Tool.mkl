#' Gradient analysis of VF
#'
#' Uses Model data for VF divided into five gradient polygons. Model scenarios are "Baseline", "N-30%" and "P-30%"
#' @param VFData Tables of gradient polygons
#' @return Vertical profiles and Monod growth kinetics in various plots
#' @export
RF_GIS_tool <- function() {
  if(!require(ggplot2)) install.packages("ggplot2")
  if(!require(extrafont)) install.packages("extrafont")
  library(ggplot2)
  library(extrafont)
  font_import()
  loadfonts(device = "win")
  fonts()

  theme_pub <<- function(base_size = 14, base_family = "Times New Roman",
                         line_size = 0.25, ...) {
    half_line <<- base_size / 2
    small_rel <<- 0.8
    small_size <<- small_rel * base_size

    theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
      theme(
        rect = element_rect(fill = NA, color = "black", size = 1.5),
        text = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size, hjust = 0.5,
                            vjust = 0.5, angle = 0, lineheight = 0.9,
                            margin = ggplot2::margin(), debug = F),

        axis.text = element_text(size = base_size),
        axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                   vjust = 1),
        axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4),
                                   hjust = 1),

        axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                             b = small_size)),
        axis.title.y = element_text(angle = 90,
                                    margin = ggplot2::margin(r = small_size,
                                                             l = small_size/4)),
        axis.ticks = element_line(colour = "black", size = line_size+0.75),
        axis.ticks.length = unit(0.25, 'lines'),

        axis.line = element_line(colour = "black", size = line_size),
        axis.line.x = element_line(colour = "black", size = line_size),
        axis.line.y = element_line(colour = "black", size = line_size),

        legend.spacing = unit(base_size/4, "pt"),
        legend.key = element_blank(),
        legend.key.size = unit(1.5 * base_size, "pt"),
        legend.key.width = unit(1.5 * base_size, 'pt'),
        legend.text = element_text(size = base_size),
        legend.title = element_blank(),
        legend.position = c(1,1),
        legend.box = 'horizontal',
        legend.justification = c(1,0.9),

        plot.tag.position = c(0.18,0.93),
        plot.tag = element_text(size = 22),

        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        panel.border = element_rect(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        strip.text = element_text(size = base_size),
        strip.background = element_rect(fill = NA, colour = "black", size = 0.125),
        strip.text.x = element_text(face = 'bold', hjust = 0,
                                    margin = ggplot2::margin(b = small_size/2,
                                                             t = small_size/4)),
        strip.text.y = element_text(angle = -90, face = 'bold',
                                    margin = ggplot2::margin(l = small_size/2,
                                                             r = small_size/4)),

        plot.margin = unit(c(5,5,0,0), "pt"),
        plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 1.2 * base_size,
                                  margin = ggplot2::margin(b = half_line),
                                  hjust = 0)
      )
  }


  if(!require(readr)) install.packages("readr")
  library(readr)

  data("RF_B_G1")
  data("RF_B_G2")
  data("RF_B_G3")
  data("RF_B_G4")

  data("RF_S1_G1")
  data("RF_S1_G2")
  data("RF_S1_G3")
  data("RF_S1_G4")

  data("RF_S2_G1")
  data("RF_S2_G2")
  data("RF_S2_G3")
  data("RF_S2_G4")

  #Load Tidyverse package

  if(!require(tidyverse)) install.packages("tidyverse")
  library(tidyverse)

  #RF_B
  #Rename Columns in RF_B_G1

  RF_B_G1 <<- RF_B_G1 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_B_G2

  RF_B_G2 <<- RF_B_G2 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_B_G3

  RF_B_G3 <<- RF_B_G3 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_B_G4

  RF_B_G4 <<- RF_B_G4 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #RF_S1
  #Rename Columns in RF_S1_G1

  RF_S1_G1 <<- RF_S1_G1 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S1_G2

  RF_S1_G2 <<- RF_S1_G2 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S1_G3

  RF_S1_G3 <<- RF_S1_G3 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S1_G4

  RF_S1_G4 <<- RF_S1_G4 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #RF_S2
  #Rename Columns in RF_S2_G1

  RF_S2_G1 <<- RF_S2_G1 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S2_G2

  RF_S2_G2 <<- RF_S2_G2 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S2_G3

  RF_S2_G3 <<- RF_S2_G3 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Rename Columns in RF_S2_G4

  RF_S2_G4 <<- RF_S2_G4 %>%
    arrange(desc(upper)) %>%
    rename(
      area_km2 = X_COL3,
      area_m2 = Sum_area_m2_,
      volume_m3 = Sum_volume_m3_,

      EC = X_COL6,
      EN = X_COL7,
      EP = X_COL8,

      BC2 = X_COL9,
      BN2 = X_COL10,
      BP2 = X_COL11,

      BC1 = X_COL12,
      BN1 = X_COL13,
      BP1 = X_COL14,

      BDC = X_COL15,
      BDN = X_COL16,
      BDP = X_COL17,

      PC = X_COL18,
      PN = X_COL19,
      PP = X_COL20,

      CH = X_COL21,

      DIN = X_COL22,
      DIP = X_COL23,

      lo_b = X_COL24,
      AvgSD = Avg_SD_,

      PREC = X_COL26,
      PRBC2 = X_COL27,
      PRBC1 = X_COL28,
      PRBDC = X_COL29,
      PRPC = X_COL30,
      DIN_b = X_COL31,
      DIP_b = X_COL32
    )

  #Create RF_gradient_no entries and Avg DIN/DIP for all scenarios and gradient polygons

  RF_gradient_no <<- c(1:4)

  RF_B_avgDIN_AG <<- c(mean(RF_B_G1$DIN),mean(RF_B_G2$DIN),mean(RF_B_G3$DIN),mean(RF_B_G4$DIN))
  RF_B_avgDIP_AG <<- c(mean(RF_B_G1$DIP),mean(RF_B_G2$DIP),mean(RF_B_G3$DIP),mean(RF_B_G4$DIP))
  RF_B_avgDIN_b_AG <<- c(mean(RF_B_G1$DIN_b),mean(RF_B_G2$DIN_b),mean(RF_B_G3$DIN_b),mean(RF_B_G4$DIN_b))
  RF_B_avgDIP_b_AG <<- c(mean(RF_B_G1$DIP_b),mean(RF_B_G2$DIP_b),mean(RF_B_G3$DIP_b),mean(RF_B_G4$DIP_b))

  RF_S1_avgDIN_AG <<- c(mean(RF_S1_G1$DIN),mean(RF_S1_G2$DIN),mean(RF_S1_G3$DIN),mean(RF_S1_G4$DIN))
  RF_S1_avgDIP_AG <<- c(mean(RF_S1_G1$DIP),mean(RF_S1_G2$DIP),mean(RF_S1_G3$DIP),mean(RF_S1_G4$DIP))
  RF_S1_avgDIN_b_AG <<- c(mean(RF_S1_G1$DIN_b),mean(RF_S1_G2$DIN_b),mean(RF_S1_G3$DIN_b),mean(RF_S1_G4$DIN_b))
  RF_S1_avgDIP_b_AG <<- c(mean(RF_S1_G1$DIP_b),mean(RF_S1_G2$DIP_b),mean(RF_S1_G3$DIP_b),mean(RF_S1_G4$DIP_b))

  RF_S2_avgDIN_AG <<- c(mean(RF_S2_G1$DIN),mean(RF_S2_G2$DIN),mean(RF_S2_G3$DIN),mean(RF_S2_G4$DIN))
  RF_S2_avgDIP_AG <<- c(mean(RF_S2_G1$DIP),mean(RF_S2_G2$DIP),mean(RF_S2_G3$DIP),mean(RF_S2_G4$DIP))
  RF_S2_avgDIN_b_AG <<- c(mean(RF_S2_G1$DIN_b),mean(RF_S2_G2$DIN_b),mean(RF_S2_G3$DIN_b),mean(RF_S2_G4$DIN_b))
  RF_S2_avgDIP_b_AG <<- c(mean(RF_S2_G1$DIP_b),mean(RF_S2_G2$DIP_b),mean(RF_S2_G3$DIP_b),mean(RF_S2_G4$DIP_b))


  #Transmute area-specific VF tables to non area-specific tables to use in creating single values data frame for all gradient polygons later on

  RF_B_G1_no_Aspec <<- RF_B_G1 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_B_G2_no_Aspec <<- RF_B_G2 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_B_G3_no_Aspec <<- RF_B_G3 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_B_G4_no_Aspec <<- RF_B_G4 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S1_G1_no_Aspec <<- RF_S1_G1 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S1_G2_no_Aspec <<- RF_S1_G2 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S1_G3_no_Aspec <<- RF_S1_G3 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S1_G4_no_Aspec <<- RF_S1_G4 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S2_G1_no_Aspec <<- RF_S2_G1 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S2_G2_no_Aspec <<- RF_S2_G2 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S2_G3_no_Aspec <<- RF_S2_G3 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  RF_S2_G4_no_Aspec <<- RF_S2_G4 %>%
    transmute(
      EC = EC*area_m2,
      EN = EN*area_m2,
      EP = EP*area_m2,

      BC2 = BC2*area_m2,
      BN2 = BN2*area_m2,
      BP2 = BP2*area_m2,

      BC1 = BC1*area_m2,
      BN1 = BN1*area_m2,
      BP1 = BP1*area_m2,

      BDC = BDC*area_m2,
      BDN = BDN*area_m2,
      BDP = BDP*area_m2,

      PC = PC*volume_m3,
      PN = PN*volume_m3,
      PP = PP*volume_m3,

      CH = CH*volume_m3,

      DIN = DIN*volume_m3,
      DIP = DIP*volume_m3,

      lo_b = lo_b*area_m2,

      PREC = PREC*area_m2,
      PRBC2 = PRBC2*area_m2,
      PRBC1 = PRBC1*area_m2,
      PRBDC = PRBDC*area_m2,
      PRPC = PRPC*area_m2,
      DIN_b = DIN_b*volume_m3,
      DIP_b = DIP_b*volume_m3
    )

  #Create single entries of summed area and volume for all gradient polygons used to create primary producers biomass and production single values for all gradient polygons

  RF_sumarea_G1 <<- sum(RF_B_G1$area_m2);RF_sumarea_G2 <<- sum(RF_B_G2$area_m2);RF_sumarea_G3 <<- sum(RF_B_G3$area_m2);RF_sumarea_G4 <<- sum(RF_B_G4$area_m2)
  RF_sumvolume_G1 <<- sum(RF_B_G1$volume_m3);RF_sumvolume_G2 <<- sum(RF_B_G2$volume_m3);RF_sumvolume_G3 <<- sum(RF_B_G3$volume_m3);RF_sumvolume_G4 <<- sum(RF_B_G4$volume_m3)

  # Create summed area and volumes for all gradient polygons used later in final data frame

  RF_B_sumarea_m2_AG <<- c(sum(RF_B_G1$area_m2),sum(RF_B_G2$area_m2),sum(RF_B_G3$area_m2),sum(RF_B_G4$area_m2))
  RF_B_sumvolume_m3_AG <<- c(sum(RF_B_G1$volume_m3),sum(RF_B_G2$volume_m3),sum(RF_B_G3$volume_m3),sum(RF_B_G4$volume_m3))

  RF_S1_sumarea_m2_AG <<- c(sum(RF_S1_G1$area_m2),sum(RF_S1_G2$area_m2),sum(RF_S1_G3$area_m2),sum(RF_S1_G4$area_m2))
  RF_S1_sumvolume_m3_AG <<- c(sum(RF_S1_G1$volume_m3),sum(RF_S1_G2$volume_m3),sum(RF_S1_G3$volume_m3),sum(RF_S1_G4$volume_m3))

  RF_S2_sumarea_m2_AG <<- c(sum(RF_S2_G1$area_m2),sum(RF_S2_G2$area_m2),sum(RF_S2_G3$area_m2),sum(RF_S2_G4$area_m2))
  RF_S2_sumvolume_m3_AG <<- c(sum(RF_S2_G1$volume_m3),sum(RF_S2_G2$volume_m3),sum(RF_S2_G3$volume_m3),sum(RF_S2_G4$volume_m3))

  #Create single value entries for all primary producers (biomass and production) for all gradient polygons and all model scenarios

  RF_B_EC_AG <<- c((sum(RF_B_G1_no_Aspec$EC))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$EC))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$EC))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$EC))/RF_sumarea_G4)
  RF_B_EN_AG <<- c((sum(RF_B_G1_no_Aspec$EN))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$EN))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$EN))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$EN))/RF_sumarea_G4)
  RF_B_EP_AG <<- c((sum(RF_B_G1_no_Aspec$EP))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$EP))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$EP))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$EP))/RF_sumarea_G4)

  RF_B_BC2_AG <<- c((sum(RF_B_G1_no_Aspec$BC2))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BC2))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BC2))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BC2))/RF_sumarea_G4)
  RF_B_BN2_AG <<- c((sum(RF_B_G1_no_Aspec$BN2))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BN2))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BN2))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BN2))/RF_sumarea_G4)
  RF_B_BP2_AG <<- c((sum(RF_B_G1_no_Aspec$BP2))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BP2))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BP2))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BP2))/RF_sumarea_G4)

  RF_B_BC1_AG <<- c((sum(RF_B_G1_no_Aspec$BC1))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BC1))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BC1))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BC1))/RF_sumarea_G4)
  RF_B_BN1_AG <<- c((sum(RF_B_G1_no_Aspec$BN1))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BN1))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BN1))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BN1))/RF_sumarea_G4)
  RF_B_BP1_AG <<- c((sum(RF_B_G1_no_Aspec$BP1))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BP1))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BP1))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BP1))/RF_sumarea_G4)

  RF_B_BDC_AG <<- c((sum(RF_B_G1_no_Aspec$BDC))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BDC))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BDC))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BDC))/RF_sumarea_G4)
  RF_B_BDN_AG <<- c((sum(RF_B_G1_no_Aspec$BDN))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BDN))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BDN))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BDN))/RF_sumarea_G4)
  RF_B_BDP_AG <<- c((sum(RF_B_G1_no_Aspec$BDP))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$BDP))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$BDP))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$BDP))/RF_sumarea_G4)

  RF_B_PC_AG <<- c((sum(RF_B_G1_no_Aspec$PC))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$PC))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$PC))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$PC))/RF_sumvolume_G4)
  RF_B_PN_AG <<- c((sum(RF_B_G1_no_Aspec$PN))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$PN))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$PN))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$PN))/RF_sumvolume_G4)
  RF_B_PP_AG <<- c((sum(RF_B_G1_no_Aspec$PP))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$PP))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$PP))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$PP))/RF_sumvolume_G4)

  RF_B_CH_AG <<- c((sum(RF_B_G1_no_Aspec$CH))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$CH))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$CH))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$CH))/RF_sumvolume_G4)

  RF_B_DIN_AG <<- c((sum(RF_B_G1_no_Aspec$DIN))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$DIN))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$DIN))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$DIN))/RF_sumvolume_G4)
  RF_B_DIP_AG <<- c((sum(RF_B_G1_no_Aspec$DIP))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$DIP))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$DIP))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$DIP))/RF_sumvolume_G4)

  RF_B_PREC_AG <<- c((sum(RF_B_G1_no_Aspec$PREC))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$PREC))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$PREC))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$PREC))/RF_sumarea_G4)
  RF_B_PRBC2_AG <<- c((sum(RF_B_G1_no_Aspec$PRBC2))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$PRBC2))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$PRBC2))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$PRBC2))/RF_sumarea_G4)
  RF_B_PRBC1_AG <<- c((sum(RF_B_G1_no_Aspec$PRBC1))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$PRBC1))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$PRBC1))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$PRBC1))/RF_sumarea_G4)
  RF_B_PRBDC_AG <<- c((sum(RF_B_G1_no_Aspec$PRBDC))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$PRBDC))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$PRBDC))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$PRBDC))/RF_sumarea_G4)
  RF_B_PRPC_AG <<- c((sum(RF_B_G1_no_Aspec$PRPC))/RF_sumarea_G1,(sum(RF_B_G2_no_Aspec$PRPC))/RF_sumarea_G2,(sum(RF_B_G3_no_Aspec$PRPC))/RF_sumarea_G3,(sum(RF_B_G4_no_Aspec$PRPC))/RF_sumarea_G4)

  RF_B_DIN_b_AG <<- c((sum(RF_B_G1_no_Aspec$DIN_b))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$DIN_b))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$DIN_b))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$DIN_b))/RF_sumvolume_G4)
  RF_B_DIP_b_AG <<- c((sum(RF_B_G1_no_Aspec$DIP_b))/RF_sumvolume_G1,(sum(RF_B_G2_no_Aspec$DIP_b))/RF_sumvolume_G2,(sum(RF_B_G3_no_Aspec$DIP_b))/RF_sumvolume_G3,(sum(RF_B_G4_no_Aspec$DIP_b))/RF_sumvolume_G4)



  RF_S1_EC_AG <<- c((sum(RF_S1_G1_no_Aspec$EC))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$EC))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$EC))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$EC))/RF_sumarea_G4)
  RF_S1_EN_AG <<- c((sum(RF_S1_G1_no_Aspec$EN))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$EN))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$EN))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$EN))/RF_sumarea_G4)
  RF_S1_EP_AG <<- c((sum(RF_S1_G1_no_Aspec$EP))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$EP))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$EP))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$EP))/RF_sumarea_G4)

  RF_S1_BC2_AG <<- c((sum(RF_S1_G1_no_Aspec$BC2))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BC2))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BC2))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BC2))/RF_sumarea_G4)
  RF_S1_BN2_AG <<- c((sum(RF_S1_G1_no_Aspec$BN2))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BN2))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BN2))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BN2))/RF_sumarea_G4)
  RF_S1_BP2_AG <<- c((sum(RF_S1_G1_no_Aspec$BP2))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BP2))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BP2))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BP2))/RF_sumarea_G4)

  RF_S1_BC1_AG <<- c((sum(RF_S1_G1_no_Aspec$BC1))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BC1))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BC1))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BC1))/RF_sumarea_G4)
  RF_S1_BN1_AG <<- c((sum(RF_S1_G1_no_Aspec$BN1))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BN1))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BN1))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BN1))/RF_sumarea_G4)
  RF_S1_BP1_AG <<- c((sum(RF_S1_G1_no_Aspec$BP1))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BP1))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BP1))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BP1))/RF_sumarea_G4)

  RF_S1_BDC_AG <<- c((sum(RF_S1_G1_no_Aspec$BDC))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BDC))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BDC))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BDC))/RF_sumarea_G4)
  RF_S1_BDN_AG <<- c((sum(RF_S1_G1_no_Aspec$BDN))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BDN))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BDN))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BDN))/RF_sumarea_G4)
  RF_S1_BDP_AG <<- c((sum(RF_S1_G1_no_Aspec$BDP))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$BDP))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$BDP))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$BDP))/RF_sumarea_G4)

  RF_S1_PC_AG <<- c((sum(RF_S1_G1_no_Aspec$PC))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$PC))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$PC))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$PC))/RF_sumvolume_G4)
  RF_S1_PN_AG <<- c((sum(RF_S1_G1_no_Aspec$PN))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$PN))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$PN))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$PN))/RF_sumvolume_G4)
  RF_S1_PP_AG <<- c((sum(RF_S1_G1_no_Aspec$PP))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$PP))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$PP))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$PP))/RF_sumvolume_G4)

  RF_S1_CH_AG <<- c((sum(RF_S1_G1_no_Aspec$CH))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$CH))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$CH))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$CH))/RF_sumvolume_G4)

  RF_S1_DIN_AG <<- c((sum(RF_S1_G1_no_Aspec$DIN))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$DIN))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$DIN))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$DIN))/RF_sumvolume_G4)
  RF_S1_DIP_AG <<- c((sum(RF_S1_G1_no_Aspec$DIP))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$DIP))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$DIP))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$DIP))/RF_sumvolume_G4)

  RF_S1_PREC_AG <<- c((sum(RF_S1_G1_no_Aspec$PREC))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$PREC))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$PREC))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$PREC))/RF_sumarea_G4)
  RF_S1_PRBC2_AG <<- c((sum(RF_S1_G1_no_Aspec$PRBC2))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$PRBC2))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$PRBC2))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$PRBC2))/RF_sumarea_G4)
  RF_S1_PRBC1_AG <<- c((sum(RF_S1_G1_no_Aspec$PRBC1))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$PRBC1))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$PRBC1))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$PRBC1))/RF_sumarea_G4)
  RF_S1_PRBDC_AG <<- c((sum(RF_S1_G1_no_Aspec$PRBDC))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$PRBDC))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$PRBDC))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$PRBDC))/RF_sumarea_G4)
  RF_S1_PRPC_AG <<- c((sum(RF_S1_G1_no_Aspec$PRPC))/RF_sumarea_G1,(sum(RF_S1_G2_no_Aspec$PRPC))/RF_sumarea_G2,(sum(RF_S1_G3_no_Aspec$PRPC))/RF_sumarea_G3,(sum(RF_S1_G4_no_Aspec$PRPC))/RF_sumarea_G4)

  RF_S1_DIN_b_AG <<- c((sum(RF_S1_G1_no_Aspec$DIN_b))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$DIN_b))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$DIN_b))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$DIN_b))/RF_sumvolume_G4)
  RF_S1_DIP_b_AG <<- c((sum(RF_S1_G1_no_Aspec$DIP_b))/RF_sumvolume_G1,(sum(RF_S1_G2_no_Aspec$DIP_b))/RF_sumvolume_G2,(sum(RF_S1_G3_no_Aspec$DIP_b))/RF_sumvolume_G3,(sum(RF_S1_G4_no_Aspec$DIP_b))/RF_sumvolume_G4)



  RF_S2_EC_AG <<- c((sum(RF_S2_G1_no_Aspec$EC))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$EC))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$EC))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$EC))/RF_sumarea_G4)
  RF_S2_EN_AG <<- c((sum(RF_S2_G1_no_Aspec$EN))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$EN))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$EN))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$EN))/RF_sumarea_G4)
  RF_S2_EP_AG <<- c((sum(RF_S2_G1_no_Aspec$EP))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$EP))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$EP))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$EP))/RF_sumarea_G4)

  RF_S2_BC2_AG <<- c((sum(RF_S2_G1_no_Aspec$BC2))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BC2))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BC2))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BC2))/RF_sumarea_G4)
  RF_S2_BN2_AG <<- c((sum(RF_S2_G1_no_Aspec$BN2))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BN2))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BN2))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BN2))/RF_sumarea_G4)
  RF_S2_BP2_AG <<- c((sum(RF_S2_G1_no_Aspec$BP2))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BP2))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BP2))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BP2))/RF_sumarea_G4)

  RF_S2_BC1_AG <<- c((sum(RF_S2_G1_no_Aspec$BC1))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BC1))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BC1))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BC1))/RF_sumarea_G4)
  RF_S2_BN1_AG <<- c((sum(RF_S2_G1_no_Aspec$BN1))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BN1))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BN1))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BN1))/RF_sumarea_G4)
  RF_S2_BP1_AG <<- c((sum(RF_S2_G1_no_Aspec$BP1))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BP1))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BP1))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BP1))/RF_sumarea_G4)

  RF_S2_BDC_AG <<- c((sum(RF_S2_G1_no_Aspec$BDC))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BDC))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BDC))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BDC))/RF_sumarea_G4)
  RF_S2_BDN_AG <<- c((sum(RF_S2_G1_no_Aspec$BDN))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BDN))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BDN))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BDN))/RF_sumarea_G4)
  RF_S2_BDP_AG <<- c((sum(RF_S2_G1_no_Aspec$BDP))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$BDP))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$BDP))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$BDP))/RF_sumarea_G4)

  RF_S2_PC_AG <<- c((sum(RF_S2_G1_no_Aspec$PC))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$PC))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$PC))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$PC))/RF_sumvolume_G4)
  RF_S2_PN_AG <<- c((sum(RF_S2_G1_no_Aspec$PN))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$PN))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$PN))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$PN))/RF_sumvolume_G4)
  RF_S2_PP_AG <<- c((sum(RF_S2_G1_no_Aspec$PP))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$PP))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$PP))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$PP))/RF_sumvolume_G4)

  RF_S2_CH_AG <<- c((sum(RF_S2_G1_no_Aspec$CH))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$CH))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$CH))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$CH))/RF_sumvolume_G4)

  RF_S2_DIN_AG <<- c((sum(RF_S2_G1_no_Aspec$DIN))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$DIN))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$DIN))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$DIN))/RF_sumvolume_G4)
  RF_S2_DIP_AG <<- c((sum(RF_S2_G1_no_Aspec$DIP))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$DIP))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$DIP))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$DIP))/RF_sumvolume_G4)

  RF_S2_PREC_AG <<- c((sum(RF_S2_G1_no_Aspec$PREC))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$PREC))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$PREC))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$PREC))/RF_sumarea_G4)
  RF_S2_PRBC2_AG <<- c((sum(RF_S2_G1_no_Aspec$PRBC2))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$PRBC2))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$PRBC2))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$PRBC2))/RF_sumarea_G4)
  RF_S2_PRBC1_AG <<- c((sum(RF_S2_G1_no_Aspec$PRBC1))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$PRBC1))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$PRBC1))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$PRBC1))/RF_sumarea_G4)
  RF_S2_PRBDC_AG <<- c((sum(RF_S2_G1_no_Aspec$PRBDC))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$PRBDC))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$PRBDC))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$PRBDC))/RF_sumarea_G4)
  RF_S2_PRPC_AG <<- c((sum(RF_S2_G1_no_Aspec$PRPC))/RF_sumarea_G1,(sum(RF_S2_G2_no_Aspec$PRPC))/RF_sumarea_G2,(sum(RF_S2_G3_no_Aspec$PRPC))/RF_sumarea_G3,(sum(RF_S2_G4_no_Aspec$PRPC))/RF_sumarea_G4)

  RF_S2_DIN_b_AG <<- c((sum(RF_S2_G1_no_Aspec$DIN_b))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$DIN_b))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$DIN_b))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$DIN_b))/RF_sumvolume_G4)
  RF_S2_DIP_b_AG <<- c((sum(RF_S2_G1_no_Aspec$DIP_b))/RF_sumvolume_G1,(sum(RF_S2_G2_no_Aspec$DIP_b))/RF_sumvolume_G2,(sum(RF_S2_G3_no_Aspec$DIP_b))/RF_sumvolume_G3,(sum(RF_S2_G4_no_Aspec$DIP_b))/RF_sumvolume_G4)


  #Create final data frame with single values for primary producers (biomass and production) for all gradient polygons and all model scenarios. Also adds column "scenario"

  RF_B_AG_ASSV <<- data.frame(RF_gradient_no,RF_B_avgDIN_AG,RF_B_avgDIP_AG,RF_B_EC_AG,RF_B_EN_AG,RF_B_EP_AG,RF_B_BC2_AG,RF_B_BN2_AG,RF_B_BP2_AG,RF_B_BC1_AG,RF_B_BN1_AG,RF_B_BP1_AG,RF_B_BDC_AG,RF_B_BDN_AG,RF_B_BDP_AG,RF_B_PC_AG,RF_B_PN_AG,RF_B_PP_AG,RF_B_CH_AG,RF_B_DIN_AG,RF_B_DIP_AG,RF_B_PREC_AG,RF_B_PRBC2_AG,RF_B_PRBC1_AG,RF_B_PRBDC_AG,RF_B_PRPC_AG,RF_B_DIN_b_AG,RF_B_DIP_b_AG) %>%
    add_column(scenario = "Baseline", .before = "RF_gradient_no")

  RF_S1_AG_ASSV <<- data.frame(RF_gradient_no,RF_S1_avgDIN_AG,RF_S1_avgDIP_AG,RF_S1_EC_AG,RF_S1_EN_AG,RF_S1_EP_AG,RF_S1_BC2_AG,RF_S1_BN2_AG,RF_S1_BP2_AG,RF_S1_BC1_AG,RF_S1_BN1_AG,RF_S1_BP1_AG,RF_S1_BDC_AG,RF_S1_BDN_AG,RF_S1_BDP_AG,RF_S1_PC_AG,RF_S1_PN_AG,RF_S1_PP_AG,RF_S1_CH_AG,RF_S1_DIN_AG,RF_S1_DIP_AG,RF_S1_PREC_AG,RF_S1_PRBC2_AG,RF_S1_PRBC1_AG,RF_S1_PRBDC_AG,RF_S1_PRPC_AG,RF_S1_DIN_b_AG,RF_S1_DIP_b_AG) %>%
    add_column(scenario = "N-30%", .before = "RF_gradient_no")

  RF_S2_AG_ASSV <<- data.frame(RF_gradient_no,RF_S2_avgDIN_AG,RF_S2_avgDIP_AG,RF_S2_EC_AG,RF_S2_EN_AG,RF_S2_EP_AG,RF_S2_BC2_AG,RF_S2_BN2_AG,RF_S2_BP2_AG,RF_S2_BC1_AG,RF_S2_BN1_AG,RF_S2_BP1_AG,RF_S2_BDC_AG,RF_S2_BDN_AG,RF_S2_BDP_AG,RF_S2_PC_AG,RF_S2_PN_AG,RF_S2_PP_AG,RF_S2_CH_AG,RF_S2_DIN_AG,RF_S2_DIP_AG,RF_S2_PREC_AG,RF_S2_PRBC2_AG,RF_S2_PRBC1_AG,RF_S2_PRBDC_AG,RF_S2_PRPC_AG,RF_S2_DIN_b_AG,RF_S2_DIP_b_AG) %>%
    add_column(scenario = "P-30%", .before = "RF_gradient_no")

  #Rename all column headers to enable merging of tables
  RF_B <<- RF_B_AG_ASSV %>%
    rename(

      AvgDIN = RF_B_avgDIN_AG,
      AvgDIP = RF_B_avgDIP_AG,

      EC = RF_B_EC_AG,
      EN = RF_B_EN_AG,
      EP = RF_B_EP_AG,

      BC2 = RF_B_BC2_AG,
      BN2 = RF_B_BN2_AG,
      BP2 = RF_B_BP2_AG,

      BC1 = RF_B_BC1_AG,
      BN1 = RF_B_BN1_AG,
      BP1 = RF_B_BP1_AG,

      BDC = RF_B_BDC_AG,
      BDN = RF_B_BDN_AG,
      BDP = RF_B_BDP_AG,

      PC = RF_B_PC_AG,
      PN = RF_B_PN_AG,
      PP = RF_B_PP_AG,

      CH = RF_B_CH_AG,

      DIN = RF_B_DIN_AG,
      DIP = RF_B_DIP_AG,

      PREC = RF_B_PREC_AG,
      PRBC2 = RF_B_PRBC2_AG,
      PRBC1 = RF_B_PRBC1_AG,
      PRBDC = RF_B_PRBDC_AG,
      PRPC = RF_B_PRPC_AG,
      DIN_b = RF_B_DIN_b_AG,
      DIP_b = RF_B_DIP_b_AG
    )

  RF_S1 <<- RF_S1_AG_ASSV %>%
    rename(

      AvgDIN = RF_S1_avgDIN_AG,
      AvgDIP = RF_S1_avgDIP_AG,

      EC = RF_S1_EC_AG,
      EN = RF_S1_EN_AG,
      EP = RF_S1_EP_AG,

      BC2 = RF_S1_BC2_AG,
      BN2 = RF_S1_BN2_AG,
      BP2 = RF_S1_BP2_AG,

      BC1 = RF_S1_BC1_AG,
      BN1 = RF_S1_BN1_AG,
      BP1 = RF_S1_BP1_AG,

      BDC = RF_S1_BDC_AG,
      BDN = RF_S1_BDN_AG,
      BDP = RF_S1_BDP_AG,

      PC = RF_S1_PC_AG,
      PN = RF_S1_PN_AG,
      PP = RF_S1_PP_AG,

      CH = RF_S1_CH_AG,

      DIN = RF_S1_DIN_AG,
      DIP = RF_S1_DIP_AG,

      PREC = RF_S1_PREC_AG,
      PRBC2 = RF_S1_PRBC2_AG,
      PRBC1 = RF_S1_PRBC1_AG,
      PRBDC = RF_S1_PRBDC_AG,
      PRPC = RF_S1_PRPC_AG,
      DIN_b = RF_S1_DIN_b_AG,
      DIP_b = RF_S1_DIP_b_AG
    )

  RF_S2 <<- RF_S2_AG_ASSV %>%
    rename(

      AvgDIN = RF_S2_avgDIN_AG,
      AvgDIP = RF_S2_avgDIP_AG,

      EC = RF_S2_EC_AG,
      EN = RF_S2_EN_AG,
      EP = RF_S2_EP_AG,

      BC2 = RF_S2_BC2_AG,
      BN2 = RF_S2_BN2_AG,
      BP2 = RF_S2_BP2_AG,

      BC1 = RF_S2_BC1_AG,
      BN1 = RF_S2_BN1_AG,
      BP1 = RF_S2_BP1_AG,

      BDC = RF_S2_BDC_AG,
      BDN = RF_S2_BDN_AG,
      BDP = RF_S2_BDP_AG,

      PC = RF_S2_PC_AG,
      PN = RF_S2_PN_AG,
      PP = RF_S2_PP_AG,

      CH = RF_S2_CH_AG,

      DIN = RF_S2_DIN_AG,
      DIP = RF_S2_DIP_AG,

      PREC = RF_S2_PREC_AG,
      PRBC2 = RF_S2_PRBC2_AG,
      PRBC1 = RF_S2_PRBC1_AG,
      PRBDC = RF_S2_PRBDC_AG,
      PRPC = RF_S2_PRPC_AG,
      DIN_b = RF_S2_DIN_b_AG,
      DIP_b = RF_S2_DIP_b_AG
    )

  #Merging tables
  RF_AS_AG_FINAL <<- bind_rows(RF_B,RF_S1,RF_S2)


  #Install and load GGplot2, gridExtra and ggpubr packages
  if(!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)
  if(!require(gridExtra)) install.packages("gridExtra")
  library(gridExtra)
  if(!require(ggpubr)) install.packages("ggpubr")
  library(ggpubr)
  if(!require(devtools)) install.packages("devtools")
  library(devtools)
  if(!require(patchwork)) install.packages("patchwork")
  library(patchwork)

  #Production VS. AvgDIN plots
  RF_PREC_vs_AvgDIN <<- ggplot(RF_AS_AG_FINAL, aes(x = AvgDIN, y = PREC, group = scenario)) +
    geom_line(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    xlab('Avg. DIN ('*'g C'~m^-2*')') +
    ylab('Eelgrass production ('*' g C'~m^-2~GS^-1*')') +
    scale_x_continuous(limits = c(0,0.45), breaks = seq(0,0.45, by = 0.1), labels = scales::number_format(accuracy = 0.05)) +
    scale_y_continuous(limits = c(0,25), breaks = seq(0,25, by = 5), labels = scales::number_format(accuracy = 1))


  RF_panel_PREC_vs_AvgDIN <<- RF_PREC_vs_AvgDIN + theme_pub()


  RF_PRBC1_vs_AvgDIN <<- ggplot(RF_AS_AG_FINAL, aes(x = AvgDIN, y = PRBC1, group = scenario)) +
    geom_line(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    xlab('Avg. DIN ('*'g C'~m^-2*')') +
    ylab('Opp. macroalgae production ('*' g C'~m^-2~GS^-1*')') +
    scale_x_continuous(limits = c(0,0.45), breaks = seq(0,0.45, by = 0.1), labels = scales::number_format(accuracy = 0.05)) +
    scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5), labels = scales::number_format(accuracy = 1))

  RF_panel_PRBC1_vs_AvgDIN <<- RF_PRBC1_vs_AvgDIN + theme_pub()



  RF_PRPC_vs_AvgDIN <<- ggplot(RF_AS_AG_FINAL, aes(x = AvgDIN, y = PRPC, group = scenario)) +
    geom_line(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    xlab('Avg. DIN ('*'g C'~m^-2*')') +
    ylab('Phytoplankton production ('*'  g C'~m^-2~GS^-1*')') +
    scale_x_continuous(limits = c(0,0.45), breaks = seq(0,0.45, by = 0.1), labels = scales::number_format(accuracy = 0.05)) +
    scale_y_continuous(limits = c(0,70), breaks = seq(0,70, by = 10), labels = scales::number_format(accuracy = 1))

  RF_panel_PRPC_vs_AvgDIN <<- RF_PRPC_vs_AvgDIN + theme_pub()


  RF_panel_PREC_vs_AvgDIN + labs(x = '', y = expression('Eelgrass production'~(g~C*~m^-2~GS^-1))) +
    RF_panel_PRBC1_vs_AvgDIN + labs(x = '', y = expression('Opp. macroalgae production'~(g~C*~m^-2~GS^-1))) +
    RF_panel_PRPC_vs_AvgDIN +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = 'A') +
    ggsave(filename = 'RF_PRXX_vs_AvgDIN_panel.tiff',

           width = 15,
           height = 35,
           units = 'cm',
           device='tiff',
           dpi=300)

  #Vertical profiles of primary producers and benthic light for all gradient polygons for each scenario

  RF_B_G1 <<- add_column(RF_B_G1, scenario = 'Baseline', RF_gradient_no = 'no. 1', .before = 'upper')
  RF_B_G2 <<- add_column(RF_B_G2, scenario = 'Baseline', RF_gradient_no = 'no. 2', .before = 'upper')
  RF_B_G3 <<- add_column(RF_B_G3, scenario = 'Baseline', RF_gradient_no = 'no. 3', .before = 'upper')
  RF_B_G4 <<- add_column(RF_B_G4, scenario = 'Baseline', RF_gradient_no = 'no. 4', .before = 'upper')

  RF_B_AG <<- bind_rows(RF_B_G1, RF_B_G2, RF_B_G3, RF_B_G4)


  RF_S1_G1 <<- add_column(RF_S1_G1, scenario = 'N-30%', RF_gradient_no = 'no. 1', .before = 'upper')
  RF_S1_G2 <<- add_column(RF_S1_G2, scenario = 'N-30%', RF_gradient_no = 'no. 2', .before = 'upper')
  RF_S1_G3 <<- add_column(RF_S1_G3, scenario = 'N-30%', RF_gradient_no = 'no. 3', .before = 'upper')
  RF_S1_G4 <<- add_column(RF_S1_G4, scenario = 'N-30%', RF_gradient_no = 'no. 4', .before = 'upper')

  RF_S1_AG <<- bind_rows(RF_S1_G1, RF_S1_G2, RF_S1_G3, RF_S1_G4)


  RF_S2_G1 <<- add_column(RF_S2_G1, scenario = 'P-30%', RF_gradient_no = 'no. 1', .before = 'upper')
  RF_S2_G2 <<- add_column(RF_S2_G2, scenario = 'P-30%', RF_gradient_no = 'no. 2', .before = 'upper')
  RF_S2_G3 <<- add_column(RF_S2_G3, scenario = 'P-30%', RF_gradient_no = 'no. 3', .before = 'upper')
  RF_S2_G4 <<- add_column(RF_S2_G4, scenario = 'P-30%', RF_gradient_no = 'no. 4', .before = 'upper')

  RF_S2_AG <<- bind_rows(RF_S2_G1, RF_S2_G2, RF_S2_G3, RF_S2_G4)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_B VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_B_PREC_vs_lower <<- ggplot(RF_B_AG, aes(x = PREC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Eelgrass production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 2), labels = scales::number_format(accuracy = 1))

  RF_B_PREC_vs_lower + theme_pub()


  RF_B_PRBC1_vs_lower <<- ggplot(RF_B_AG, aes(x = PRBC1, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Opp. macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_B_PRBC1_vs_lower + theme_pub()


  RF_B_PRBC2_vs_lower <<- ggplot(RF_B_AG, aes(x = PRBC2, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Perennial macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_B_PRBC2_vs_lower + theme_pub()


  RF_B_PRBDC_vs_lower <<- ggplot(RF_B_AG, aes(x = PRBDC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic diatom Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,50), breaks = seq(0,50, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_B_PRBDC_vs_lower + theme_pub()


  RF_B_PC_vs_lower <<- ggplot(RF_B_AG, aes(x = PC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_B_PC_vs_lower + theme_pub()


  RF_B_lo_b_vs_lower <<- ggplot(RF_B_AG, aes(x = lo_b, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,650), breaks = seq(0,650, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_B_lo_b_vs_lower + theme_pub()


  RF_B_PREC_vs_lower+ theme_pub() + labs(x = expression('Eelgrass production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1.1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_B_PRBC1_vs_lower+ theme_pub() + labs(x = expression('Opp. macroalgae production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_B_PRBC2_vs_lower+ theme_pub() + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_B_PRBDC_vs_lower+ theme_pub() + labs(x = expression('Benthic diatom production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_B_PC_vs_lower+ theme_pub() + labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_B_lo_b_vs_lower + theme_pub() + labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = 'A') +
    ggsave(filename = 'RF_B_AG_production.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)


  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_S1 VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_S1_PREC_vs_lower <<- ggplot(RF_S1_AG, aes(x = PREC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Eelgrass Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 2), labels = scales::number_format(accuracy = 1))

  RF_S1_PREC_vs_lower + theme_pub()


  RF_S1_PRBC1_vs_lower <<- ggplot(RF_S1_AG, aes(x = PRBC1, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Opp. macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S1_PRBC1_vs_lower + theme_pub()


  RF_S1_PRBC2_vs_lower <<- ggplot(RF_S1_AG, aes(x = PRBC2, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Perennial macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S1_PRBC2_vs_lower + theme_pub()


  RF_S1_PRBDC_vs_lower <<- ggplot(RF_S1_AG, aes(x = PRBDC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic diatom Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,50), breaks = seq(0,50, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S1_PRBDC_vs_lower + theme_pub()


  RF_S1_PC_vs_lower <<- ggplot(RF_S1_AG, aes(x = PC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_S1_PC_vs_lower + theme_pub()


  RF_S1_lo_b_vs_lower <<- ggplot(RF_S1_AG, aes(x = lo_b, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,650), breaks = seq(0,650, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_S1_lo_b_vs_lower + theme_pub()


  RF_S1_PREC_vs_lower+ theme_pub() + labs(x = expression('Eelgrass production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1.1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S1_PRBC1_vs_lower+ theme_pub() + labs(x = expression('Opp. macroalgae production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S1_PRBC2_vs_lower+ theme_pub() + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S1_PRBDC_vs_lower+ theme_pub() + labs(x = expression('Benthic diatom production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S1_PC_vs_lower+ theme_pub() + labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S1_lo_b_vs_lower + theme_pub() + labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = 'A') +
    ggsave(filename = 'RF_S1_AG_production.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_S2 VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_S2_PREC_vs_lower <<- ggplot(RF_S2_AG, aes(x = PREC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Eelgrass Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 2), labels = scales::number_format(accuracy = 1))

  RF_S2_PREC_vs_lower + theme_pub()


  RF_S2_PRBC1_vs_lower <<- ggplot(RF_S2_AG, aes(x = PRBC1, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Opp. macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S2_PRBC1_vs_lower + theme_pub()


  RF_S2_PRBC2_vs_lower <<- ggplot(RF_S2_AG, aes(x = PRBC2, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Perennial macroalgae Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S2_PRBC2_vs_lower + theme_pub()


  RF_S2_PRBDC_vs_lower <<- ggplot(RF_S2_AG, aes(x = PRBDC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic diatom Production'~(g~C*~m^-2~GS^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,50), breaks = seq(0,50, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_S2_PRBDC_vs_lower + theme_pub()


  RF_S2_PC_vs_lower <<- ggplot(RF_S2_AG, aes(x = PC, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_S2_PC_vs_lower + theme_pub()


  RF_S2_lo_b_vs_lower <<- ggplot(RF_S2_AG, aes(x = lo_b, y = lower, group = RF_gradient_no)) +
    geom_path(size = 1) +
    geom_point(aes(shape = RF_gradient_no, color = RF_gradient_no, fill = RF_gradient_no), size = 3) +
    scale_shape_manual(values = c(21, 24, 22, 22)) +
    scale_color_manual(values = c('black', 'black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white')) +
    labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = expression('Depth'~(m)))+
    scale_x_continuous(limits = c(0,650), breaks = seq(0,650, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_S2_lo_b_vs_lower + theme_pub()


  RF_S2_PREC_vs_lower+ theme_pub() + labs(x = expression('Eelgrass production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1.1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S2_PRBC1_vs_lower+ theme_pub() + labs(x = expression('Opp. macroalgae production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S2_PRBC2_vs_lower+ theme_pub() + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S2_PRBDC_vs_lower+ theme_pub() + labs(x = expression('Benthic diatom production'~(g~C*~m^-2~GS^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S2_PC_vs_lower+ theme_pub() + labs(x = expression('Phytoplankton biomass'~(g~C*~m^-3)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_S2_lo_b_vs_lower + theme_pub() + labs(x = expression('Benthic light'~(mu*E*~m^-2*~s^-1)), y = '') + theme(legend.justification = c(1,2.15), plot.tag.position = c(0.952,0.77), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = 'A') +
    ggsave(filename = 'RF_S2_AG_production.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_PREC VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_G1_AS <<- bind_rows(RF_B_G1, RF_S1_G1, RF_S2_G1)
  RF_G2_AS <<- bind_rows(RF_B_G2, RF_S1_G2, RF_S2_G2)
  RF_G3_AS <<- bind_rows(RF_B_G3, RF_S1_G3, RF_S2_G3)
  RF_G4_AS <<- bind_rows(RF_B_G4, RF_S1_G4, RF_S2_G4)


  RF_AS_G1_PREC <<- ggplot(RF_G1_AS, aes(x = PREC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Eelgrass production'~(g~C*~m^-2*~GS^-1)), y = '') +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 1), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_PREC + theme_pub()

  RF_AS_G2_PREC <<- ggplot(RF_G2_AS, aes(x = PREC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 1), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_PREC + theme_pub()

  RF_AS_G3_PREC <<- ggplot(RF_G3_AS, aes(x = PREC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 1), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_PREC + theme_pub()

  RF_AS_G4_PREC <<- ggplot(RF_G4_AS, aes(x = PREC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 20), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-6,0), breaks = seq(-6,0, by = 1), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_PREC + theme_pub()


  RF_AS_G1_PREC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_PREC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_PREC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_PREC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_PREC_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_PRBC1 VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_AS_G1_PRBC1 <<- ggplot(RF_G1_AS, aes(x = PRBC1, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Opp. macroalgae production'~(g~C*~m^-2*~GS^-1)), y = '') +
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_PRBC1 + theme_pub()

  RF_AS_G2_PRBC1 <<- ggplot(RF_G2_AS, aes(x = PRBC1, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_PRBC1 + theme_pub()

  RF_AS_G3_PRBC1 <<- ggplot(RF_G3_AS, aes(x = PRBC1, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_PRBC1 + theme_pub()

  RF_AS_G4_PRBC1 <<- ggplot(RF_G4_AS, aes(x = PRBC1, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,45), breaks = seq(0,45, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_PRBC1 + theme_pub()


  RF_AS_G1_PRBC1+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_PRBC1+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_PRBC1+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_PRBC1+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_PRBC1_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_PRBC2 VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_AS_G1_PRBC2 <<- ggplot(RF_G1_AS, aes(x = PRBC2, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Perennial macroalgae production'~(g~C*~m^-2*~GS^-1)), y = '') +
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_PRBC2 + theme_pub()

  RF_AS_G2_PRBC2 <<- ggplot(RF_G2_AS, aes(x = PRBC2, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_PRBC2 + theme_pub()

  RF_AS_G3_PRBC2 <<- ggplot(RF_G3_AS, aes(x = PRBC2, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_PRBC2 + theme_pub()

  RF_AS_G4_PRBC2 <<- ggplot(RF_G4_AS, aes(x = PRBC2, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,18), breaks = seq(0,18, by = 3), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-8,0), breaks = seq(-8,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_PRBC2 + theme_pub()


  RF_AS_G1_PRBC2+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_PRBC2+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_PRBC2+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_PRBC2+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_PRBC2_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_PRBDC VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_AS_G1_PRBDC <<- ggplot(RF_G1_AS, aes(x = PRBDC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Benthic diatom production'~(g~C*~m^-2*~GS^-1)), y = '') +
    scale_x_continuous(limits = c(0,55), breaks = seq(0,55, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_PRBDC + theme_pub()

  RF_AS_G2_PRBDC <<- ggplot(RF_G2_AS, aes(x = PRBDC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,55), breaks = seq(0,55, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_PRBDC + theme_pub()

  RF_AS_G3_PRBDC <<- ggplot(RF_G3_AS, aes(x = PRBDC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,55), breaks = seq(0,55, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_PRBDC + theme_pub()

  RF_AS_G4_PRBDC <<- ggplot(RF_G4_AS, aes(x = PRBDC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,55), breaks = seq(0,55, by = 5), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-10,0), breaks = seq(-10,0, by = 2), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_PRBDC + theme_pub()


  RF_AS_G1_PRBDC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_PRBDC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_PRBDC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_PRBDC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_PRBDC_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_PC VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_AS_G1_PC <<- ggplot(RF_G1_AS, aes(x = PC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Phytoplakton biomass'~(g~C*~m^-3)), y = '') +
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_PC + theme_pub()

  RF_AS_G2_PC <<- ggplot(RF_G2_AS, aes(x = PC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_PC + theme_pub()

  RF_AS_G3_PC <<- ggplot(RF_G3_AS, aes(x = PC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_PC + theme_pub()

  RF_AS_G4_PC <<- ggplot(RF_G4_AS, aes(x = PC, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,2), breaks = seq(0,2, by = 0.3), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_PC + theme_pub()


  RF_AS_G1_PC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_PC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_PC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_PC+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_PC_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH RF_ASAG_lo_b VERTICAL PROFILES HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_AS_G1_lo_b <<- ggplot(RF_G1_AS, aes(x = lo_b, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = expression('                                                                                         Benthic light'~(mu*E*~m^-2*~s^-1)), y = '') +
    scale_x_continuous(limits = c(0,700), breaks = seq(0,700, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G1_lo_b + theme_pub()

  RF_AS_G2_lo_b <<- ggplot(RF_G2_AS, aes(x = lo_b, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,700), breaks = seq(0,700, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G2_lo_b + theme_pub()

  RF_AS_G3_lo_b <<- ggplot(RF_G3_AS, aes(x = lo_b, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = expression('                                                                                           Depth'~(m))) +
    scale_x_continuous(limits = c(0,700), breaks = seq(0,700, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G3_lo_b + theme_pub()

  RF_AS_G4_lo_b <<- ggplot(RF_G4_AS, aes(x = lo_b, y = lower, group = scenario)) +
    geom_path(size = 1) +
    geom_point(aes(shape = scenario, color = scenario, fill = scenario), size = 3) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = '', y = '') +
    scale_x_continuous(limits = c(0,700), breaks = seq(0,700, by = 100), labels = scales::number_format(accuracy = 1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 3), labels = scales::number_format(accuracy = 1))


  RF_AS_G4_lo_b + theme_pub()


  RF_AS_G1_lo_b+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G2_lo_b+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G3_lo_b+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    RF_AS_G4_lo_b+ theme_pub() + theme(legend.justification = c(1,4.45), plot.tag.position = c(0.93,0.82), plot.tag = element_text(size = 22), axis.title = element_text(size = 18)) +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = '1', tag_prefix = 'no. ') +
    ggsave(filename = 'RF_ASAG_lo_b_vs_lower.tiff',

           width = 30,
           height = 30,
           units = 'cm',
           device='tiff',
           dpi=300)

  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH   Benthic Vs. WC production   HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  RF_benthic_vs_WC_prod <<- RF_AS_AG_FINAL %>%
    select(scenario, RF_gradient_no, PREC, PRBC1, PRBC2, PRBDC, PRPC) %>%
    mutate(benthic_prod = PREC + PRBC1 + PRBC2 + PRBDC,
           WC_prod = PRPC,
           prod_100_pct = benthic_prod + WC_prod,
           benthic_prod_pct = benthic_prod / prod_100_pct * 100,
           WC_prod_pct = WC_prod / prod_100_pct *100,
           control = benthic_prod_pct + WC_prod_pct
    ) %>%
    rename('Benthic prod.' = benthic_prod_pct,
           'WC prod.' = WC_prod_pct) %>%
    select(scenario, RF_gradient_no, 'Benthic prod.', 'WC prod.') %>%
    gather(prod_ID, prod_pct, 'Benthic prod.':'WC prod.')


  RF_benthic_vs_WC_prod_plot <<- ggplot(data = RF_benthic_vs_WC_prod, aes(x = RF_gradient_no, y = prod_pct, group = interaction(scenario, prod_ID))) +
    geom_path(size = 1, aes(linetype = prod_ID)) +
    geom_point(size = 3, aes(shape = scenario, color = scenario, fill = scenario)) +
    scale_shape_manual(values = c(22, 22, 24)) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    scale_fill_manual(values = c('black', 'white', 'black')) +
    labs(x = 'Distance (km)', y = 'Relative growth season production\n(% of total production)') +
    scale_x_discrete(limits = c('0-6', '6-8', '8-11', '11-14'), expand = c(0,0.05)) +
    scale_y_continuous(limits = c(0,105), breaks = seq(0,105, by = 10), labels = scales::number_format(accuracy = 1), expand = c(0,0.05))

  RF_benthic_vs_WC_prod_plot + theme_pub() +
    theme(
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.justification = c(1.3,2),
      legend.direction = 'horizontal',
      legend.spacing.x = unit(0.05, 'cm'),
      axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
      plot.margin = unit(c(5,50,0,0), "pt"),
    ) +
    ggsave(filename = 'RF_Benthic_vs_WC_prod.tiff',

           width = 30,
           height = 20,
           units = 'cm',
           device='tiff',
           dpi=300)


  #HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH   Monod tabels and monod vertical profiles   HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

  EC_vmax_C <<- 0.04; EC_kmDIN_C <<- 0.09; EC_kmDIP_C <<- 0.005; EC_kmlight_C <<- 355

  BC1_vmax_C <<- 0.23; BC1_kmDIN_C <<- 0.06; BC1_kmDIP_C <<- 0.015; BC1_kmlight_C <<- 200

  BC2_vmax_C <<- 0.09; BC2_kmDIN_C <<- 0.02; BC2_kmDIP_C <<- 0.01; BC2_kmlight_C <<- 170

  BDC_vmax_C <<- 1; BDC_kmDIN_C <<- 0.025; BDC_kmDIP_C <<- 0.005; BDC_kmlight_C <<- 30

  PC_vmax_C <<- 2; PC_kmDIN_C <<- 0.03; PC_kmDIP_C <<- 0.005; PC_kmlight_C <<- 75

  RF_monod_table <<- bind_rows(RF_B_AG, RF_S1_AG, RF_S2_AG) %>%
    select(scenario, RF_gradient_no, lower, EC, EN, EP, BC1, BN1, BP1, BC2, BN2, BP2, BDC, BDN, BDP, PC, PN, PP, DIN, DIP, lo_b, DIN_b, DIP_b) %>%
    rename(
      EC_kmDIN = EC,
      EC_kmDIP = EN,
      EC_kmlight = EP,

      BC1_kmDIN = BC1,
      BC1_kmDIP = BN1,
      BC1_kmlight = BP1,

      BC2_kmDIN = BC2,
      BC2_kmDIP = BN2,
      BC2_kmlight = BP2,

      BDC_kmDIN = BDC,
      BDC_kmDIP = BDN,
      BDC_kmlight = BDP,

      PC_kmDIN = PC,
      PC_kmDIP = PN,
      PC_kmlight = PP
    ) %>%
    mutate(
      EC_kmDIN = DIN_b / (DIN_b + EC_kmDIN_C),
      EC_kmDIP = DIP_b / (DIP_b+EC_kmDIP_C),
      EC_kmlight = lo_b / (lo_b + EC_kmlight_C),

      BC1_kmDIN = DIN_b / (DIN_b + BC1_kmDIN_C),
      BC1_kmDIP = DIP_b / (DIP_b+BC1_kmDIP_C),
      BC1_kmlight = lo_b / (lo_b + BC1_kmlight_C),

      BC2_kmDIN = DIN_b / (DIN_b + BC2_kmDIN_C),
      BC2_kmDIP = DIP_b / (DIP_b+BC2_kmDIP_C),
      BC2_kmlight = lo_b / (lo_b + BC2_kmlight_C),

      BDC_kmDIN = DIN_b / (DIN_b + BDC_kmDIN_C),
      BDC_kmDIP = DIP_b / (DIP_b+BDC_kmDIP_C),
      BDC_kmlight = lo_b / (lo_b + BDC_kmlight_C),

      PC_kmDIN = DIN / (DIN + PC_kmDIN_C),
      PC_kmDIP = DIP / (DIP+PC_kmDIP_C),
      PC_kmlight = lo_b / (lo_b + PC_kmlight_C)
    )


  #Monod EC
  RF_monod_EC_light <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = EC_kmlight, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.5, aes(color = scenario)) +
    geom_point(size = 3, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,0.65), breaks = seq(0,0.65, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Eelgrass growth limitation by benthic light\n', y = 'Depth (m)')

  RF_monod_EC_light + theme_pub() +
    theme(
      plot.margin = unit(c(1,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical' ,
      legend.text = element_text(size = 22)
    )

  RF_monod_EC_light + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 22), axis.text = element_text(size = 18), legend.justification = c(1.05,6),
                                          legend.direction = 'vertical',
                                          legend.box = 'vertical',
                                          legend.text = element_text(size = 22),
                                          legend.spacing.x = unit(0.05, 'cm'),) +
    plot_layout(ncol = 1) +
    ggsave(filename = 'RF_monod_EC_light.tiff',

           width = 30,
           height = 40,
           units = 'cm',
           device='tiff',
           dpi=300)

  #Monod BC1
  RF_monod_BC1_DIN <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC1_kmDIN, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1.1, by = 0.2), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Opp. macroalgae growth inhibition by DIN\n', y = '')

  RF_monod_BC1_DIN + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical',
    )

  RF_monod_BC1_DIP <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC1_kmDIP, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0.50,1.09), breaks = seq(0.50,1.09, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Opp. macroalgae growth inhibition by DIP\n', y = 'Depth (m)')

  RF_monod_BC1_DIP + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_BC1_light <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC1_kmlight, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,0.8), breaks = seq(0,0.8, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Opp. macroalgae growth inhibition by benthic light\n', y = '')

  RF_monod_BC1_light + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_BC1_DIN + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                         legend.direction = 'vertical',
                                         legend.box = 'vertical') +
    RF_monod_BC1_DIP + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                           legend.direction = 'vertical',
                                           legend.box = 'vertical') +
    RF_monod_BC1_light + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                             legend.direction = 'vertical',
                                             legend.box = 'vertical') +
    plot_layout(ncol = 1) +
    ggsave(filename = 'RF_monod_BC1.tiff',

           width = 20,
           height = 40,
           units = 'cm',
           device='tiff',
           dpi=300)


  #Monod PC
  RF_monod_PC_DIN <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = PC_kmDIN, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1.1, by = 0.2), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Phytoplankton growth inhibition by DIN\n', y = '')

  RF_monod_PC_DIN + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical',
    )

  RF_monod_PC_DIP <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = PC_kmDIP, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0.75,1), breaks = seq(0.75,1, by = 0.05), labels = scales::number_format(accuracy = 0.01), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Phytoplankton growth inhibition by DIP\n', y = 'Depth (m)')

  RF_monod_PC_DIP + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_PC_light <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = PC_kmlight, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,0.9), breaks = seq(0,0.9, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Phytoplankton growth inhibition by benthic light\n', y = '')

  RF_monod_PC_light + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_PC_DIN + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                        legend.direction = 'vertical',
                                        legend.box = 'vertical') +
    RF_monod_PC_DIP + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(5.5,1.75),
                                          legend.direction = 'vertical',
                                          legend.box = 'vertical') +
    RF_monod_PC_light + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                            legend.direction = 'vertical',
                                            legend.box = 'vertical') +
    plot_layout(ncol = 1) +
    ggsave(filename = 'RF_monod_PC.tiff',

           width = 20,
           height = 40,
           units = 'cm',
           device='tiff',
           dpi=300)


  #Monod BC2
  RF_monod_BC2_DIN <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC2_kmDIN, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0.15,1), breaks = seq(0.15,1, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Perennial macroalgae growth inhibition by DIN\n', y = '')

  RF_monod_BC2_DIN + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical',
    )

  RF_monod_BC2_DIP <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC2_kmDIP, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0.5,1), breaks = seq(0.5,1, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Perennial macroalgae growth inhibition by DIP\n', y = 'Depth (m)')

  RF_monod_BC2_DIP + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_BC2_light <<- ggplot(subset(RF_monod_table, scenario %in% c('Baseline', 'N-30%')), aes(x = BC2_kmlight, y = lower, group = interaction(scenario, RF_gradient_no))) +
    geom_path(size = 1.2, aes(color = scenario)) +
    geom_point(size = 2, aes(shape = RF_gradient_no, fill = RF_gradient_no)) +
    scale_shape_manual(values = c(21, 24, 22, 22, 24)) +
    scale_color_manual(values = c('black', 'orange', 'black')) +
    scale_fill_manual(values = c('black', 'black', 'black', 'white', 'white')) +
    scale_x_continuous(limits = c(0,0.8), breaks = seq(0,0.8, by = 0.1), labels = scales::number_format(accuracy = 0.1), position = 'top') +
    scale_y_continuous(limits = c(-16,0), breaks = seq(-16,0, by = 2), labels = scales::number_format(accuracy = 1)) +
    labs(x = 'Perennial macroalgae growth inhibition by benthic light\n', y = '')

  RF_monod_BC2_light + theme_pub() +
    theme(
      plot.margin = unit(c(20,1,1,1), "pt"),
      legend.justification = c(1,1.6),
      legend.direction = 'vertical',
      legend.box = 'vertical'
    )

  RF_monod_BC2_DIN + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(5.5,1.75),
                                         legend.direction = 'vertical',
                                         legend.box = 'vertical') +
    RF_monod_BC2_DIP + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(5.5,1.75),
                                           legend.direction = 'vertical',
                                           legend.box = 'vertical') +
    RF_monod_BC2_light + theme_pub() + theme(plot.margin = unit(c(15,1,1,1), "pt"), axis.title = element_text(size = 18), legend.justification = c(1,1.75),
                                             legend.direction = 'vertical',
                                             legend.box = 'vertical') +
    plot_layout(ncol = 1) +
    ggsave(filename = 'RF_monod_BC2.tiff',

           width = 20,
           height = 40,
           units = 'cm',
           device='tiff',
           dpi=300)


}


