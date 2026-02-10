source(here::here('opeck','01_expo.R'))
source(here::here('opeck','02_outc.R'))

opeck <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
)

# 03_00 LONGITUDINAL ANALYSES --------------------------------------------------
surv

fit_00_00 <- coxph(
  
)