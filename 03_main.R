source('01_expo.R')
source('02_outc.R')

opeck <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
)
