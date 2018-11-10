# Relative Humidity
calc.relhum=function(tmp,dpt)
{
  ac=dpt
  bc=tmp
  c=6.11*10^(7.5*ac/(237.7+ac))
  d=6.11*10^(7.5*bc/(237.7+bc))
  return(c/d*100)
}