modelPower <- function(pc=NULL, pa=NULL, N=NULL, alpha=0.05, power=NULL, f2=NULL, peta2=NULL, dR2=NULL, R2=NULL)
{
  #SET UP EFFECT SIZE##################
  nEffs = 0
  if (!is.null(f2))
  {
    EffTxt = sprintf('f2 =    %.3f\n', f2)
    nEffs = nEffs+1
  }
  if(!is.null(peta2))
  {
    EffTxt = sprintf('pEta2 = %.3f\n', peta2)
    f2 = peta2/(1-peta2)
    nEffs = nEffs+1
  }
  if(!is.null(dR2) & !is.null(R2))
  {
    EffTxt = sprintf('dR2 =   %.3f; R2 =  %.3f\n', dR2,R2)
    f2 = dR2/(1-R2)
    nEffs = nEffs+1
  }
  if (nEffs!=1)
  {
    stop('Must specify either f2, peta2, or both dR2 and R2')
  }

  #SET UP U and V if needed##############
  u = pa-pc
  if(!is.null(N))
  {
    v = N-pa
  }
  else
  {
    v=NULL
  }
  
  #CONDUCT POWER ANALYSIS###########################
  results=pwr.f2.test(u=u, v=v, f2=f2 , sig.level = alpha, power=power)
  
  
  cat('Results from Power Analysis\n\n')
  cat(EffTxt)
  cat(sprintf('pa =     %i \n', pa))
  cat(sprintf('pc =     %i \n', pc))
  cat(sprintf('alpha = %.3f \n\n', results$sig.level))
  
  cat(sprintf('N = %.3f \n', results$v+pa))
  cat(sprintf('power = %.3f \n', results$power))
} 