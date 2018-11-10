# Fetch Forecast Data
if(checkNewFcst("HRDPS")$fetch){fetchHRDPS()}
if(checkNewFcst("GEMRegional")$fetch){fetchGEMRegional()}
if(checkNewFcst("GEMGlobal")$fetch){fetchGEMGlobal()}





