# 999 - Provide user instructions
cat("\014") # Clear the Console
cat("[Welcome to the Snow-Reports.ca Weather Interface]\r\n")
cat("* Weather data provided by Environment Canada. (http://weather.gc.ca/canada_e.html)\r\n")
cat("* Please use the fetch functions provided to get the latest forecast data.\r\n")
cat("* Latest forecasts available locally are as follows:\r\n")
suppressMessages(cat(paste0("  * Global 25km GEM:   ",format(suppressMessages(checkNewFcst(fcst="GEMGlobal"))$current,format="%Y-%m-%d %H:%M %Z"),if(!checkNewFcst("GEMGlobal")$fetch){" (current)"},"\r\n")))
suppressMessages(cat(paste0("  * Regional 10km GEM: ",format(suppressMessages(checkNewFcst(fcst="GEMRegional"))$current,format="%Y-%m-%d %H:%M %Z"),if(!checkNewFcst("GEMRegional")$fetch){" (current)"},"\r\n")))
suppressMessages(cat(paste0("  * HRDPS West 2.5km:  ",format(suppressMessages(checkNewFcst(fcst="HRDPS"))$current,format="%Y-%m-%d %H:%M %Z"),if(!checkNewFcst("HRDPS")$fetch){" (current)"},"\r\n")))
cat("* This software is property of Maunakea Design Inc. for use by authorized users of Snow-Reports.ca only.\r\n")


