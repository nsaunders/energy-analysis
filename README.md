# energy-usage

### How to use
1. Set `solarOutput` in `src/Main.purs` to the kW output of the solar system being considered.
2. Place usage data downloaded from APS account in `usage.csv`. See `usage.csv.example` for the required format.
3. Run `spago run && open output.html` to view the report.

### Assumptions
1. System generates no power before (sunrise + 1 hour).
2. System generates no power after (sunset - 1 hour).
3. System generates `solarOutput` kW consistently throughout the day otherwise.
4. [APS Saver Choice Max plan](https://www.aps.com/en/Residential/Service-Plans/Compare-Service-Plans/Saver-Choice-Max)
5. Located in Phoenix.

### References
Holidays: [APS Saver Choice](https://www.aps.com/en/Residential/Service-Plans/Compare-Service-Plans/Saver-Choice)
Rates: [APS Saver Choice Max plan](https://www.aps.com/en/Residential/Service-Plans/Compare-Service-Plans/Saver-Choice-Max)
