# Analyze hourly Haka and Mahela temperature/humidity files and rainfall records; normalize the different CSV/Excel formats, flag and exclude implausible temperature readings, then write cleaned datasets, seasonal/monthly summaries, plots, and an HTML report to the analysis folder.
suppressPackageStartupMessages({library(readr); library(dplyr); library(ggplot2); library(readxl); library(lubridate); library(stringr)})
data_dir <- normalizePath('./data/Cheyenne_ppt_temp_hourly', winslash='/', mustWork=TRUE)
out_dir <- file.path(data_dir, './analysis')
dir.create(out_dir, showWarnings=FALSE)

# Haka 2024-25: hourly CSV with canopy temperature and relative humidity.
haka_2425 <- read_csv(file.path(data_dir,'Haka_readings2024.2025.csv'), show_col_types=FALSE, trim_ws=TRUE) |>
  transmute(site='Haka', season='2024-25', datetime=ymd_hms(str_trim(DateTime)), temp_c=as.numeric(Canopy), humidity_pct=as.numeric(Humidity))
# Haka 2025-26: Excel stores date and time as Excel serial values; the logger includes blank separator rows.
haka_raw <- read_excel(file.path(data_dir,'Haka_readings_2025.2026.xlsx'), sheet='Block E Bottom weather variable', skip=2, col_names=FALSE, .name_repair='minimal')
names(haka_raw) <- c('date','time','temp','humidity')
haka_2526 <- haka_raw |>
  transmute(date_num=suppressWarnings(as.numeric(date)), time_num=suppressWarnings(as.numeric(time)), temp_c=suppressWarnings(as.numeric(temp)), humidity_pct=suppressWarnings(as.numeric(humidity))) |>
  filter(!is.na(date_num), !is.na(time_num)) |>
  transmute(site='Haka', season='2025-26', datetime=as.POSIXct(as.Date(date_num, origin='1899-12-30')) + time_num*86400, temp_c, humidity_pct)

# Mahela 2023-24 uses decimal commas; 2024-25 uses decimal points.
read_mahela_semicolon <- function(file, season, decimal_mark='.') {
  x <- read_delim(file.path(data_dir,file), delim=';', skip=0, show_col_types=FALSE,
                  trim_ws=TRUE, na=c('', 'NA'),
                  locale=locale(decimal_mark=decimal_mark,
                                grouping_mark=if_else(decimal_mark==',','.',',')),
                  name_repair='minimal')
  # These logger exports always store Date, Time, temperature, and humidity in columns 3-6.
  # Use generated ASCII names so the degree symbol in the source header is never converted.
  if (ncol(x) < 6) stop('Expected at least 6 columns in ', file)
  names(x) <- paste0('column_', seq_along(x))
  dt <- suppressWarnings(mdy_hms(paste(str_trim(as.character(x[[3]])),
                                       str_trim(as.character(x[[4]])))))
  tibble(site='Mahela', season=season, datetime=dt,
         temp_c=suppressWarnings(as.numeric(x[[5]])),
         humidity_pct=suppressWarnings(as.numeric(x[[6]]))) |>
    filter(!is.na(datetime), !is.na(temp_c))
}
mahela_2324 <- read_mahela_semicolon('Mahela_readings2023.2024.csv','2023-24',',')
mahela_2425 <- read_mahela_semicolon('Mahela_readings2024.2025.csv','2024-25','.')
# The 2025-26 logger CSV has a device-information preamble followed by date;time;temperature;humidity rows.
lines <- readLines(file.path(data_dir,'Mahela_readings_2025.2026.csv'), warn=FALSE)
rows <- lines[str_detect(lines, '^\\s*\\d{1,2}-[A-Za-z]{3}-\\d{2};')]
parts <- str_split_fixed(rows, ';', 4)
mahela_2526 <- tibble(site='Mahela', season='2025-26', datetime=dmy_hms(str_trim(paste(parts[,1],parts[,2]))), temp_c=parse_number(str_trim(parts[,3])), humidity_pct=parse_number(str_trim(parts[,4]))) |>
  mutate(temp_c=if_else(temp_c < -10 | temp_c > 50, NA_real_, temp_c)) |>
  filter(!is.na(datetime), !is.na(temp_c))

weather_raw <- bind_rows(haka_2425,haka_2526,mahela_2324,mahela_2425,mahela_2526) |>
  filter(!is.na(datetime), is.finite(temp_c))
quality <- weather_raw |>
  group_by(site, season) |>
  summarise(
    rows_total = n(),
    temperature_out_of_range = sum(temp_c < -10 | temp_c > 50),
    zero_zero_excluded = sum(coalesce(temp_c == 0 & humidity_pct == 0, FALSE)),
    rows_excluded = sum(temp_c < -10 | temp_c > 50 |
                           coalesce(temp_c == 0 & humidity_pct == 0, FALSE)),
    .groups = 'drop'
  )

write_csv(quality, file.path(out_dir, 'data_quality.csv'))
weather <- weather_raw |>
  filter(
    temp_c >= -10,
    temp_c <= 50,
    !coalesce(temp_c == 0 & humidity_pct == 0, FALSE)
  ) |>
  mutate(month = floor_date(datetime, 'month'), hour = hour(datetime)) |>
  arrange(site, season, datetime)
if (!nrow(weather)) stop('No valid weather observations parsed.')

# Accumulate growing degree days from September 1, the practical start of the South African kiwifruit growing season.
agdd_base_c <- 4
growing_seasons <- tibble(
  growing_season=c('2023/2024','2024/2025','2025/2026'),
  season_start=as.Date(c('2023-10-01','2024-10-01','2025-10-01')),
  season_end=as.Date(c('2024-03-31','2025-03-31','2026-03-31'))
)
daily_extrema <- weather |>
  mutate(date=as.Date(datetime)) |>
  group_by(site,date) |>
  summarise(tmin_c=min(temp_c), tmax_c=max(temp_c), logger_records=n(), .groups='drop')
season_grid <- bind_rows(lapply(seq_len(nrow(growing_seasons)),function(i) {
  expand.grid(site=sort(unique(weather$site)),
              date=seq.Date(growing_seasons$season_start[i],growing_seasons$season_end[i],by='day'),
              KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE) |>
    as_tibble() |>
    mutate(growing_season=growing_seasons$growing_season[i],
           season_start=growing_seasons$season_start[i])
}))
agdd_daily <- season_grid |>
  left_join(daily_extrema,by=c('site','date')) |>
  arrange(site,growing_season,date) |>
  group_by(site,growing_season,season_start) |>
  mutate(season_day=as.integer(date-season_start)+1L,
         daily_agdd=if_else(is.na(tmax_c),NA_real_,pmax(0,((tmax_c+tmin_c)/2)-agdd_base_c)),
         cumulative_agdd=cumsum(if_else(is.na(daily_agdd),0,daily_agdd)),
         agdd_plot=if_else(is.na(daily_agdd),NA_real_,cumulative_agdd)) |>
  ungroup()
agdd_summary <- agdd_daily |>
  group_by(site,growing_season,season_start) |>
  summarise(agdd_base_c=agdd_base_c,
            days_in_season=n(),
            days_with_weather=sum(!is.na(daily_agdd)),
            coverage_pct=100*days_with_weather/days_in_season,
            agdd_total=if(all(is.na(daily_agdd))) NA_real_ else max(cumulative_agdd),
            first_observed=if(all(is.na(daily_agdd))) as.Date(NA) else min(date[!is.na(daily_agdd)]),
            last_observed=if(all(is.na(daily_agdd))) as.Date(NA) else max(date[!is.na(daily_agdd)]),
            .groups='drop')
write_csv(agdd_daily,file.path(out_dir,'agdd_daily.csv'))
write_csv(agdd_summary,file.path(out_dir,'agdd_summary.csv'))
p_agdd <- ggplot(agdd_daily,aes(season_day,agdd_plot,color=growing_season,group=growing_season)) +
  geom_line(linewidth=.8,na.rm=TRUE) + facet_wrap(~site,ncol=1) +
  scale_x_continuous(limits=c(1,366),breaks=seq(1,361,by=30),expand=expansion(mult=c(0,.01))) +
  scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.05))) +
  labs(title='Accumulated growing degree days (base 4 C)',
       subtitle='Daily GDD = max(0, ((Tmax + Tmin) / 2) - 4); gaps indicate missing logger days',
       x='Days since 1 September',y='Accumulated GDD (degree-days)',color='Growing season') +
  theme_minimal(base_size=11) + theme(legend.position='top')
ggsave(file.path(out_dir,'agdd_by_site_season.png'),p_agdd,width=10,height=8,dpi=180)

summ <- weather |>
  group_by(site,season) |>
  summarise(n=n(), start=min(datetime), end=max(datetime), temp_mean=mean(temp_c), temp_sd=sd(temp_c), temp_min=min(temp_c), temp_max=max(temp_c), humidity_mean=mean(humidity_pct,na.rm=TRUE), humidity_min=min(humidity_pct,na.rm=TRUE), humidity_max=max(humidity_pct,na.rm=TRUE), .groups='drop')
write_csv(summ, file.path(out_dir,'season_summary.csv'))
write_csv(weather, file.path(out_dir,'hourly_weather_clean.csv'))
monthly <- weather |>
  group_by(site,season,month) |>
  summarise(n=n(), temp_mean=mean(temp_c), temp_min=min(temp_c), temp_max=max(temp_c), humidity_mean=mean(humidity_pct,na.rm=TRUE), .groups='drop')

write_csv(monthly,file.path(out_dir,'monthly_summary.csv'))
weekly_weather <- weather |>
  mutate(week_start=floor_date(datetime,'week',week_start=1)) |>
  group_by(site,season,week_start) |>
  summarise(n=n(), temp_mean=mean(temp_c), temp_min=min(temp_c), temp_max=max(temp_c),
            humidity_n=sum(!is.na(humidity_pct)),
            humidity_mean=if_else(all(is.na(humidity_pct)),NA_real_,mean(humidity_pct,na.rm=TRUE)),
            humidity_min=if_else(all(is.na(humidity_pct)),NA_real_,min(humidity_pct,na.rm=TRUE)),
            humidity_max=if_else(all(is.na(humidity_pct)),NA_real_,max(humidity_pct,na.rm=TRUE)),
            .groups='drop')
write_csv(weekly_weather,file.path(out_dir,'weekly_weather_summary.csv'))
p5 <- ggplot(weekly_weather,aes(week_start,temp_mean,color=season,fill=season,group=season)) +
  geom_ribbon(aes(ymin=temp_min,ymax=temp_max),alpha=.15,color=NA) +
  geom_line(linewidth=.7) + geom_point(size=1) + facet_wrap(~site,scales='free_y',ncol=1) +
  scale_x_datetime(date_labels='%b %Y',date_breaks='2 months') +
  labs(title='Weekly temperature',subtitle='Lines show weekly means; shaded bands show weekly minima and maxima',x=NULL,y='Temperature (°C)',color='Season',fill='Season') +
  theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')
ggsave(file.path(out_dir,'weekly_temperature.png'),p5,width=11,height=9,dpi=180)
p6 <- ggplot(weekly_weather,aes(week_start,humidity_mean,color=season,fill=season,group=season)) +
  geom_ribbon(aes(ymin=humidity_min,ymax=humidity_max),alpha=.15,color=NA) +
  geom_line(linewidth=.7,na.rm=TRUE) + geom_point(size=1,na.rm=TRUE) + facet_wrap(~site,scales='free_y',ncol=1) +
  scale_x_datetime(date_labels='%b %Y',date_breaks='2 months') +
  labs(title='Weekly relative humidity',subtitle='Lines show weekly means; shaded bands show weekly minima and maxima',x=NULL,y='Relative humidity (%)',color='Season',fill='Season') +
  theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')
ggsave(file.path(out_dir,'weekly_humidity.png'),p6,width=11,height=9,dpi=180)

p1 <- ggplot(monthly,aes(month,temp_mean,color=site,group=site)) + geom_line(linewidth=.7) + geom_point(size=1.3) + geom_ribbon(aes(ymin=temp_min,ymax=temp_max,fill=site),alpha=.12,color=NA,inherit.aes=TRUE) + facet_wrap(~season,scales='free_x',ncol=1) + scale_x_datetime(date_labels='%b %Y',date_breaks='2 months') + labs(title='Monthly canopy / logger temperature',x=NULL,y='Temperature (°C)',color='Site',fill='Site') + theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')

ggsave(file.path(out_dir,'temperature_by_season.png'),p1,width=10,height=8,dpi=180)

p2 <- weather |> group_by(site,season,hour) |> summarise(temp=mean(temp_c),.groups='drop') |> ggplot(aes(hour,temp,color=site)) + geom_line(linewidth=.8) + geom_point(size=1.5) + facet_wrap(~season,ncol=2) + scale_x_continuous(breaks=seq(0,21,3)) + labs(title='Mean daily temperature profile',x='Hour of day',y='Mean temperature (°C)',color='Site') + theme_minimal(base_size=11) + theme(legend.position='top')
ggsave(file.path(out_dir,'diurnal_temperature.png'),p2,width=10,height=6,dpi=180)
p3 <- weather |> filter(!is.na(humidity_pct)) |> group_by(site,season,month) |> summarise(humidity=mean(humidity_pct),.groups='drop') |> ggplot(aes(month,humidity,color=site,group=site)) + geom_line() + geom_point(size=1.2) + facet_wrap(~season,scales='free_x',ncol=1) + scale_x_datetime(date_labels='%b %Y',date_breaks='2 months') + labs(title='Monthly mean relative humidity',x=NULL,y='Relative humidity (%)',color='Site') + theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')
ggsave(file.path(out_dir,'humidity_by_season.png'),p3,width=10,height=7,dpi=180)

# Rainfall: Haka event logs are mm/event; Mahela workbook's filtered sheet provides monthly farm totals.
rain_files <- c('Haka_KZNrain_2024.2025.csv','Haka_KZNrain_2025.2026.csv')
rain_haka <- bind_rows(lapply(rain_files,function(f){
  x <- read_delim(file.path(data_dir,f),delim=';',skip=0,show_col_types=FALSE,trim_ws=TRUE,na=c('','','NA'))
  names(x) <- make.names(names(x)); names(x)[1:4] <- c('field','series','datetime','value')
  x |> filter(!is.na(datetime),!is.na(value)) |> transmute(site='Haka',season=if_else(str_detect(f,'2024'),'2024-25','2025-26'),datetime=ymd_hm(datetime),rain_mm=as.numeric(value))
}))
mahela_rain <- read_excel(file.path(data_dir,'Mahela Rainfall_Tableau (Live).xls'),sheet='Mahela filtered') |> 
  filter(Farm=='Sandford_Mahela') |> mutate(month_num=match(Month,month.name), datetime=as.POSIXct(make_date(as.integer(Season),month_num,1))) |> 
  transmute(site='Mahela',season=if_else(Season==2024,'2024-25',as.character(Season)),datetime,rain_mm=as.numeric(Rainfall)) |> filter(!is.na(datetime))
write_csv(rain_haka,file.path(out_dir,'haka_rain_events_clean.csv'))
weekly_haka_rain <- rain_haka |>
  mutate(week_start=floor_date(datetime,'week',week_start=7)) |> # week starts on Sunday
  group_by(site,season,week_start) |>
  summarise(rain_mm=sum(rain_mm,na.rm=TRUE), rain_events=n(), .groups='drop')
write_csv(weekly_haka_rain,file.path(out_dir,'weekly_haka_rainfall.csv'))
write_csv(mahela_rain,file.path(out_dir,'mahela_monthly_rain_clean.csv'))
rain_month <- bind_rows(rain_haka |> mutate(month=floor_date(datetime,'month')),mahela_rain |> mutate(month=floor_date(datetime,'month'))) |>
  group_by(site,season,month) |> summarise(rain_mm=sum(rain_mm,na.rm=TRUE),.groups='drop')
write_csv(rain_month,file.path(out_dir,'monthly_rainfall.csv'))

p4 <- ggplot(rain_month,aes(month,rain_mm,fill=season,group=season)) +
  geom_col(position='dodge') + facet_wrap(~site,scales='free_y',ncol=1) +
  scale_x_datetime(date_labels='%b %Y',date_breaks='2 months') +
  scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.05))) +
  labs(title='Monthly rainfall by site',subtitle='Panels use separate zero-based y-scales; colors distinguish seasons',x=NULL,y='Rainfall (mm)',fill='Season') +
  theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')
ggsave(file.path(out_dir,'monthly_rainfall.png'),p4,width=11,height=12,dpi=180)
p7 <- ggplot(weekly_haka_rain,aes(week_start,rain_mm,fill=season)) +
  geom_col(position='dodge') + facet_wrap(~site,ncol=1) +
  scale_x_datetime(date_labels='%b %Y',date_breaks='1 month') +
  scale_y_continuous(limits=c(0,NA),expand=expansion(mult=c(0,.05))) +
  labs(title='Weekly Haka rainfall',x=NULL,y='Rainfall (mm)',fill='Season') +
  theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='top')
ggsave(file.path(out_dir,'weekly_rainfall.png'),p7,width=11,height=7,dpi=180)

cat('Parsed weather rows:',nrow(weather),' | Haka rain events:',nrow(rain_haka),' | Mahela monthly rain rows:',nrow(mahela_rain),'\n')
print(summ)



















