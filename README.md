# conky_weather_noaa

Example .conkyrc_net is a .conkyrc for high DPI screens.

Download the JSON file from http://forecast.weather.gov/MapClick.php?lat=[your_latitude]&lon=[your_longitude]&FcstType=json
and save as a file the program can find. I have it downloaded and parsed regularly (no more than hourly; not sure how often NOAA
updates their forecast) using a cron command using wget:

~~~~
wget -O /home/me/.conky_weather/fetched "http://forecast.weather.gov/MapClick.php?lat=0.000&lon=-0.000&FcstType=json" && /home/me/.conky_weather/parse
~~~~
