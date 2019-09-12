httpd = from(bucket:"telegraf/autogen")
|> range(start: -1h)
|> filter(fn:(r) => r._measurement == "influxdb_httpd" and r._field == "writeReq")
|> limit(n:3)
httpd
