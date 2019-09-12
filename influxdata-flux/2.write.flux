write = from(bucket:"telegraf/autogen")
|> range(start: -1h)
|> filter(fn:(r) => r._measurement == "influxdb_write" and r._field == "pointReq")
|> limit(n:3)
//write
