dashboardTime = -1h

httpd = from(bucket:"telegraf/autogen")
|> range(start: dashboardTime)
|> filter(fn:(r) => r._measurement == "influxdb_httpd" and r._field == "writeReq")

write = from(bucket:"telegraf/autogen")
|> range(start: dashboardTime)
|> filter(fn:(r) => r._measurement == "influxdb_write" and r._field == "pointReq")

avg_batch_size = join(tables:{httpd:httpd, write:write}, on:["_time"])
|> map(fn:(r) => ({
	_value: r._value_write / r._value_httpd
}))
|> mean()

avg_batch_size
