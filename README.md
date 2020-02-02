# nibbleql

Client for nibbledb

```
john@air nibbleql % docker run --network host -it jptmoore/nibbleql
Welcome to nibbleql the query language for nibbledb

nibble> set host "https://localhost:8000";
https://localhost:8000
nibble> post 42.0 to "sensor1";

nibble> POST 42.1 TO "sensor2";

nibble> post 43.0 TO "sensor1" where "serial" is "A001";

nibble> get min from "sensor1" SINCE 1m;
{ "min": 42 }
nibble> get from "sensor1" SINCE 24h;
[
  {
    "timestamp": 1580662264279578,
    "data": { "serial": "A001", "value": 43 }
  },
  { "timestamp": 1580662246471956, "data": { "value": 42 } }
]
nibble> get from "sensor1" SINCE 1d;
[
  {
    "timestamp": 1580662264279578,
    "data": { "serial": "A001", "value": 43 }
  },
  { "timestamp": 1580662246471956, "data": { "value": 42 } }
]
nibble> get from "sensor1,sensor2" where "serial" is "A001" since 1000s;
[
  {
    "timestamp": 1580662264279578,
    "data": { "serial": "A001", "value": 43 }
  }
]
nibble> get from "sensor2" last 10;
[ { "timestamp": 1580662256258819, "data": { "value": 42.1 } } ]
nibble> get count from "sensor1,sensor2" where "serial" is "A001" since 10sec;
{ "count": 0 }
nibble> get from "sensor1" range 10days to 0days;
[
  {
    "timestamp": 1580662264279578,
    "data": { "serial": "A001", "value": 43 }
  },
  { "timestamp": 1580662246471956, "data": { "value": 42 } }
]
nibble> delete from "sensor2" range 10m to 5m;

nibble> delete from "sensor1" where "serial" is "A001" range 5s to 0s;

nibble> quit
john@air nibbleql %
```

