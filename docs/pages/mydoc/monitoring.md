---
title: Monitoring Antidote with prometheus
last_updated: Nov 20, 2017
tags: [monitoring]
sidebar: mydoc_sidebar
permalink: monitoring.html
folder: mydoc/dev
---

# Monitoring Antidote with prometheus

Antidote is configured to expose metrics to prometheus server. At present the metrics collected are operations per second, open transactions, staleness, aborted transactions and errors together with several virtual machine metrics.

Follow the steps to start one Antidote node and view the metrics.

```bash
cd monitoring
docker-compose up -d
```
This starts one Antidote node, a prometheus server and a graphana database.
If the containers started with no problems, open
`http://localhost:9090/targets` in a web-browser. You will see targets antidote and prometheus.

Now open
`http://localhost:3000/login` and login with
`admin:admin`


Click `Add data source`.

Enter following details:  
Name: *SomeDataSourceName*  
Type: Prometheus

Url : http://prometheus:9090   
Access: proxy

Click **Add**.

Go To
Dashboard -> import new dashboard.  
Import file monitoring/Antidote-Dashboard.json

To view the monitoring data of antidote, go to Dashboard and open Antidote from the list of dashboards.

To shutdown containers:
`docker-compose down`
