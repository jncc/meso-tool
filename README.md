# meso-tool
MESO Shiny app

# Build the docker container
```
docker build -t jncc/meso-tool
```

# Run the app
```
docker run -p 3838:3838 -p 8787:8787 -t jncc/meso-tool
```

It may take a few seconds for the app to start up

You should see some output in the console like the following:

```
[2020-06-01T12:20:00.184] [INFO] shiny-server - Shiny Server v1.5.13.943 (Node.js v12.14.1)
[2020-06-01T12:20:00.217] [INFO] shiny-server - Using config file "/etc/shiny-server/shiny-server.conf"
[2020-06-01T12:20:00.354] [INFO] shiny-server - Starting listener on http://[::]:3838
```

Navigate to http://localhost:3838
