# meso-tool
MESO Shiny app

# Build the docker container
docker build -t jncc/meso-tool

# Run the app
docker run -p 3838:3838 -p 8787:8787 -t jncc/meso-tool

It may take a few seconds for the app to start up

Navigate to http://localhost:3838
