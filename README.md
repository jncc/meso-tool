# meso-tool

MESO Shiny app

A Proof of Concept visualisation tool to assist in understanding the probabilitic impact that Anthropogenic Pressures (i.e. human activities) has on the habitats of sub-littoral areas of the United Kingdom.

The tool provides a mapping using a Continuous Gaussian Bayesian Belief Network from the Anthropogenic Pressures through the biotopes and to the output processes and ultimately the Ecosystem services, to which the habitat supports.

## Build the docker container

    docker build . -t meso-shiny

## Run the app

    docker run -p 3838:3838 -t meso-shiny

It may take a few seconds for the app to start up

You should see some output in the console like the following:

    [2020-06-01T12:20:00.184] [INFO] shiny-server - Shiny Server v1.5.13.943 (Node.js v12.14.1)
    [2020-06-01T12:20:00.217] [INFO] shiny-server - Using config file "/etc/shiny-server/shiny-server.conf"
    [2020-06-01T12:20:00.354] [INFO] shiny-server - Starting listener on http://[::]:3838

Navigate to http://localhost:3838

## Push to AWS ECR

Activate your credentials in aws-vault

Get login

    $(aws ecr get-login --region eu-west-2 --no-include-email)

Push image

    docker push <account no>.dkr.ecr.eu-west-2.amazonaws.com/meso-shiny
