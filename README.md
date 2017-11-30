# Simulation and Performance Evaluation Assignment 2

#### Run

##### With RStudio

```sh
docker-compose up -d
# browse localhost:8787 with usr:pass rstudio:rstudio
```

##### With R
```sh
docker-compose up -d
docker exec -it first_studio_1 bash
Rscript first.r
```

##### R already installed
```sh
# move first.t in Rscript HOME | modify first.r:4 and set the working directory
Rscript first.r
```