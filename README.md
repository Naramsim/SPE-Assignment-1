# Simulation and Performance Evaluation Assignment 1

#### Install

```sh
git clone https://github.com/Naramsim/SPE-Assignment-1.git
```

#### Run

##### With RStudio

```sh
docker-compose up -d
# browse localhost:8787 with usr:pass rstudio:rstudio
```

##### With R
```sh
docker-compose up -d
docker exec -it rstudio bash
Rscript first.r
```

##### R already installed
```sh
Rscript first.r
```