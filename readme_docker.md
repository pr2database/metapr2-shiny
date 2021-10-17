# Dockerise a shiny app on Google cloud

## Major steps

1. Make sure that the shiny application works on R studio
2. Build docker file 
  * from shiny
  * Install packages as needed
  * Use the shiny-server command
3. Test docker locally
4. Upload to GitHub, this will automatically force compilation on Google cloud and Docker site.

If the Docker application has not been yet uploaded to Google
1. Upload to Google cloud
  * Set up enough memory (2 Go for pr2 primers)
2. Upload to Docker web site

## Building docker image

* Do not use package renv which is really heavy....
* Start Powershell under windows

```
cd 'C:/daniel.vaulot@gmail.com/Shiny/metapr2'


# Buid with cache

docker build . -t metapr2

# Buid without cache

docker build --no-cache . -t metapr2

# Run

docker run --rm -p 8080:8080 metapr2

# Run interactively to check problems 

docker run -it metapr2 /bin/bash
R
source("app.R")

```

Test locally

* http://localhost:8080/


## Push to Cloud run


Utilize Google Builds to build image on the cloud

```
gcloud auth login

gcloud auth configure-docker

gcloud config set project tactile-bolt-247111

gcloud builds submit --timeout=2h --tag asia.gcr.io/tactile-bolt-247111/metapr2
```

Deploy to Google Cloud Run (Need only first time)

```
gcloud run deploy --image asia.gcr.io/tactile-bolt-247111/metapr2 --platform managed --max-instances 1
```

Effectuer ensuite un mappage de domaine sur:

http://app.pr2-primers.org


THIS IS MUCH FASTER FOR TESTING - Push image to Google Registry 

After just do "Modifier et Deployer la Nouvelle Revision"

Will need to change if new tag

```
gcloud auth login

gcloud auth configure-docker

docker tag metapr2 asia.gcr.io/tactile-bolt-247111/metapr2:v0.1.0

docker push asia.gcr.io/tactile-bolt-247111/metapr2:v0.1.0
```


## Push container to Docker repository

```
docker images

docker tag metapr2 vaulot/metapr2:v0.1.0

docker push vaulot/metapr2:v0.1.0
```

## Docker misc

* List running containers

```
docker container ls
```

* Stop a container
```
docker stop 12a32e8928ef
```

* Remove dangling caches
```
docker builder prune
docker image prune
```

* Buid without cache
```
docker build --no-cache . -t asia.gcr.io/aerial-citron-246112/pr2-primers
```

* Management

```
# see All containers
docker ps -a

# See images and remove

docker images
docker rmi xxxxx

# Remove containers, image, cache and volumes

docker system prune --volumes
```

