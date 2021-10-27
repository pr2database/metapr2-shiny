cd 'C:/daniel.vaulot@gmail.com/Shiny/metapr2'

# Public dataset

docker build . -t metapr2-public

docker run --rm -p 8080:8080 metapr2-public

docker tag metapr2-public asia.gcr.io/tactile-bolt-247111/metapr2-public:v0.1.0

docker push asia.gcr.io/tactile-bolt-247111/metapr2-public:v0.1.0


# Private dataset

docker build . -t metapr2-private

docker run --rm -p 8080:8080 metapr2-private

docker tag metapr2-private asia.gcr.io/tactile-bolt-247111/metapr2-private:v0.1.0

docker push asia.gcr.io/tactile-bolt-247111/metapr2-private:v0.1.0





# http://localhost:8080/

# Run interactively to check problems 

docker run -it metapr2 /bin/bash
R
source("app.R")
