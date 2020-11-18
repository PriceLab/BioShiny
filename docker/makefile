ID = 18nov2020
IMAGE_NAME = bioshinyapp_$(ID)
BASE_IMAGE_NAME = bioshinybase_$(ID)

TESTPORT=60055

build:
	docker build -t $(IMAGE_NAME) -f Dockerfile .

bash:
	docker run -it \
           --entrypoint /bin/bash \
           $(IMAGE_NAME)



# takes about 40 minutes
build.base:
	docker build -t $(BASE_IMAGE_NAME) -f Dockerfile-base .

bash.base:
	docker run -it \
           --entrypoint /bin/bash \
           $(BASE_IMAGE_NAME)



