To get up and running:

```

cp .env.example .env

docker-compose up --build

```

Connect to docker container:

```

docker exec -ti docker-cobol_app_1 bash

```

Compile and Run Cobol Demo:

```

cobc -x -free -o CobolContants CobolContants.cbl

./CobolContants

```
