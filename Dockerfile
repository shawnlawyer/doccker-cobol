FROM ubuntu:16.04
WORKDIR /var/www/html
RUN apt-get -y update
RUN apt-get -y install screen gcc open-cobol
COPY . /var/www/html
ENTRYPOINT ["/var/www/html/start.sh"]
