# docker build . -t kagawalab/haslan
# docker run -v /tmp/haslan:/tmp/haslan --restart unless-stopped kagawalab/haslan &
#
# /etc/nginx/conf.d/default.conf の記述例
#    location /haslan/ {
#        proxy_pass http://unix:/tmp/haslan/haslan.unixsocket:/;
#    } 
#
# docker run 実行後に（ホスト側で）以下のコマンドを実行する必要があるかもしれない
#
# host$ sudo chown nginx.nginx /tmp/haslan/haslan.unixsocket

FROM library/ubuntu:22.04
LABEL maintainer="Koji Kagawa <kagawa.koji@kagawa-u.ac.jp>"

RUN mkdir haslan
COPY dist-newstyle/build/x86_64-linux/ghc-9.2.8/start-haskell-0.1.0.0/x/start-haskell/build/start-haskell/start-haskell haslan/
COPY static haslan/

RUN apt-get update && apt-get install -y socat

ENV LANG en_US.UTF-8

RUN mkdir -p /tmp/haslan
RUN chmod a+w /tmp/haslan

RUN echo "#!/bin/bash" > start.sh
RUN echo "./haslan/start-haskell &" >> start.sh
RUN echo "exec socat UNIX-LISTEN:/tmp/haslan/haslan.unixsocket,fork,user=www-data,reuseaddr TCP4:127.0.0.1:8080" >> start.sh
RUN chmod a+x start.sh



VOLUME ["/tmp/haslan"]
CMD ["./start.sh"]
