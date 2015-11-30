# HTTPS certificates

This directory holds a demo HTTPS   certificate,  key and password. With
this in place, the deamon can be started  with the following to start on
port the default HTTPS port (443):

  ==
  sudo ./daemon.pl --user=www-data --https \
       --certfile=server/server-cert.pem \
       --keyfile=server/server-key.pem \
       --pwfile=server/passwd
  ==

Note that the file `server/passwd`,  which   holds  the password for the
private key, only needs to be readable by _root_.
