---+ HTTPS/SSL certificate generation

Last updated:

 * Nov 3, 2010, Jan Wielemaker
   - Updated paths and copy commands for OpenSSL 0.9.8k


1. /usr/lib/ssl/misc/CA.pl -newca

    ==
    apenoot
    NL
    Noord-Holland
    Amsterdam
    SWI-Prolog foundation
    ClioPatria demo
    Jan Wielemaker
    jan@swi-prolog.org
    ----- extra -----
    nootjes
    SWI-Prolog foundation
    apenoot
    ==

    creates: ./demoCA/

2. /usr/lib/ssl/misc/CA.pl -newreq

    ==
    apenoot1
    NL
    Noord-Holland
    Amsterdam
    SWI-Prolog foundation
    ClioPatria demo
    Jan Wielemaker
    jan@swi-prolog.org
    ----- extra -----
    nootjes
    SWI-Prolog foundation
    ==

    creates: newkey.pem and newreq.pem

3. /usr/lib/ssl/misc/CA.pl -signreq

    ==
    apenoot
    ==

    creates: newcert.pem

4.  Move to server directory:

    ==
    mv newkey.pem server/server-key.pem
    mv newcert.pem server/server-cert.pem
    ==

@see config-available/https.pl
