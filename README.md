# Distributed JavaScript computing

 * http://redd.it/178vsz

Each open page consumes one core.

## Running a Server

You'll need [simple-httpd][simple-httpd] and cache-table.el from
[skewer-mode][skewer-mode]. Then run Emacs in batch mode and visit
http://localhost:8080/ (determined by `httpd-port`).

    emacs -batch -Q -L skewer-mode/ -L simple-httpd/ -l key-collab.el \
          -f key-collab-batch

Notice: Emacs' batch mode is broken in Windows, so you'll need to drop
the first argument when running the server on that platform.


[simple-httpd]: https://github.com/skeeto/emacs-web-server
[skewer-mode]: (https://github.com/skeeto/skewer-mode
