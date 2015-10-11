Pastecode
---------

A small webapp for sharing snippets of code directly from Emacs/SLIME, with support for interactive Common-Lisp macroexpansion

Pastecode depends on quicklisp and (since the effect of backquote is standaradised, but not the implementation) sbcl (>= 1.2.2). 
This code also depends on a (potentially distressing) number of my own libraries, which aren't in quicklisp.
To fetch all dependencies you can use the following shell script:

```sh
cd ~/quicklisp/local-projects/

git clone https://github.com/pjstirling/pastecode.git
git clone https://github.com/pjstirling/pjs-utils.git
git clone https://github.com/pjstirling/pjs-webapp.git
git clone https://github.com/pjstirling/pjs-yaclml.git
git clone https://github.com/pjstirling/pjs-logging.git
git clone https://github.com/pjstirling/pjs-sqlite.git
git clone https://github.com/pjstirling/pjs-chtml-helpers.git

# Disabled for "security" by default, but if your lisp is running hostile code then it can 'rm -rf ~/',
# anything it can do to emacs seems a bit trivial in comparison...
# Without this enabled pastecode can't automatically load the small emacs module (in pastecode.el) when
# pastecode is loaded into your lisp, and you will have to do it manually (or arrange for the file to
# be loaded from your '.emacs'
echo "(setq slime-enable-evaluate-in-emacs t)" >> ~/.emacs

sbcl --eval "(asdf:load-system '#:pjs-yaclml-generator)" \
     --eval "(pjs-yaclml-generator:generate-code)" \
     --eval "(asdf:load-system '#:pastecode)"
```

At this point there are several bits of config inside `~/quicklisp/local-projects/pjs-webapp/pjs-webapp.lisp` that should be adjusted for your local network configuration, and whether you are running a reverse-proxy. Pastecode also uses LOCAL-NETWORK-HOST-P to decide if you can delete pastes.

Usage
-----

1. Load ASDF system `pastecode` from within SLIME
2. Place the cursor between the opening and closing parentheses of a form in a SLIME buffer
3. Use `M-x slime-pastecode-create-paste`
4. Input a description in the minibuffer
5. Point a browser at http://localhost:8181/paste/
