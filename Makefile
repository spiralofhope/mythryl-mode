clean:
	true

install:
	mkdir -p $(DESTDIR)/usr/share/emacs/site-lisp/mythryl-mode/
	cp -a mythryl-mode.el $(DESTDIR)/usr/share/emacs/site-lisp/mythryl-mode/mythryl-mode.el

