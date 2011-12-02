EMACS=emacs

clean:
	rm -f *.tmp *.tmp.* loaddefs.el

install:
	mkdir -p $(DESTDIR)/usr/share/emacs/site-lisp/mythryl-mode/
	cp -a mythryl-mode.el $(DESTDIR)/usr/share/emacs/site-lisp/mythryl-mode/mythryl-mode.el

# Recipe for loaddefs.el, useful to create site-lisp autoload code
loaddefs.el: mythryl-mode.el
	cat mythryl-mode.el >mythryl-mode.tmp.el
	echo ";;Local Variables: " >>mythryl-mode.tmp.el
	echo ";;generated-autoload-file: \"loaddefs.el\"" >>mythryl-mode.tmp.el
	echo ";;End: " >>mythryl-mode.tmp.el
	$(EMACS) -q --batch --eval "(update-file-autoloads \"mythryl-mode.tmp.el\" t)"
	sed -i.tmp "s/[.]tmp//g" loaddefs.el
	rm -f mythryl-mode.tmp.el loaddefs.el.tmp

all:
