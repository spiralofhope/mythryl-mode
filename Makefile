PREFIX=$(DESTDIR)/usr/share
ELINSTALLDIR=$(PREFIX)/emacs/site-lisp/mythryl-mode
EMACS=emacs
ELFILE=mythryl-mode.el

clean:
	rm -f *.tmp *.tmp.* loaddefs.el

install:
	mkdir -p $(ELINSTALLDIR)
	cp -a $(ELFILE) $(ELINSTALLDIR)/$(ELFILE)

# Recipe for loaddefs.el, useful to create site-lisp autoload code
loaddefs.el: $(ELFILE)
	cat $(ELFILE) >$(ELFILE).tmp.el
	echo >>$(ELFILE).tmp.el
	echo ";;Local Variables: " >>$(ELFILE).tmp.el
	echo ";;generated-autoload-file: \"loaddefs.el\"" >>$(ELFILE).tmp.el
	echo ";;End: " >>$(ELFILE).tmp.el
	$(EMACS) -q --batch --eval "(update-file-autoloads \"$(ELFILE).tmp.el\" t)"
	sed -i.tmp "s/[.]tmp//g" loaddefs.el
	rm -f $(ELFILE).tmp.el loaddefs.el.tmp

all:
