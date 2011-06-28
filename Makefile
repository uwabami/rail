#
# Makefile for rail
#
# $Id: Makefile,v 1.7 1994/02/23 15:05:00 tmo Exp $
# last modified by simm-emacs@fan.gr.jp, Fri, 17 Sep 1999 00:10:36 +0900
#

LISPDIR	= default
PACKAGEDIR	= default

EMACS	= emacs
#EMACS	= mule
XEMACS	= xemacs
MANIFEST= contrib/MANIFEST.rail

MULEVER	= contrib/MULE_VERSION
MW32VER	= contrib/MEADOW_VERSION
FLIMVER	= contrib/FLIM_VERSION contrib/ADD_FLIM_VERSION
SEMIVER	= contrib/SEMI_VERSION contrib/ADD_SEMI_VERSION

MAKE	=	make.el

#
# You shouldn't need to change anything after this point.
#

CAT	= cat
ECHO	= echo
TEST	= test
MKDIR	= mkdir
INSTALL	= install
PERL	= perl
INSTALL_DATA	= $(INSTALL) -m644

LISPS	= rail.el rail-user-agent.el
MTABLES	= rail-table-flim.el rail-table-semi.el
TABLES	= rail-table-mule.el rail-table-meadow.el $(MTABLES)

all: rail
install: install-rail

# rail
rail: table
	$(EMACS) -q -no-site-file -batch -l ./$(MAKE) -f compile-rail

install-rail: rail
	$(EMACS) -q -no-site-file -batch -l ./$(MAKE) -f install-rail $(LISPDIR)

# rail-*-table rebuild
table: $(TABLES)

rail-table-mule.el: $(MULEVER)
	$(EMACS) -batch -q -no-site-file -l ./rail-make-table.el -f rail-make-table-mule

rail-table-meadow.el: $(MW32VER)
	$(EMACS) -batch -q -no-site-file -l ./rail-make-table.el -f rail-make-table-meadow

rail-table-flim.el: $(FLIMVER)
	$(EMACS) -batch -q -no-site-file -l ./rail-make-table.el -f rail-make-table-flim

rail-table-semi.el: $(SEMIVER)
	$(EMACS) -batch -q -no-site-file -l ./rail-make-table.el -f rail-make-table-semi

# for XEmacs21 package
package: $(TABLES)
	$(XEMACS) -q -no-site-file -batch -l ./$(MAKE) -f compile-rail

install-package: package
	$(XEMACS) -q -no-site-file -batch -l ./$(MAKE) -f install-package $(PACKAGEDIR)

# clean up
clean: 
	-rm -f *~ *.elc

distclean: 
	-rm -f *~ *.elc $(MTABLES)

maintainer-clean:
	-rm -f *~ *.elc $(TABLES)

cl: git2cl
git2cl:
	@git log --date=short --pretty=format:"%ad %an <%ae>%n%n%s%n%b" | \
	sed -e 's/^\(.*\)$$/\t\1/g' | \
	sed -e 's/^\t\([0-9]*-[0-9]*-[0-9]*.*\)$$/\1/g' | \
	sed -e 's/^\t$$//g' \
	> ChangeLog
