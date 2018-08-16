#
# Makefile for rail
#
# $Lastupdate: 2018-08-16 16:09:32$
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
RAILVER = $(shell grep rail-version rail-vars.el | awk '{print $$3}')
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

test: rail
	$(EMACS) -q -no-site-file -batch -L . -l test/run-test.el

cl: git2cl
git2cl:
	@git log --date=short --pretty=format:"%ad  %an  <%ae>%n%n* %s%n%b%n" | \
	sed -e 's/^\(.*\)$$/\t\1/g' | \
	sed -e 's/^\t\([0-9]*-[0-9]*-[0-9]*.*\)$$/\1/g' | \
	sed -e 's/^\t$$//g' | \
	sed ':loop; N; $$!b loop; s/\n\n\n/\n\n/g' \
	> ChangeLog
# create tar.gz
tar:
	@git archive --format=tar --prefix=rail-$(RAILVER)/ HEAD \
	  | gzip -9 > ../rail-$(RAILVER).tar.gz
