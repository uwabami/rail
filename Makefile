#
# Makefile for rail
#
# $Lastupdate: 2018-08-15 16:28:11$
#

EMACS	= emacs
FLAGS   = -batch -q -no-site-file
MULEVER	= contrib/MULE_VERSION
FLIMVER	= contrib/FLIM_VERSION contrib/ADD_FLIM_VERSION
SEMIVER	= contrib/SEMI_VERSION contrib/ADD_SEMI_VERSION
RAILVER = $(shell grep rail-version rail-vars.el | awk '{print $$3}')

MTABLES	= rail-table-flim.el rail-table-semi.el
TABLES	= rail-table-mule.el $(MTABLES)
EL		= $(wildcard *.el) $(TABLES)
ELC		= $(EL:%.el=%.elc)

%.elc: %.el
	$(EMACS) $(FLAGS) -L . -f batch-byte-compile $<

all: $(ELC)
$(ELC): table

# rail-*-table rebuild
table: $(TABLES)

rail-table-mule.el: $(MULEVER)
	$(EMACS) $(FLAGS) -l ./contrib/rail-make-table.el -f rail-make-table-mule

rail-table-flim.el: $(FLIMVER)
	$(EMACS) $(FLAGS) -l ./rail-make-table.el -f rail-make-table-flim

rail-table-semi.el: $(SEMIVER)
	$(EMACS) $(FLAGS) -l ./rail-make-table.el -f rail-make-table-semi

# clean up
clean:
	-rm -f *~ *.elc

distclean:
	-rm -f *~ *.elc $(MTABLES)

maintainer-clean:
	-rm -f *~ *.elc $(TABLES)

cl: git2cl
git2cl:
	@git log --date=short --pretty=format:"%ad  %an  <%ae>%n%n* %s%n%b%n" | \
	sed -e 's/^\(.*\)$$/\t\1/g' | \
	sed -e 's/^\t\([0-9]*-[0-9]*-[0-9]*.*\)$$/\1/g' | \
	sed -e 's/^\t$$//g' | \
	sed ':loop; N; $$!b loop; s/\n\n\n/\n\n/g' \
	> ChangeLog
# create tar.gz
release:
	@git archive --format=tar --prefix=rail-$(RAILVER)/ HEAD \
	  | gzip -9 > ../rail-$(RAILVER).tar.gz
