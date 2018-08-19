#
# Makefile for rail
#
# $Lastupdate: 2018-08-19 19:39:50$
#

ifeq (, $(shell which cask))
  EMACS = emacs
else
  EMACS = cask exec emacs
endif
MANIFEST= contrib/MANIFEST.rail
MULEVER	= contrib/MULE_VERSION
FLIMVER	= contrib/FLIM_VERSION contrib/ADD_FLIM_VERSION
SEMIVER	= contrib/SEMI_VERSION contrib/ADD_SEMI_VERSION
RAILVER = $(shell grep rail-version rail-vars.el | awk '{print $$3}')

TABLES	?= rail-table-flim.el
TABLES	+= rail-table-semi.el
TABLES	+= rail-table-mule.el
EL		?= $(TABLES)
EL		+= rail.el
EL		+= rail-common.el
EL		+= rail-vars.el
EL		+= rail-user-agent.el
ELC		= $(EL:%.el=%.elc)
ELFLAGS = -q -no-site-file -batch

%.elc: %.el
	$(EMACS) $(ELFLAGS) -L . -f batch-byte-compile $<

all: $(ELC)

# rail
rail.el: $(TABLES)

rail-table-mule.el: $(MULEVER)
	$(EMACS) $(ELFLAGS) -l ./contrib/rail-make-table.el -f rail-make-table-mule

rail-table-flim.el: $(FLIMVER)
	$(EMACS) $(ELFLAGS) -l ./contrib/rail-make-table.el -f rail-make-table-flim

rail-table-semi.el: $(SEMIVER)
	$(EMACS) $(ELFLAGS) -l ./contrib/rail-make-table.el -f rail-make-table-semi

# clean up
clean:
	-rm -f $(ELC)

distclean: maintainer-clean

maintainer-clean:
	-rm -f *~ $(ELC) $(TABLES)

test: run-test
run-test: $(ELC)
	$(EMACS) $(ELFLAGS) -L . -l test/run-test.el

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
