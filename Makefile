# Makefile for org-cpt

VERSION="$(shell sed -nre '/^;;; Version:/ { s/^;;; Version:[ \t]+//; p }' org-cpt.el)"

# EMACS value may be overridden
EMACS?=emacs

default:
	@echo version is $(VERSION)

test: test-all

test-all:
	cask exec buttercup -L . test
