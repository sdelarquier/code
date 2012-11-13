#!/bin/bash

DIR := igrf/

all:
	f2py ${DIR}igrf11.f90 -m igrf -h ${DIR}igrf11.pyf
	f2py -c ${DIR}igrf11.pyf ${DIR}igrf11.f90
	python setup.py install

clean:
	find . -type f -name "*.o" | xargs rm -f
	find . -type f -name "*.so" | xargs rm -f
	find ${DIR}. -type f -name "*.pyf" | xargs rm -f