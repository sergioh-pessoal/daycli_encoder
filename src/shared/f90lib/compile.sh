#!/bin/bash
gfortran -c mtree.f90
gfortran -o test teste.f90 mtree.o