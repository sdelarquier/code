#!/usr/bin/env python
from numpy.distutils.core import setup, Extension

ext = Extension('igrf.igrfFort',sources=['igrf11.pyf','igrf11.f90'])

setup (name = "IGRF",
       version = "0.1",
       description = "Wrapper to call fortran routines from the IGRF model",
       author = "Sebastien de Larquier",
       author_email = "sdelarquier@vt.edu",
       url = "",
       long_description =
        """
For more information on the International Geomagnetic Reference Field, go to 
http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html
        """,
       packages = ['igrf'],
       keywords=['Scientific/Space'],
       classifiers=[
                   "Programming Language :: Python/Fortran"
                   ]
        )

