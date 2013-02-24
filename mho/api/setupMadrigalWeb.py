"""set up file for the Python Madrigal Remote API

$Id: setupMadrigalWeb.py 4088 2012-11-08 18:26:37Z brideout $
"""
import os, os.path, sys

from distutils.core import setup

if sys.argv[1] == 'sdist':
    # update the documentation
    cmd = 'cp -r ../../doc/madpyDoc/remotePythonAPI/*.html remotePyDoc/'
    os.system(cmd)
    cmd = 'cp -r ../../doc/madpyDoc/remotePythonAPI/madrigalWeb/*.html remotePyDoc/madrigalWeb/'
    os.system(cmd)
    cmd = 'cp -r ../../doc/madpyDoc/remotePythonAPI/madrigalWebPlot/*.html remotePyDoc/madrigalWebPlot/'
    os.system(cmd)
    cmd = 'cp MANIFEST.in.madrigalWeb MANIFEST.in'
    os.system(cmd)
    
setup(name="remotePythonAPI",
      version="2.2",
      description="Remote Madrigal Python API",
      author="Bill Rideout",
      author_email="wrideout@haystack.mit.edu",
      url="http://www.haystack.mit.edu/~brideout/",
      packages=["madrigalWeb", "madrigalWebPlot"],
      scripts=['madrigalWeb/globalIsprint.py', 'madrigalWeb/globalDownload.py',
               'madrigalWebPlot/scripts/madrigalPColor.py',
               'madrigalWebPlot/scripts/madrigalScatter.py',
               'madrigalWeb/examples/exampleMadrigalWebServices.py']
      )

if sys.argv[1] == 'sdist':
    cmd = 'rm MANIFEST.in'
    os.system(cmd)
    