# Copyright (C) 2012  VT SuperDARN Lab
# Full license can be found in LICENSE.txt
"""
*********************
**Module**: isrDb
*********************
This module writes ISR FoVs to a mongoDB


"""
import subprocess, os
import pymongo

#####################################
# Set DB params
#####################################
username = 'sd_dbwrite'
password = 'more_dbwrite'
dbAddress = 'sd-work8.ece.vt.edu'
unme_read = 'sd_dbread'
upwd_read = '5d'
#####################################


#####################################
# Env var
#####################################
command = ['bash', '-c', 'source /davit/env/env_rst && env']
proc = subprocess.Popen(command, stdout=subprocess.PIPE)
for line in proc.stdout:
  (key, _, value) = line.partition("=")
  os.environ[key] = value.rstrip()
proc.communicate()
#####################################


#####################################
# Connect to DB
#####################################
try:
  conn = pymongo.MongoClient('mongodb://'+username+':'+password+'@'+dbAddress+'/')

  db = conn.isr
  db.drop_collection('info')
  db.drop_collection('codes')
  # db.add_user(username, password, False)
  db.add_user(unme_read, upwd_read, True)
  keys = [('code', pymongo.ASCENDING)]
  db.info.ensure_index(keys, unique=True, drop_dups=True)
except:
  print "Problem connecting to MongoDB at "+username+':'+password+'@'+dbAddress+'/'
  raise
#####################################


def isrFov(isrName, isrPos, elev, azim=[-180.,180.]):
    import sys
    sys.path.append('/davitpy')
    from utils import geoPack as gp
    from numpy import linspace
    
    # MHO fov
    nazims = 100
    azims = linspace(azim[0], azim[1], nazims)
    isrFov = {'lat': [],
              'lon': [],
              'alt': []}
    Rav = 6370.
    for iaz, taz in enumerate(azims):
        dd = gp.calcDistPnt(isrPos['lat'], isrPos['lon'], 0., distAlt=500., el=elev, az=taz)
        isrFov['lat'].append( dd['distLat'] )
        isrFov['lon'].append( dd['distLon'] )
        isrFov['alt'].append( dd['distAlt'] )
    
    return isrFov


def insDb(isrpos, isrname, isrcode, isrfov):
    dbIn = {'pos': isrpos,
            'name': isrname,
            'code': isrcode,
            'fov': isrfov}
    ins = db.info.insert(dbIn)

codes = []

# Mho fov
isrpos = {'lat': 42.6, 'lon': 288.5}
isrname = 'Millstone Hill Observatory'
isrcode = 'mho'
codes.append(isrcode)
isrfov = isrFov('MHO', isrpos, 6.)
insDb(isrpos, isrname, isrcode, isrfov)

# Sondie fov
isrname = 'Sondrestrom'
isrcode = 'sdt'
codes.append(isrcode)
isrpos = {'lat': 67.0, 'lon': 309.0}
isrfov = isrFov('Sond.', isrpos, 25.)
insDb(isrpos, isrname, isrcode, isrfov)

# PFISR fov
isrname = 'Poker Flat ISR'
isrcode = 'pfisr'
codes.append(isrcode)
isrpos = {'lat': 65.1, 'lon': -147.4}
isrfov = isrFov('PFISR', isrpos, 25.)
insDb(isrpos, isrname, isrcode, isrfov)

# RISR fov
isrname = 'Resolute Bay ISR'
isrcode = 'risr'
codes.append(isrcode)
isrpos = {'lat': 74.7, 'lon': -94.9}
isrfov = isrFov('RISR', isrpos, 25.)
insDb(isrpos, isrname, isrcode, isrfov)

# EISCAT fov
isrname = 'European Incoherent Scatter Radar'
isrcode = 'eiscat'
codes.append(isrcode)
isrpos = {'lat': [69.6, 67.9, 67.4], 'lon': [19.2, 20.4, 26.6]}
isrfov = isrFov('EISCAT', {'lat': isrpos['lat'][0], 'lon': isrpos['lon'][0]}, 21.)
insDb(isrpos, isrname, isrcode, isrfov)

# Insert code list
ins = db.codes.insert({'codes': codes})