#!/bin/bash
import os
import subprocess
import sys
sys.path.append('/davitpy')
from pydarn import radar
import pymongo
from datetime import datetime
import h5py

#####################################
# Set DB params
#####################################
username = 'sd_dbwrite'
password = 'more_dbwrite'
dbAddress = 'sd-work9.ece.vt.edu'
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
# Read data from files
#####################################
# Build radar and hdw dictionnaries
radars = []
hdw = []
radarF = radar.radarRead()
nradar = len(radarF['id'])
for irad in xrange( nradar ):
    radars.append( {'id': radarF['id'][irad], 
                    'cnum': radarF['cnum'][irad], 
                    'code': radarF['code'][irad], 
                    'name': radarF['name'][irad], 
                    'operator': radarF['operator'][irad], 
                    'hdwfname': radarF['hdwfname'][irad], 
                    'status': radarF['status'][irad], 
                    'stTime': radarF['stTime'][irad], 
                    'edTime': radarF['edTime'][irad], 
                    'snum': 0} )
    siteF = radar.hdwRead(radars[-1]['hdwfname'])
    if not siteF: continue
    tsnum = 0
    for isit in xrange( len(siteF['tval']) ):
        if siteF['tval'][isit] == 0: continue
        tval = datetime(3000,1,1) if siteF['tval'][isit] == -1 else siteF['tval'][isit]
        hdw.append( {'id': radarF['id'][irad], 
                   'tval': tval,
                   'geolat': siteF['geolat'][isit],
                   'geolon': siteF['geolon'][isit],
                   'alt': siteF['alt'][isit], 
                   'boresite': siteF['boresite'][isit],
                   'bmsep': siteF['bmsep'][isit],
                   'vdir': siteF['vdir'][isit],
                   'atten': siteF['atten'][isit],
                   'tdiff': siteF['tdiff'][isit],
                   'phidiff': siteF['phidiff'][isit],
                   'interfer': siteF['interfer'][isit],
                   'recrise': siteF['recrise'][isit],
                   'maxatten': siteF['maxatten'][isit],
                   'maxgate': siteF['maxgate'][isit],
                   'maxbeam': siteF['maxbeam'][isit]} )
        tsnum += 1
    radars[-1]['snum'] = tsnum

# Build metadata dictionnary
infodict = [{'var': 'id' ,  'description': '''unique radar ID (int)'''}, 
            {'var': 'cnum' ,  'description': '''number of radar codes (int)'''}, 
            {'var': 'code' ,  'description': '''radar codes, , i.e. [gbr, g] (list of strings)'''}, 
            {'var': 'name' ,  'description': '''radar full name (string)'''}, 
            {'var': 'operator' ,  'description': '''radar operator (string)'''}, 
            {'var': 'hdwfname' ,  'description': '''hdw.dat file name, usually hdw.dat.code (string)'''}, 
            {'var': 'status' ,  'description': '''radar operating status: 1 for active, -1 for inactive and 0 for planned (int)'''}, 
            {'var': 'stTime' ,  'description': '''radar first light (string: %Y-%m-%d %H:%M:%S)'''}, 
            {'var': 'edTime' ,  'description': '''radar last day of operation (string: %Y-%m-%d %H:%M:%S)'''}, 
            {'var': 'snum' ,  'description': '''number of site updates, i.e. entries in hdw.dat (int)'''}, 
            {'var': 'tval' ,  'description': '''last date/time operating with given parameters; 3000/1/1 if current (string: %Y-%m-%d %H:%M:%S)'''}, 
            {'var': 'geolat' ,  'description': '''main array geographic latitude (float) [degrees]'''}, 
            {'var': 'geolon' ,  'description': '''main array geographic longitude (float) [degrees]'''}, 
            {'var': 'alt' ,  'description': '''main array altitude (float) [km]'''}, 
            {'var': 'boresite' ,  'description': '''boresight azimuth (float) [degrees]'''}, 
            {'var': 'bmsep' ,  'description': '''beam separation (float) [degrees]'''}, 
            {'var': 'vdir' ,  'description': '''velocity sign: (int) 
At the radar level, backscattered signals with frequencies above the 
transmitted frequency are assigned positive Doppler velocities while 
backscattered signals with frequencies below the transmitted frequency 
are assigned negative Doppler velocity. This convention can be reversed 
by changes in receiver design or in the data samping rate. This parameter 
is set to +1 or -1 to maintain the convention.'''}, 
            {'var': 'atten' ,  'description': '''Analog Rx attenuator step (float) [dB]'''}, 
            {'var': 'tdiff' ,  'description': '''(float) [microsecond] 
Propagation time from interferometer array antenna to 
phasing matrix input minus propagation time from main array antenna 
through transmitter to phasing matrix input. If the signal from the 
interferometer comes first, then tdiff < 0'''}, 
            {'var': 'phidiff' ,  'description': '''(float) 
Phase sign (Cabling errors can lead to a 180 degree shift of the 
interferometry phase measurement. +1 indicates that the sign is 
correct, -1 indicates that it must be flipped.)'''}, 
            {'var': 'interfer' ,  'description': '''Interferometer offset  (Displacement of midpoint of 
interferometer array from midpoint of main array. This is given in 
meters in Cartesian coordinates. X is along the line of antennas with 
+X toward higher antenna numbers, Y is along the array normal 
direction with +Y in the direction of the array normal. Z is the 
altitude difference, +Z up.)'''}, 
            {'var': 'recrise' ,  'description': '''Analog Rx rise time (float) [microsecond]
Time delays of less than ~10 microseconds can be ignored. If narrow-band filters are 
used in analog receivers or front-ends, the time delays should be specified.'''}, 
            {'var': 'maxatten' ,  'description': '''Analog attenuation stages (int) 
Number of stages. This is used for gain control of an analog receiver or front-end.'''}, 
            {'var': 'maxgate' ,  'description': '''Maximum number of range gates (int)'''}, 
            {'var': 'maxbeam' ,  'description': '''Maximum number of beams (int)'''} ]
#####################################


#####################################
# Fill DB
#####################################
try:
  conn = pymongo.MongoClient('mongodb://'+username+':'+password+'@'+dbAddress+'/')

  db = conn.radarInfo
  db.add_user(unme_read, upwd_read, True)
except:
  print "Problem connecting to MongoDB at "+username+':'+password+'@'+dbAddress+'/'
  raise

try:
  db.drop_collection('radars')
  db.drop_collection('hdw')
  db.drop_collection('metadata')
except:
  print "Problem dropping collections"
  raise

try:
  coll_radars = db.radars
  ins = coll_radars.insert(radars)
  coll_radars.ensure_index('id')
  coll_radars.ensure_index('code')

  coll_hdw = db.hdw
  ins = coll_hdw.insert(hdw)
  coll_hdw.ensure_index('id')

  coll_metadata = db.metadata
  ins = coll_metadata.insert(infodict)
except:
  print "Problem inserting data"
  raise
#####################################
