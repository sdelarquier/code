# Copyright (C) 2012  VT SuperDARN Lab
# Full license can be found in LICENSE.txt
"""
*********************
**Module**: rbspDb
*********************
This module writes RBSP positions and FP to a Mongo DB


"""
import subprocess, os
from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta
import pymongo
import numpy as np


datapath = '/home/sebastien/Documents/code/rbsp/data/'
orbitpath = '/home/sebastien/Documents/code/rbsp/orbit/'

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

  db = conn.rbsp
  # db.add_user(username, password, False)
  # db.add_user(unme_read, upwd_read, True)
  keys = [('time', pymongo.ASCENDING),
          ('scraft', pymongo.DESCENDING)]
  # db.orbits.ensure_index(keys, unique=True, drop_dups=True)
  # db.apogees.ensure_index(keys, unique=True, drop_dups=True)
  db.ephemeris.ensure_index(keys, unique=True, drop_dups=True)
except:
  print "Problem connecting to MongoDB at "+username+':'+password+'@'+dbAddress+'/'
  raise
#####################################


#####################################
# Set time range to 1 month back and 3 month ahead
#####################################
sdate = datetime(2012,8,31)
dateNow = datetime.utcnow().replace(hour=0, minute=0, second=0, microsecond=0)
#sdate = dateNow - relativedelta(months=1)
edate = dateNow + relativedelta(months=3)
dates = [sdate]
while dates[-1] < edate:
	dates.append( dates[-1]+timedelta(days=1) )
#####################################


#####################################
# Find apogees
#####################################
def getApogees(trace):
    mins = np.r_[True, trace.rho[1:] >= trace.rho[:-1]] & np.r_[trace.rho[:-1] > trace.rho[1:], True]
    mins[0] = mins[-1] = False

    lonNH = trace.lonNH[mins]
    latNH = trace.latNH[mins]
    lonSH = trace.lonSH[mins]
    latSH = trace.latSH[mins]

    time = trace.datetime[mins]

    return time, latNH, lonNH, latSH, lonSH
#####################################


#####################################
# Trace FPs
#####################################
def getTrace(date, data, spacecraft):
	import tsyganenko as ts
	import os

	inds = (np.array(data['date']) == date.date())
	
	tdates = np.array(data['time'])[inds]
	lats = np.array(data['lat'])[inds]
	lons = np.array(data['lon'])[inds]
	rhos = np.array(data['alt'])[inds]

	# Trace FPs
	kind = 'f' if date.date() <= datetime.utcnow().date() else 'p'
	fname = os.path.join( datapath, 
		'trace.{:%Y%m%d}.{}.{}.dat'.format(date, spacecraft.upper(), kind) )
	try: 
		trace = ts.tsygTrace(filename=fname)
	except:
		print 'Tracing {}...'
		trace = ts.tsygTrace(lats, lons, rhos, datetime=tdates, rmin=1.047)
		trace.save( fname )

	trace.kind = list( np.array(data['kind'])[inds] )
	trace.nIn = len(tdates)

	return trace
#####################################


#####################################
# Function to DL orbit information from APL
#####################################
def getOrbit(sTime, eTime, spacecraft):
	import urllib2, urllib
	from datetime import datetime
	import pickle, os

	fileName = os.path.join(orbitpath, 
		'orbitpos.{}.{:%Y%m%d}.dat'.format(spacecraft.upper(), datetime.utcnow()))
	try:
		with open(fileName, 'rb') as f:
			data = pickle.load(f)
	except:
		print 'Get orbit of spacecraft {} from APL'.format(spacecraft)
		header = 'on'
		cmode = 'geo'
		Cadence = 5

		params = urllib.urlencode({'sDay': str( sTime.day ),
									'sMonth': str( sTime.month ),
									'sYear': str( sTime.year ),
									'sHour': str( sTime.hour ),
									'sMinute': str( sTime.minute ),
									'eDay': str( eTime.day ),
									'eMonth': str( eTime.month ),
									'eYear': str( eTime.year ),
									'eHour': str( eTime.hour ),
									'eMinute': str( eTime.minute ),
									'Cadence': str( Cadence ),
									'mode': str( cmode ),
									'scraft': spacecraft,
									'header': header,
									'getASCII': 'Get ASCII Output'})
		f = urllib2.urlopen("http://athena.jhuapl.edu/LT_Position_Calc", params)
		# f = urllib2.urlopen("http://athena.jhuapl.edu/orbit_pos", params)
		out = f.read().splitlines()
		f.close()

		st = out.index('<pre>')+1
		ed = out.index('</pre>')
		header = out[st].split()
		lines = out[st+1:ed]
		data = {'date': [],
				'time': [], 
				'alt': [],
				'lat': [],
				'lon': [],
				'kind': [], 
				'scraft': []}
		for i,l in enumerate(lines):
			row = l.split()
			cTime = datetime(	int(row[0]), int(row[1]), int(row[2]), 
								int(row[3]), int(row[4]), int(row[5])	)
			data['date'].append( cTime.date() )
			data['time'].append( cTime )
			data['alt'].append( float(row[6]) )
			data['lat'].append( float(row[7]) )
			data['lon'].append( float(row[8]) )
			if cTime > datetime.utcnow():
				data['kind'].append( 'forecast' )
			else:
				data['kind'].append( 'final' )
			data['scraft'].append( spacecraft )

		with open(fileName, 'wb') as f:
			pickle.dump(data, f)

	return data
#####################################


#####################################
# Function to DL orbit information from APL
#####################################
def makeDbDict(date, data, scraft):
	import numpy as np

	trace = getTrace(date, data, scraft.upper())

	# List apogees
	time, latNH, lonNH, latSH, lonSH = getApogees(trace)

	dbIn = []
	for i in range(trace.nIn):
		# Calculate L-shell
		Lsh = np.sqrt(trace.xTrace[i,:]**2 + trace.yTrace[i,:]**2 + trace.zTrace[i,:]**2).max()
		# Apogee or not
		isAp = True if trace.datetime[i] in time else False
		# Make new DB entry
		dbIn.append( {'time': trace.datetime[i],
						'scraft': scraft.lower(),
						'kind': trace.kind[i],
						'alt': trace.rho[i],
						'lat': trace.lat[i],
						'lon': trace.lon[i], 
						'lonNH': trace.lonNH[i],
						'latNH': trace.latNH[i],
						'lonSH': trace.lonSH[i],
						'latSH': trace.latSH[i],
						'L': Lsh,
						'isApogee': isAp} )

	return dbIn

#####################################


#####################################
# Get positions from APL
#####################################
dataA = getOrbit(sdate, edate, 'a')
dataB = getOrbit(sdate, edate, 'b')
#####################################


#####################################
# Update DB
#####################################
for date in dates[:-2]:
	print '--> ', date

	# Only process new positions if:
	#	- they are not in the DB
	#	- their final version is available
	noSave = True
	dbQ = conn.rbsp.pos.find({'time': {'$gte': date, '$lt': date+timedelta(hours=24)}})
	if dbQ.count() > 0:
		for doc in dbQ:
			if doc['kind'] != 'final' and doc['time'].date() < datetime.utcnow().date(): 
				noSave = False
				break
	else: noSave = False
	# Do not update DB
	if noSave: continue

	dbIn = makeDbDict(date, dataA, 'a')
	ins = db.ephemeris.insert(dbIn)

	dbIn = makeDbDict(date, dataB, 'b')
	ins = db.ephemeris.insert(dbIn)
#####################################
