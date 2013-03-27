# Copyright (C) 2012  VT SuperDARN Lab
# Full license can be found in LICENSE.txt
"""
*********************
**Module**: gme.mho
*********************
This module handles Millstone Hill ISR data

**Class**:
	* :class:`mhoData`: Read Millstone Hill data, either locally if it can be found, or directly from Madrigal

"""
import sys
sys.path.append('/davitpy')


#####################################################
#####################################################
def mhoRead( expDate,
	dataPath='/home/sebastien/Documents/code/mho/data/', 
	fileExt=None) :
	'''Acces Millstone Hill data.
	Looks first for local hdf5 file, if nothing is found, 
	try to dowmload from Madrigal.
	'''
	import madrigalWeb.madrigalWeb
	import os, glob
	import datetime as dt
	import h5py as h5
	import matplotlib as mp
	import numpy as np
	from utils import geoPack as gp

	fileName = 'mlh{:%y%m%d}'.\
		format( expDate )
	if not fileExt:
		dfiles = np.sort(glob.glob(dataPath+fileName+'?.???.hdf5'))
		if not list(dfiles):
			fileExt = ''
		else:
			fileExt = dfiles[-1][-10:-5]
	
	fileName = fileName+fileExt+'.hdf5'
	filePath = os.path.join(dataPath, fileName)

	try:
		with h5.File(filePath,'r') as f: pass
		print 'Found local file: '+filePath
	except:
		dl = getFilesFromMad(expDate, 
			expDate+dt.timedelta(days=1),
			dataPath=dataPath)
		if dl is None: 
			print 'Nothing for {:%Y-%b-%d}'.format(expDate)
			return
		else:
			fileExt = dl[-1]
			filePath = dataPath+dl+'.hdf5'
			try:
				with h5.File(filePath,'r') as f: pass
			except:
				raise
		print 'Downloaded remote file: '+filePath

	with h5.File(filePath,'r') as f:
		data = f['Data']['Array Layout']
		data2D = data['2D Parameters']
		data1D = data['1D Parameters']
		params = f['Metadata']['Experiment Parameters']
		nel = data2D['nel'][:].transpose()
		ne = data2D['ne'][:].transpose()
		popl = data2D['popl'][:].transpose()
		ti = data2D['ti'][:].transpose()
		tr = data2D['tr'][:].transpose()
		vo = data2D['vo'][:].transpose()
		te = tr*ti
		mho_range = data['range'][:]
		mho_range2 = data2D['range'][:].transpose()
		mho_time = mp.dates.epoch2num( data['timestamps'][:] )
		mho_elev = np.array( [data1D['el1'][:],
			data1D['el2'][:]] ).T
		mho_azim = np.array( [data1D['az1'][:], 
			data1D['az2'][:]] ).T
		try:
			mho_scntyp = data1D['scntyp'][:]
		except:
			mho_scntyp = np.zeros(mho_time.shape)
		vinds = np.where( mho_elev[:,0] >= 85. )
		if len(vinds[0]) > 0:
			mho_scntyp[vinds] = 5
		telev = data1D['el1'][:].reshape((1,len(data1D['el1'])))
		telev = np.radians(telev)
		telev = np.sin(telev)
		tran = data['range'][:].reshape((len(data['range']),1))
		Re = 6371.
		mho_alti = np.sqrt(Re**2 + \
						tran**2 + \
						2.*Re*tran.dot(telev)) - Re
		mho_gdalt = data2D['gdalt'][:].transpose()
		mhoPos = [float(params[7][1]),
		          float(params[8][1]),
		          float(params[9][1])]
		f.close()
		mho_lat = np.zeros(np.shape(mho_alti))
		mho_lon = np.zeros(np.shape(mho_alti))
		for ir, dist in enumerate(mho_range):
			dd = gp.calcDistPnt(mhoPos[0],mhoPos[1],mhoPos[2],
				dist=dist, el=mho_elev[:,0], az=mho_azim[:,0])
			mho_lat[ir,:] = dd['distLat']
			mho_lon[ir,:] = dd['distLon']

	return {'nel': nel, 
			'ne': nel, 
			'popl': popl,
	        'ti': ti, 
	        'tr': tr, 
	        'te': te, 
	        'vo': vo, 
	        'range': mho_range,
	        'range2': mho_range2,
	        'time': mho_time, 
	        'elev': mho_elev, 
	        'azim': mho_azim, 
	        'scntyp': mho_scntyp,
	        'alti': mho_alti, 
	        'gdalt': mho_gdalt, 
	        'lat': mho_lat, 
	        'lon': mho_lon,
	        'pos': mhoPos, 
	        'filename': os.path.split(filePath)[-1]}

	
#####################################################
#####################################################
def getFilesFromMad(sdate, fdate, dataPath=None):
	'''Get files from Madrigal database
	'''
	import madrigalWeb.madrigalWeb
	import os, h5py
	import numpy as np
	from matplotlib.dates import date2num, epoch2num, num2date
	import datetime as dt
	# constants
	user_fullname = 'Sebastien de Larquier'
	user_email = 'sdelarquier@vt.edu'
	user_affiliation = 'Virginia Tech'

	madrigalUrl = 'http://cedar.openmadrigal.org'
	madData = madrigalWeb.madrigalWeb.MadrigalData(madrigalUrl)

	expList = madData.getExperiments(30, 
		sdate.year, sdate.month, sdate.day, sdate.hour, 
		sdate.minute, sdate.second, 
		fdate.year, fdate.month, fdate.day, fdate.hour, 
		fdate.minute, fdate.second)
	if not expList: return

	thisFilename = False
	fileList = madData.getExperimentFiles(expList[0].id)
	for thisFile in fileList:
	    if thisFile.category == 1:
	        thisFilename = thisFile.name
	        break
	    
	if not thisFilename: return

	result = madData.downloadFile(thisFilename, 
		os.path.join( dataPath,"{}.hdf5"\
		.format(os.path.split(thisFilename)[1]) ), 
		user_fullname, user_email, user_affiliation, 
		format="hdf5")

	# Now add some derived data to the hdf5 file
	res = madData.isprint(thisFilename, 
		'YEAR,MONTH,DAY,HOUR,MIN,SEC,GDALT,RANGE,POPL,NE,NEL,TE,MDTYP',
 		'', user_fullname, user_email, user_affiliation)

	rows = res.split("\n")
	moreData = {'dt': [], 
	            'gdalt': [], 
	            'range': [],  
	            'popl': [],  
	            'ne': [],  
	            'nel': [],  
	            'te': [],  }
	for r in rows:
	    dat = r.split()
	    if dat and dat[-1] == '115':
	        if dat[8] == 'missing': dat[8] = 'nan'
	        if dat[9] == 'missing': dat[9] = 'nan'
	        if dat[10] == 'missing': dat[10] = 'nan'
	        if dat[11] == 'missing': dat[11] = 'nan'
	        moreData['dt'].append( dt.datetime(int(dat[0]), 
											   int(dat[1]), 
											   int(dat[2]), 
											   int(dat[3]), 
											   int(dat[4])) )
	        moreData['gdalt'].append( float(dat[6]) ) 
	        moreData['range'].append( float(dat[7]) ) 
	        moreData['popl'].append( float(dat[8]) ) 
	        moreData['ne'].append( float(dat[9]) )
	        moreData['nel'].append( float(dat[10]) ) 
	        moreData['te'].append( float(dat[11]) )

	filePath = os.path.join( dataPath,
		os.path.split(thisFilename)[1]+'.hdf5' )
	print 'Downloading '+filePath
	with h5py.File(filePath,'r+') as f:
		ftime = epoch2num( f['Data']['Array Layout']['timestamps'] )
		frange = f['Data']['Array Layout']['range']
		tDim = ftime.shape[0]
		rDim = frange.shape[0]
		shape2d = (tDim, rDim)
		gdalt = np.empty(shape2d)
		gdalt[:] = np.nan
		ne = np.empty(shape2d)
		ne[:] = np.nan
		nel = np.empty(shape2d)
		nel[:] = np.nan
		dtfmt = '%Y-%m-%d %H:%M:%S'
		dttype = np.dtype('a{}'.format(len(dtfmt)+2))
		dtime = np.empty(tDim, dtype=dttype)

		# Iterate through the downloaded data
		for i in range(len(moreData['dt'])):
		    # Figure out your range/time index
		    tind = np.where(ftime[:] <= date2num(moreData['dt'][i]))[0]
		    rind = np.where(frange[:] <= moreData['range'][i])[0]
		    if not list(tind) or not list(rind): continue
		    gdalt[tind[-1],rind[-1]] = moreData['gdalt'][i]
		    ne[tind[-1],rind[-1]] = moreData['ne'][i]
		    nel[tind[-1],rind[-1]] = moreData['nel'][i]
		    dtime[tind[-1]] = moreData['dt'][i].strftime(dtfmt)

		parent = f['Data']['Array Layout']['2D Parameters']
		gdalt_ds = parent.create_dataset('gdalt', data=gdalt)
		ne_ds = parent.create_dataset('ne', data=ne)
		nel_ds = parent.create_dataset('nel', data=nel)

		parent = f['Data']['Array Layout']
		datetime_ds = parent.create_dataset('datetime', data=dtime)

	return os.path.split(thisFilename)[1]


#####################################################
#####################################################
def isrFov(myMap, isrName, isrPos, elev, 
	azim=[-180.,180.], misa=None, ax=None):
	'''Plot ISR field-of-view
	'''
	from utils import geoPack as gp
	import numpy as np
	from matplotlib import pylab
	import matplotlib as mp

	if not ax: ax = pylab.gca()
	fovCol = '0'

	# ISR location
	x0, y0 = myMap(isrPos[1], isrPos[0], coords='geo')
	myMap.scatter(x0, y0, s=20, zorder=5, c='k', ax=ax)
	ax.text(x0*1.04, y0*0.96, isrName, 
		fontsize=10, variant='small-caps')

	# MHO fov
	nazims = 100
	azims = np.linspace(azim[0], azim[1], nazims)
	isrFov = np.zeros((nazims, 3))
	isrFov2 = np.zeros((nazims, 3))
	Rav = 6370.
	for iaz, taz in enumerate(azims):
		dd = gp.calcDistPnt(isrPos[0], isrPos[1], 0., 
			distAlt=450., el=elev, az=taz)
		isrFov[iaz,0] = dd['distLat']
		isrFov[iaz,1] = dd['distLon']
		isrFov[iaz,2] = dd['distAlt']
	# x, y = myMap(isrFov[:,1], isrFov[:,0], coords='geo')
	# myMap.plot(x, y, c=fovCol, ax=ax, zorder=10)
	for iaz, taz in enumerate(azims):
		dd = gp.calcDistPnt(isrPos[0], isrPos[1], 0., 
			distAlt=250., el=elev, az=taz)
		isrFov2[iaz,0] = dd['distLat']
		isrFov2[iaz,1] = dd['distLon']
		isrFov2[iaz,2] = dd['distAlt']
	# x2, y2 = myMap(isrFov2[:,1], isrFov2[:,0], coords='geo')
	# myMap.plot(x2, y2, c=fovCol, ax=ax, zorder=10)

	contourLat = np.concatenate( (isrFov[:,0], 
								  [isrPos[0]]) )
	contourLon = np.concatenate( (isrFov[:,1],
								  [isrPos[1]]) )
	x3, y3 = myMap(contourLon, contourLat, coords='geo')
	contour = np.transpose( np.vstack((x3,y3)) )
	patch = mp.patches.Polygon( contour, color='k', 
		alpha=.8, zorder=15)
	pylab.gca().add_patch(patch)


	# if abs(azim[1] - azim[0]) != 360.:
	#     myMap.plot([x0,x[0]], [y0,y[0]], zorder=10,
	#     	c=fovCol, ax=ax, lw=1)
	#     myMap.plot([x0,x[-1]], [y0,y[-1]], zorder=10,
	#     	c=fovCol, ax=ax, lw=1)
	if misa:
		dd = gp.calcDistPnt(isrPos[0], isrPos[1], 0., 
			distAlt=450., el=elev, az=misa)
		x, y = myMap(dd['distLon'], dd['distLat'],coords='geo')
		myMap.plot([x0,x], [y0,y], zorder=10,
			c=(0,.3,0), ax=ax)

	return isrFov

