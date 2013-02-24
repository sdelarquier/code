import sys
sys.path.append('/davitpy')

mhoCipher = {'edens': r'Electron dens. [$\log$(m$^{-3}$)]',
			 'ti': r'T$_i$ [K]',
			 'te': r'T$_e$ [K]', 
			 'tr': r'T$_e$/T$_i$'}


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
	import os
	import datetime as dt
	import h5py as h5
	import matplotlib as mp
	import numpy as np
	from utils import geoPack as gp

	fileName = 'mlh{}g'.\
		format( expDate.strftime('%y%m%d') )
	fileName += '.00{}.hdf5'
	filePath = os.path.join(dataPath, fileName)

	try:
		for i in range(1,4):
			if not os.path.isfile(filePath.format(i)): continue
			fileExt = i
			filePath = filePath.format(fileExt)
			with h5.File(filePath,'r') as f: pass
			break
		if not fileExt: 
			with h5.File(filePath,'r') as f: pass
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
				print filePath
				with h5.File(filePath,'r') as f: pass
			except:
				raise

	with h5.File(filePath,'r') as f:
		data = f['Data']['Array Layout']
		data2D = data['2D Parameters']
		data1D = data['1D Parameters']
		params = f['Metadata']['Experiment Parameters']
		edens = data2D['popl'][:].transpose()
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
		mho_scntyp = data1D['scntyp'][:]
		vinds = np.where( mho_elev[:,0] >= 85. )
		if len(vinds[0]) > 0:
			mho_scntyp[vinds] = 5
		mho_alti = np.dot( \
			np.diag( np.sin(np.radians(mho_elev[:,0])) ),\
			mho_range2.T \
			).transpose()
		#mho_alti = mho_range2*mho_elev
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

	return {'edens': edens, 
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
	        'lat': mho_lat, 
	        'lon': mho_lon,
	        'pos': mhoPos}

	
#####################################################
#####################################################
def getFilesFromMad(sdate, fdate, dataPath=None):
	'''Get files from Madrigal database
	'''
	import madrigalWeb.madrigalWeb
	import os
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

	madData.isprint()

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
	fovCol = '.3'

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
	x, y = myMap(isrFov[:,1], isrFov[:,0], coords='geo')
	myMap.plot(x, y, c=fovCol, ax=ax, zorder=10)
	for iaz, taz in enumerate(azims):
		dd = gp.calcDistPnt(isrPos[0], isrPos[1], 0., 
			distAlt=250., el=elev, az=taz)
		isrFov2[iaz,0] = dd['distLat']
		isrFov2[iaz,1] = dd['distLon']
		isrFov2[iaz,2] = dd['distAlt']
	x2, y2 = myMap(isrFov2[:,1], isrFov2[:,0], coords='geo')
	myMap.plot(x2, y2, c=fovCol, ax=ax, zorder=10)

	contourLat = np.concatenate( (isrFov[:,0], 
								  isrFov2[-1::-1,0]) )
	contourLon = np.concatenate( (isrFov[:,1], 
								  isrFov2[-1::-1,1]) )
	x3, y3 = myMap(contourLon, contourLat, coords='geo')
	contour = np.transpose( np.vstack((x3,y3)) )
	patch = mp.patches.Polygon( contour, color='g', 
		alpha=.3, zorder=15)
	pylab.gca().add_patch(patch)


	if abs(azim[1] - azim[0]) != 360.:
	    myMap.plot([x0,x[0]], [y0,y[0]], zorder=10,
	    	c=fovCol, ax=ax, lw=.5)
	    myMap.plot([x0,x[-1]], [y0,y[-1]], zorder=10,
	    	c=fovCol, ax=ax, lw=.5)
	if misa:
		dd = gp.calcDistPnt(isrPos[0], isrPos[1], 0., 
			distAlt=450., el=elev, az=misa)
		x, y = myMap(dd['distLon'], dd['distLat'],coords='geo')
		myMap.plot([x0,x], [y0,y], zorder=10,
			c=(0,.3,0), ax=ax)

	return isrFov

