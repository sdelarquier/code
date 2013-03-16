import sys
sys.path.append('/davitpy')
from pydarn.sdio import *
import datetime as dt
from utils.timeUtils import *
from pydarn.sdio.dbUtils import *
from multiprocessing import Process
from collections import defaultdict
import gc
import os	


d = dt.datetime(2010,11,17)
radar = 'bks'
ndays = 1
for j in range(1):
	
	#before = defaultdict(int)
	#after = defaultdict(int)
	#for i in gc.get_objects(): before[type(i)] += 1
	
	# if(j % 1000 == 0): 
	# 	print 'repairing'
	# 	db = getDbConn(username='sd_dbwrite',password='more_dbwrite')
	# 	if(j == 0): db.beams.remove()
		#db.command('repairDatabase')
		#db.connection.disconnect()
	# p1 = Process(target=mapDbFit, args=(dateToYyyymmdd(d+dt.timedelta(days=j)), 'bks', [0,2400], 'fitex', 1,))
	# p2 = Process(target=mapDbFit, args=(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhe', [0,2400], 'fitex', 1,))
	# p3 = Process(target=mapDbFit, args=(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhw', [0,2400], 'fitex', 1,))
	# p1.start()
	# p2.start()
	# p3.start()
	# p1.join()
	# p2.join()
	# p3.join()
	mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), radar, time=[0,2400], fileType='fitex', vb=1)
	#mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), 'bks', time=[0,2400], fileType='fitacf', vb=1)
	#mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhe', time=[0,2400], fileType='fitex', vb=1)
	#mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhe', time=[0,2400], fileType='fitacf', vb=1)
	#mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhw', time=[0,2400], fileType='fitex', vb=1)
	#mapDbFit(dateToYyyymmdd(d+dt.timedelta(days=j)), 'fhw', time=[0,2400], fileType='fitacf', vb=1)
	
	##myFile = dmapOpen(dateToYyyymmdd(d+dt.timedelta(days=j)),'fhe',time=[0,2400])
	#print j
	#myFile = open('/tmp/fit/1355580121.fhe.fitex','r')
	#dfile = pydarn.dmapio.readDmapRec(myFile)
	#while(dfile != None):
		##print dt.datetime.fromtimestamp(dfile['time'])
		#dfile = pydarn.dmapio.readDmapRec(myFile)
		##print sys.gettotalrefcount()
	#myFile.close()
	gc.collect()
	os.system('rm /tmp/fit/*')	
	#reset
	#y

	#leaked_things=[[x] for x in range(10)]
	#for i in get_objects():after[type(i)]+=1 
	#print [(k,after[k]-before[k]) for k in after if after[k]-before[k]]

