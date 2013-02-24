"""
This script traces, plots and store RBSP footpoints, apogees, 
and radars (those covering apogee points).

You can call each function on its own, but it is preferable to 
call it from the command line

    $ python make_plot.py START_DATE [END_DATE]

Edit the IF statement at the bottom of this code to control 
what gets executed.
"""
import tsyganenko as ts
import numpy as np
from datetime import datetime, timedelta
import sys
sys.path.append('/davitpy')
from pydarn.plot import overlayFov
from pydarn.radar import network
from mpl_toolkits.basemap import Basemap
import pylab



def trace(date=None, fig=True):
    """
Trace RBSP footprints for a given date (python datetime object)
of for all the available dates 
    """
    datapath = '/home/sebastien/Documents/code/rbsp/data/'

    # Read the data
    year, month, day, hour, minute, second, altA, latA, lonA = np.genfromtxt('data/orbitposA.dat', unpack=True, skiprows=1)
    dates = []
    for y,m,d,h,mn,s in zip(year, month, day, hour, minute, second):
        dates.append( datetime( int(y),int(m),int(d),int(h),int(mn),int(s) ) )
    datesA = np.array(dates)

    year, month, day, hour, minute, second, altB, latB, lonB = np.genfromtxt('data/orbitposB.dat', unpack=True, skiprows=1)
    dates = []
    for y,m,d,h,mn,s in zip(year, month, day, hour, minute, second):
        dates.append( datetime( int(y),int(m),int(d),int(h),int(mn),int(s) ) )
    datesB = np.array(dates)

    # Ganerate day-by-day array
    if not date:
        datesAll = np.concatenate((datesA, datesB))
        un, inds = np.unique( [d.strftime('%Y%m%d') for d in datesAll], return_index=True )
        dateIn = datesAll[inds]
    else:
        dateIn = [date]

    # Generate traces day-by-day
    for day in dateIn:
        print 'Tracing {}'.format(day.strftime('%Y-%b-%d'))
        # First A
        inds = [d.date()==day.date() for d in datesA]
        sdates = datesA[np.array(inds)]
        if len(sdates) >= 1:
            # trace points
            lats = latA[np.array(inds)]
            lons = lonA[np.array(inds)]
            rhos = altA[np.array(inds)]
            traceA = ts.tsygTrace(lats, lons, rhos, datetime=sdates)
            # Save traces
            traceA.save( datapath+'trace.{}.A.dat'.format(day.strftime('%Y%m%d')) )

        # Then B
        inds = [d.date()==day.date() for d in datesB]
        sdates = datesB[np.array(inds)]
        if len(sdates) >= 1:
        # trace points
            lats = latB[np.array(inds)]
            lons = lonB[np.array(inds)]
            rhos = altB[np.array(inds)]
            traceB = ts.tsygTrace(lats, lons, rhos, datetime=sdates)
            # Save traces
            traceB.save( datapath+'trace.{}.B.dat'.format(day.strftime('%Y%m%d')) )

    if fig: plot(traceA=traceA, traceB=traceB)


def textHighlighted(xy, text, ax=None, zorder=None, color='k', fontsize=None):
    """
Text with highlighted contour
    """
    import matplotlib as mp
    
    text_path = mp.text.TextPath( (0,0), text, size=fontsize )
    
    p1 = mp.patches.PathPatch(text_path, ec="w", lw=2, fc="w", alpha=0.8, zorder=zorder, 
                       transform=mp.transforms.IdentityTransform())
    p2 = mp.patches.PathPatch(text_path, ec="none", fc=color, zorder=zorder, 
                       transform=mp.transforms.IdentityTransform())
    
    offsetbox2 = mp.offsetbox.AuxTransformBox(mp.transforms.IdentityTransform())
    offsetbox2.add_artist(p1)
    offsetbox2.add_artist(p2)
    ab = mp.offsetbox.AnnotationBbox(
                offsetbox2, xy,
                box_alignment=(.5,0),
                frameon=False
                )
    
    if not ax: ax = gca()
    ax.add_artist(ab)


def plot(date=None, traceA=None, traceB=None, saveDb=True, noPlot=False):
    """
Plot RBSP footprints
    """
    assert not date==None or not (traceA==None and traceB==None), 'Either give a date, or a trace for A and B'

    datapath = '/home/sebastien/Documents/code/rbsp/data/'
    imgpath = '/home/sebastien/Documents/code/rbsp/img/'

    # Read trace
    if not traceA:
        traceA = ts.tsygTrace(filename=datapath+'trace.{}.A.dat'.format(date.strftime('%Y%m%d')))
    if not traceB:
        traceB = ts.tsygTrace(filename=datapath+'trace.{}.B.dat'.format(date.strftime('%Y%m%d')))
    if not date:
        date = traceA.datetime[0]

    if not noPlot:
        # Init figure
        fig = pylab.figure(figsize=(11,6.5))
        pylab.rcParams.update({'font.size': 14})
        fovCol = (.5,.6,.6)
        fovAlpha = .2

        # NH
        ax1 = fig.add_subplot(121)
        m1 = Basemap(projection='npstere',boundinglat=35,lon_0=270,resolution='l')
        m1.drawcoastlines(color='.5')
        m1.fillcontinents(color=(.8,.8,.8))
        # draw parallels and meridians.
        m1.drawparallels(np.arange(-80.,81.,20.), color='.6')
        m1.drawmeridians(np.arange(-180.,181.,20.), color='.6')
        m1.drawmapboundary()
        # Draw FP
        x, y = m1(traceA.lonNH, traceA.latNH)
        m1.scatter(x, y, color='r', zorder=3, s=5)
        x, y = m1(traceB.lonNH, traceB.latNH)
        m1.scatter(x, y, color='b', zorder=3, s=5)
        overlayFov(m1, all=True, hemi='north', maxGate=75, dateTime=traceA.datetime[0], 
            fovColor=fovCol, lineColor=fovCol, lineWidth=0, fovAlpha=fovAlpha)

        # SH
        ax2 = fig.add_subplot(122)
        m2 = Basemap(projection='spstere',boundinglat=-35,lon_0=270,resolution='l')
        m2.drawcoastlines(color='.5')
        m2.fillcontinents(color=(.8,.8,.8))
        # draw parallels and meridians.
        m2.drawparallels(np.arange(-80.,81.,20.), color='.6')
        m2.drawmeridians(np.arange(-180.,181.,20.), color='.6')
        m2.drawmapboundary()
        # Draw FP
        x, y = m2(traceA.lonSH, traceA.latSH)
        m2.scatter(x, y, color='r', zorder=3, s=5)
        x, y = m2(traceB.lonSH, traceB.latSH)
        m2.scatter(x, y, color='b', zorder=3, s=5)
        overlayFov(m2, all=True, hemi='south', maxGate=75, dateTime=traceA.datetime[0],
            fovColor=fovCol, lineColor=fovCol, lineWidth=0, fovAlpha=fovAlpha)

        fig.tight_layout()

        Title = '{}'.format(date.strftime('%Y - %b - %d'))
        fig.text(.02, .92, Title)
        fig.text(.02, .09, 'VAP-A', color='r', va='top')
        fig.text(.1, .09, 'VAP-B', color='b', va='top')

        Title = '{}'.format(date.strftime('%Y - %b - %d'))
        fig.text(.51, .92, Title)
        fig.text(.51, .09, 'VAP-A', color='r', va='top')
        fig.text(.59, .09, 'VAP-B', color='b', va='top')

    # List apogees
    minsA = np.r_[True, traceA.rho[1:] >= traceA.rho[:-1]] & np.r_[traceA.rho[:-1] > traceA.rho[1:], True]
    minsA[0] = minsA[-1] = False
    lonANH = traceA.lonNH[minsA]
    latANH = traceA.latNH[minsA]
    lonASH = traceA.lonSH[minsA]
    latASH = traceA.latSH[minsA]
    timeA = traceA.datetime[minsA]
    minsB = np.r_[True, traceB.rho[1:] >= traceB.rho[:-1]] & np.r_[traceB.rho[:-1] > traceB.rho[1:], True]
    minsB[0] = minsB[-1] = False
    lonBNH = traceB.lonNH[minsB]
    latBNH = traceB.latNH[minsB]
    lonBSH = traceB.lonSH[minsB]
    latBSH = traceB.latSH[minsB]
    timeB = traceB.datetime[minsB]
    rads = network()

    apoA_NH = ''
    apoA_SH = ''
    radsANH = []
    radsASH = []
    for i in xrange(len(lonANH)):
        apoA_NH += '({}UT, {:4.2f}E, {:4.2f}N) | '.format(timeA[i].strftime('%H:%M'), lonANH[i], latANH[i])
        tradANH = rads.getRadarsByPosition(latANH[i], lonANH[i], 300., datetime=timeA[i])
        if not noPlot:
            x, y = m1(lonANH[i], latANH[i])
            # ax1.text(x, y, timeA[i].strftime('%H:%M'), zorder=5, clip_on=True, color=(.3,0,0), fontsize=10, ha='center')
            textHighlighted((x,y), timeA[i].strftime('%H:%M'), ax=ax1, zorder=6, color=(.3,0,0), fontsize=10)
        if tradANH: 
            radsANH.append( tradANH )
        writeToDb(timeA[i], latANH[i], lonANH[i], 'A', tradANH)

        apoA_SH += '({}UT, {:4.2f}E, {:4.2f}N) | '.format(timeA[i].strftime('%H:%M'), lonASH[i], latASH[i])
        tradASH = rads.getRadarsByPosition(latASH[i], lonASH[i], 300., datetime=timeA[i])
        if not noPlot:
            x, y = m2(lonASH[i], latASH[i])
            # ax2.text(x, y, timeA[i].strftime('%H:%M'), zorder=5, clip_on=True, color=(.3,0,0), fontsize=10, ha='center')
            textHighlighted((x,y), timeA[i].strftime('%H:%M'), ax=ax2, zorder=6, color=(.3,0,0), fontsize=10)
        if tradASH: 
            radsASH.append( tradASH )
        if saveDb: writeToDb(timeA[i], latASH[i], lonASH[i], 'A', tradASH)

    apoB_NH = ''
    apoB_SH = ''
    radsBNH = []
    radsBSH = []
    for i in xrange(len(lonBNH)):
        apoB_NH += '({}UT, {:4.2f}E, {:4.2f}N) '.format(timeB[i].strftime('%H:%M'), lonBNH[i], latBNH[i])
        tradBNH = rads.getRadarsByPosition(latBNH[i], lonBNH[i], 300., datetime=timeB[i])
        if not noPlot:
            x, y = m1(lonBNH[i], latBNH[i])
            # ax1.text(x, y, timeB[i].strftime('%H:%M'), zorder=5, clip_on=True, color=(0,0,.3), fontsize=10, ha='center')
            textHighlighted((x,y), timeB[i].strftime('%H:%M'), ax=ax1, zorder=6, color=(0,0,.3), fontsize=10)
        if tradBNH: 
            radsBNH.append( tradBNH )
        writeToDb(timeB[i], latBNH[i], lonBNH[i], 'B', tradBNH)

        apoB_SH += '({}UT, {:4.2f}E, {:4.2f}N) '.format(timeB[i].strftime('%H:%M'), lonBSH[i], latBSH[i])
        tradBSH = rads.getRadarsByPosition(latBSH[i], lonBSH[i], 300., datetime=timeB[i])
        if not noPlot:
            x, y = m2(lonBSH[i], latBSH[i])
            # ax2.text(x, y, timeB[i].strftime('%H:%M'), zorder=5, clip_on=True, color=(0,0,.3), fontsize=10, ha='center')
            textHighlighted((x,y), timeB[i].strftime('%H:%M'), ax=ax2, zorder=6, color=(0,0,.3), fontsize=10)
        if tradBSH: 
            radsBSH.append( tradBSH )
        if saveDb: writeToDb(timeB[i], latBSH[i], lonBSH[i], 'B', tradBSH)

    if not noPlot:
        fig.savefig( imgpath+'rbsp.map.{}.pdf'.format(date.strftime('%Y%m%d')) )
        fig.savefig( imgpath+'rbsp.map.{}.svg'.format(date.strftime('%Y%m%d')) )

        if __name__ == '__main__':
            fig.clf()
            pylab.close(fig)


def createDb():
    """
Initialize RBSP apogees sqlite database
    """
    import sqlite3 as lite
    import sys

    con = lite.connect('/var/www/assets/db/rbsp.db', detect_types=lite.PARSE_DECLTYPES)

    with con:
        
        cur = con.cursor()   
        cur.execute("DROP TABLE IF EXISTS rbsp_apogee") 
        cur.execute("CREATE TABLE rbsp_apogee(\
            date DATE, \
            time DATETIME, \
            lat REAL, \
            lon REAL, \
            sat TEXT \
            )")

        cur.execute("DROP TABLE IF EXISTS rbsp_apogee_radars") 
        cur.execute("CREATE TABLE rbsp_apogee_radars(\
            date DATE, \
            time DATETIME, \
            lat REAL, \
            lon REAL, \
            code TEXT, \
            beam INT, \
            dist INT \
            )")


def writeToDb(dTime, apLat, apLon, sat, radDict):
    """
Populate RBSP apogees database (removing older entries when necessary)
    """
    import sqlite3 as lite
    import sys


    con = lite.connect('/var/www/assets/db/rbsp.db', detect_types=lite.PARSE_DECLTYPES)
    with con:
        cur = con.cursor()

        # First check if this is an update to existing entry
        cur.execute("SELECT * FROM rbsp_apogee WHERE date=? and time=? and lat=? and lon=? and sat=?", (dTime.date(), dTime, apLat, apLon, sat))
        data = cur.fetchone()
        if data:
            cur.execute("DELETE FROM rbsp_apogee WHERE date=? and time=? and lat=? and lon=? and sat=?", (dTime.date(), dTime, apLat, apLon, sat))
            if radDict:
                cur.execute("DELETE FROM rbsp_apogee_radars WHERE date=? and time=? and lat=? and lon=?", (dTime.date(), dTime, apLat, apLon))

        # Insert new data
        cur.execute("INSERT INTO rbsp_apogee VALUES(?,?,?,?,?)", (dTime.date(), dTime, apLat, apLon, sat))
        if radDict:
            for rad,dist,beam in zip(radDict['radars'],radDict['dist'],radDict['beam']):
                cur.execute("INSERT INTO rbsp_apogee_radars VALUES(?,?,?,?,?,?,?)", (dTime.date(), dTime, apLat, apLon, rad.code[0], beam, dist))


if __name__ == '__main__':
    """
Command-line call
    """
    import sys

    if len(sys.argv) == 1: sdate = datetime.utcnow()

    if len(sys.argv) >= 2:
        sdate = datetime(int(sys.argv[1][0:4]),
                        int(sys.argv[1][4:6]),
                        int(sys.argv[1][6:8]))

    if len(sys.argv) >= 3:
        edate = datetime(int(sys.argv[2][0:4]),
            			int(sys.argv[2][4:6]),
            			int(sys.argv[2][6:8]))
    else: edate = sdate

    # If necessary, recreate the database from scratch 
    # (only if you know what you are doing..)
    # createDb()

    date = sdate
    dates = [date]
    while date < edate:
    	date = dates[-1]+timedelta(days=1)
    	dates.append(date)

    """ Start editing here """
    for date in dates:
        """ Uncomment this line to (re)generate footpoints from RBSP orbit coordinates (GEO) """
        trace(date=date)
        """ Set this to False if you do not want to update the DB """
        saveDb = True
        """ Set this to True if you do not want to plot (can still write to DB) """
        noPlot = False
        """ Uncomment this line to (re)generate RBSP footpoints plots """
    	plot(date=date, saveDb=saveDb, noPlot=noPlot)
    	print '{} plotted.'.format(date.date())
