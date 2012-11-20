#!/bin/bash
import tsyganenko as ts
import numpy as np
import matplotlib as mp
import subprocess
from mpl_toolkits.basemap import Basemap
from pylab import *
from datetime import datetime as dtmod
import sys
sys.path.append('/davitpy')
from pydarn.plot import overlayFov
from pydarn.radar import network


# <codecell>

def plotFpProgress(lons, lats, lon0=0, hemi='N'):
    sign = 1 if hemi.lower()[0] == 'n' else -1
    llcrnrlon = lon0 - 45.
    urcrnrlon = lon0 + 135.

    #myMap = Basemap(projection=projection, boundinglat=sign*15, lon_0=sign*lon0, resolution='l')
    try:
        myMap = Basemap(projection='stere', lat_0=sign*90, lon_0=lon0, resolution='l', 
                    llcrnrlat=sign*20, llcrnrlon=llcrnrlon,
                    urcrnrlat=sign*20, urcrnrlon=urcrnrlon)
    except:
        myMap = Basemap(projection='stere', lat_0=sign*90, lon_0=lon0+1e-4, resolution='l', 
                    llcrnrlat=sign*20, llcrnrlon=llcrnrlon,
                    urcrnrlat=sign*20, urcrnrlon=urcrnrlon)
    myMap.drawcoastlines(color='.5')
    myMap.fillcontinents(color='.8')
    # draw parallels and meridians.
    myMap.drawparallels(np.arange(-80.,81.,20.), color='.6')
    myMap.drawmeridians(np.arange(-180.,181.,20.), color='.6')
    #myMap.drawmeridians([0], color='r')
    myMap.drawmapboundary()
    # Draw FP
    x, y = myMap(lons, lats)
    myMap.scatter(x, y, color='r', zorder=2, s=5)
    myMap.scatter(x[-1], y[-1], color='k', zorder=2, s=50, facecolors='none', linewidth=2)
    
    return myMap

# <codecell>

def gridGsw(trace, ind=0):
    # This has to be called first
    ts.tsygFort.recalc_08(trace.datetime[ind].year, trace.datetime[ind].timetuple().tm_yday,
                        trace.datetime[ind].hour, trace.datetime[ind].minute, trace.datetime[ind].second,
                        trace.vswgse[0], trace.vswgse[1], trace.vswgse[2])
    
    # Generate equator
    eqlon = arange(0,361,10)
    eqXgsw = zeros(len(eqlon))
    eqYgsw = zeros(len(eqlon))
    eqZgsw = zeros(len(eqlon))
    for ip in xrange(len(eqlon)):
        r, theta, phi, xgeo, ygeo, zgeo = ts.tsygFort.sphcar_08(
                                            1., radians(90.), radians(eqlon[ip]),
                                            0., 0., 0.,
                                            1)
        xgeo, ygeo, zgeo, xgsw, ygsw, zgsw = ts.tsygFort.geogsw_08(
                                            xgeo, ygeo, zgeo,
                                            0. ,0. ,0. ,
                                            1)
        
        eqXgsw[ip] = xgsw
        eqYgsw[ip] = ygsw
        eqZgsw[ip] = zgsw
    
    # Generate meridians
    merlat = arange(-90,91,10)
    merlon = arange(0,361,60)
    merXgsw = zeros((len(merlat),7))
    merYgsw = zeros((len(merlat),7))
    merZgsw = zeros((len(merlat),7))
    for im,lon in enumerate(merlon):
        for ip in xrange(len(merlat)):
            r, theta, phi, xgeo, ygeo, zgeo = ts.tsygFort.sphcar_08(
                                                1., radians(90.-merlat[ip]), radians(lon),
                                                0., 0., 0.,
                                                1)
            xgeo, ygeo, zgeo, xgsw, ygsw, zgsw = ts.tsygFort.geogsw_08(
                                                xgeo, ygeo, zgeo,
                                                0. ,0. ,0. ,
                                                1)
            
            merXgsw[ip,im] = xgsw
            merYgsw[ip,im] = ygsw
            merZgsw[ip,im] = zgsw
    
    return eqXgsw, eqYgsw, eqZgsw, merXgsw, merYgsw, merZgsw, merlon, merlat, eqlon

# <codecell>

def plotTraceGsw(trace, proj='xz', ind=0, xylim=8):
    ax = trace.plot(onlyPts=ind, proj=proj, disp=False, showPts=True)
    
    eqXgsw, eqYgsw, eqZgsw, merXgsw, merYgsw, merZgsw, merlon, merlat, eqlon = gridGsw(trace, ind=ind)
    if proj[0] == 'x':
        xxEq = eqXgsw
        xxMer = merXgsw
        xdir = [1,0,0]
    elif proj[0] == 'y':
        xxEq = eqYgsw
        xxMer = merYgsw
        xdir = [0,1,0]
    elif proj[0] == 'z':
        xxEq = eqZgsw
        xxMer = merZgsw
        xdir = [0,0,1]
    if proj[1] == 'x':
        yyEq = eqXgsw
        yyMer = merXgsw
        ydir = [1,0,0]
    elif proj[1] == 'y':
        yyEq = eqYgsw
        yyMer = merYgsw
        ydir = [0,1,0]
    elif proj[1] == 'z':
        yyEq = eqZgsw
        yyMer = merZgsw
        ydir = [0,0,1]
    sign = 1 if -1 not in cross(xdir,ydir) else -1
    if 'x' not in proj:
        indsEq = sign*eqXgsw < 0
        indsMer = sign*merXgsw < 0
    elif 'y' not in proj:
        indsEq = sign*eqYgsw < 0
        indsMer = sign*merYgsw < 0
    elif 'z' not in proj:
        indsEq = sign*eqZgsw < 0
        indsMer = sign*merZgsw < 0
    ax.plot(ma.masked_array(xxEq, mask=indsEq), ma.masked_array(yyEq, mask=indsEq), color='.1')
    ax.plot(ma.masked_array(xxMer, mask=indsMer), ma.masked_array(yyMer, mask=indsMer), color='.1')
    #im = 0
    #ax.plot(ma.masked_array(xxMer[:,im], mask=indsMer[:,im]), ma.masked_array(yyMer[:,im], mask=indsMer[:,im]), color='r')
    
    #ax.text(-7, 7, traceA.datetime[ind].strftime('%H:%M UT'), size=14)
    ax.arrow(7, 6.5, -3, 0, head_width=0.4, head_length=0.5, fc='k', ec='k')
    ax.text(4, 7, 'SOLAR WIND', size=14)
    
    ax.set_xlim([-xylim,xylim])
    ax.set_ylim([-xylim,xylim])
    
    return ax

# <codecell>

def plotPanels(ind=0, start=0):
    lon0 = -(traceA.datetime[ind].hour + traceA.datetime[ind].minute/60.)*360./24. + 90.
    
    # Plot GSW orbit
    ax1 = fig.add_subplot(221)
    cla()
    plotTraceGsw(traceA, proj='xz', ind=ind, xylim=8)
    ax2 = fig.add_subplot(222)
    cla()
    plotTraceGsw(traceA, proj='xy', ind=ind, xylim=8)
    
    # NH
    ax3 = fig.add_subplot(223)
    cla()
    m1 = plotFpProgress(traceA.lonNH[start:ind+1], traceA.latNH[start:ind+1], lon0=lon0, hemi="N")
    m1.nightshade(traceA.datetime[ind])
    
    # SH
    ax4 = fig.add_subplot(224)
    cla()
    m2 = plotFpProgress(traceA.lonSH[start:ind+1], traceA.latSH[start:ind+1], lon0=lon0, hemi="S")
    m2.nightshade(traceA.datetime[ind])
    #ax4.set_xlim(ax4.get_xlim()[::-1])
    #ax4.set_ylim(ax4.get_ylim()[::-1])
    
    # Find L-shell
    L = sqrt(traceA.xTrace[ind,:]**2 + traceA.yTrace[ind,:]**2 + traceA.zTrace[ind,:]**2).max()
    ax1.text(0, -7.5, 'L = {:2.1f}'.format(L), size=16, horizontalalignment='center')
    ax2.text(0, -7.5, 'L = {:2.1f}'.format(L), size=16, horizontalalignment='center')
    ax1.text(9, -9, traceA.datetime[ind].strftime('%H:%M UT'), size=16, va="center", ha="center")

# <codecell>



if __name__ == '__main__':
    """
Command-line call
    """
    import sys, getopt
    import os

    inputfile = ''
    outputfile = ''
    try:
      opts, args = getopt.getopt(sys.argv[1:],"hd:",["date="])
    except getopt.GetoptError:
      print 'rbsp_movie.py [-d <date>]'
      sys.exit(2)
    for opt, arg in opts:
      if opt == '-h':
         helpstr =  ''' Generate Orbit/FootPoint movie of the RBSP-A spacecraft -
            Use:
               rbsp_movie.py [options]
            Options:
               -h
                   help
               -d<date>, --date=<date>
                   date in YYYYMMDD format
            '''
         print helpstr
         sys.exit()
      elif opt in ("-d", "--date"):
         dateIn = arg
    print 'Date is %s' % dateIn

    try:
        date = dtmod(dateIn[0:4], dateIn[4:6], dateIn[6:])
    except:
        date = dtmod.utcnow()

    rbspdata = '/home/sebastien/Documents/code/rbsp/'
    traceA = ts.tsygTrace(filename=rbspdata+'trace.{}.A.dat'.format(date.strftime('%Y%m%d')))
    traceB = ts.tsygTrace(filename=rbspdata+'trace.{}.B.dat'.format(date.strftime('%Y%m%d')))

    strdate = date.strftime('%Y%m%d')
    print date.strftime('%Y-%b-%d')

    path = '/home/sebastien/Desktop/rbsp.orbit'

    fig = figure(figsize=(15,15))
    start = 0
    ip = start
    while ip < len(traceA.lat):
        plotPanels(ind=ip, start=start)
        
        if ip == start: fig.tight_layout()
        fig.savefig('{}/rbsp.orbit.{}.{:06d}.png'.format(path, strdate, ip))
        print '--> '+traceA.datetime[ip].strftime('%H:%M UT')
        
        ip += 1
    fig.clf()

    # make animated gif:
    res = subprocess.call(['convert -delay 20 {}/rbsp.orbit.*.png \
        /var/www/img/rbsp.orbit.{}.gif'.format(path, date.strftime('%Y%m%d'))], 
        shell=True)
