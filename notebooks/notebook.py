# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

from pylab import *

#draw a vector
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import proj3d

class Arrow3D(FancyArrowPatch):
    def __init__(self, xs, ys, zs, *args, **kwargs):
        FancyArrowPatch.__init__(self, (0,0), (0,0), *args, **kwargs)
        self._verts3d = xs, ys, zs

    def draw(self, renderer):
        xs3d, ys3d, zs3d = self._verts3d
        xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, renderer.M)
        self.set_positions((xs[0],ys[0]),(xs[1],ys[1]))
        FancyArrowPatch.draw(self, renderer)

# <codecell>

from matplotlib.font_manager import FontProperties
fig = figure(figsize=(10,10))
ax = fig.add_subplot(111, projection='3d')

# Main and Interf array
O = [0,0,0]
ax.plot3D([O[0]-6,O[0]+6],[O[1],O[1]],[O[2],O[2]],'k')
dsep = 6.
C = [0,dsep,0]
ax.plot3D([C[0]-2,C[0]+2],[C[1],C[1]],[C[2],C[2]],'k')

# Bore sight
a1 = Arrow3D([0,0],[O[1]-2,C[1]+4],[0,0], 
    mutation_scale=20, lw=1, arrowstyle="-|>", color="k", ls='dashed')
ax.add_artist(a1)

# Beam direction
theta = -radians(40)
a2 = Arrow3D([0,8*sin(theta)],[0,8*cos(theta)],[0,0], 
    mutation_scale=20, lw=1, arrowstyle="-|>", color="b", ls='dashed')
ax.add_artist(a2)

# Project interf onto beam direction
B = [dsep*cos(theta)*sin(theta),dsep*cos(theta)*cos(theta),0]
ax.plot3D([C[0],B[0]], [C[1],B[1]], [C[2],B[2]], 'k:')

# Elevation direction
el = radians(30)
ax.plot3D([0,8*cos(el)*sin(theta)],[0,8*cos(el)*cos(theta)],[0,8*sin(el)],'r-')
ax.plot3D([0,4*cos(el)*sin(theta)],[dsep,dsep+4*cos(el)*cos(theta)],[0,4*sin(el)],'r-')
ax.plot3D([B[0],B[0]+5*cos(el)*sin(theta)],[B[1],B[1]+5*cos(el)*cos(theta)],[0,5*sin(el)],'r-')

# Mark k vector
ael1 = Arrow3D([8*cos(el)*sin(theta),6*cos(el)*sin(theta)],
    [8*cos(el)*cos(theta),6*cos(el)*cos(theta)],
    [8*sin(el),6*sin(el)], mutation_scale=20, lw=1, arrowstyle="-|>", color="r")
ax.add_artist(ael1)
ael2 = Arrow3D([B[0]+4*cos(el)*sin(theta),B[0]+3*cos(el)*sin(theta)],
    [B[1]+4*cos(el)*cos(theta),B[1]+3*cos(el)*cos(theta)],
    [4*sin(el),3*sin(el)], mutation_scale=20, lw=1, arrowstyle="-|>", color="r")
ax.add_artist(ael2)
ael3 = Arrow3D([5*cos(el)*sin(theta),3*cos(el)*sin(theta)],
    [dsep+5*cos(el)*cos(theta),dsep+3*cos(el)*cos(theta)],
    [5*sin(el),3*sin(el)], mutation_scale=20, lw=1, arrowstyle="-|>", color="r")
ax.add_artist(ael3)

# Project Elevation onto beam direction
A = [dsep*cos(theta)*cos(el)*cos(el)*sin(theta), 
     dsep*cos(theta)*cos(el)*cos(el)*cos(theta),
     dsep*cos(theta)*cos(el)*sin(el)]
ax.plot3D([C[0],A[0]], [C[1],A[1]], [C[2],A[2]], 'k:')
ax.plot3D([B[0],A[0]], [B[1],A[1]], [B[2],A[2]], 'k:')

# Mark corner angles
# At B first
Bnorm = sqrt(B[0]**2 + B[1]**2 + B[2]**2)*4
Bun = [b/Bnorm for b in B]
BC = map(lambda b,c:c-b, B, C)
BCnorm = sqrt(BC[0]**2 + BC[1]**2 + BC[2]**2)*4
BCun = [bc/BCnorm for bc in BC]
CBun = [-bc for bc in BCun]
Bperp = map(sum,zip(B,Bun,BCun))
ax.plot3D([B[0]+Bun[0],Bperp[0]], [B[1]+Bun[1],Bperp[1]], [B[2]+Bun[2],Bperp[2]], 'k-')
ax.plot3D([B[0]+BCun[0],Bperp[0]], [B[1]+BCun[1],Bperp[1]], [B[2]+BCun[2],Bperp[2]], 'k-')
BA = map(lambda b,a:a-b, B, A)
BAnorm = sqrt(BA[0]**2 + BA[1]**2 + BA[2]**2)*4
BAun = [ba/BAnorm for ba in BA]
BAperp = map(sum,zip(B,BCun,BAun))
ax.plot3D([B[0]+BAun[0],BAperp[0]], [B[1]+BAun[1],BAperp[1]], [B[2]+BAun[2],BAperp[2]], 'k-')
ax.plot3D([B[0]+BCun[0],BAperp[0]], [B[1]+BCun[1],BAperp[1]], [B[2]+BCun[2],BAperp[2]], 'k-')
# Then at A
Anorm = sqrt(A[0]**2 + A[1]**2 + A[2]**2)*4
Aun = [a/Anorm for a in A]
ABun = [-ba for ba in BAun]
Aperp = map(sum,zip(A,Aun,ABun))
ax.plot3D([A[0]+Aun[0],Aperp[0]], [A[1]+Aun[1],Aperp[1]], [A[2]+Aun[2],Aperp[2]], 'k-')
ax.plot3D([A[0]-BAun[0],Aperp[0]], [A[1]-BAun[1],Aperp[1]], [A[2]-BAun[2],Aperp[2]], 'k-')
AC = map(lambda c,a:c-a, C, A)
ACnorm = sqrt(AC[0]**2 + AC[1]**2 + AC[2]**2)*4
ACun = [ac/ACnorm for ac in AC]
Anun = [-a for a in Aun]
ACperp = map(sum,zip(A,ACun,Anun))
ax.plot3D([A[0]+Anun[0],ACperp[0]], [A[1]+Anun[1],ACperp[1]], [A[2]+Anun[2],ACperp[2]], 'k-')
ax.plot3D([A[0]+ACun[0],ACperp[0]], [A[1]+ACun[1],ACperp[1]], [A[2]+ACun[2],ACperp[2]], 'k-')

# Mark angles
# First azimuth
taz = linspace(0,theta,20)
x = sin(taz)
y = cos(taz)
ax.plot3D(x, y, color='grey')
# Then elevation
tel = linspace(0,el,20)
x = 1.5*sin(taz[-1])*linspace(1,1,20)
y = 1.5*cos(taz[-1])*cos(tel)
z = 1.5*sin(tel)
ax.plot3D(x, y, z, color='grey')

# Set view
ax.view_init(25,60)
#ax.view_init(90,0)
ax.set_xlim3d([-4,4])
ax.set_ylim3d([-2,6])
ax.set_zlim3d([-4,4])
rcParams.update({'font.size': 14})

# Annotation
ax.text(7,.2,0,
    "Main array", 
    ha = 'left', va = 'center')
ax.text(3,dsep+.2,0,
    'Interferometer array', 
    ha = 'left', va = 'center')
ax.text(B[0]+12*Bun[0]+.2,B[1]+12*Bun[1]+.2,B[2]+12*Bun[2],
    "Beam direction", 
    ha = 'left', va = 'top')
ax.text(0,C[1]+4,.5,
    "Boresite", 
    ha = 'center', va = 'bottom')
ax.text(C[0]+14*Aun[0],C[1]+14*Aun[1],C[2]+14*Aun[2]-.2, 
    "k", 
    ha = 'left', va = 'top', 
    color='r', fontproperties = FontProperties(weight='bold'))
ax.text(B[0]+18*Aun[0],B[1]+18*Aun[1],B[2]+18*Aun[2]-.2,
    "k", 
    ha = 'left', va = 'top', 
    color='r', fontproperties = FontProperties(weight='bold'))
ax.text(O[0]+28*Aun[0],O[1]+28*Aun[1],O[2]+28*Aun[2]+.2, 
    "k", 
    ha = 'left', va = 'bottom', 
    color='r', fontproperties = FontProperties(weight='bold'))
ax.text(O[0]+.3,O[1]-.4,O[2], 
    "O", 
    ha = 'right', va = 'center')
ax.text(C[0],C[1],C[2]-.2, 
    "C", 
    ha = 'center', va = 'top')
ax.text(B[0]-.2,B[1]-.2,B[2]+.2,
    "B", 
    ha = 'left', va = 'bottom')
ax.text(A[0],A[1],A[2]+.2,
    "A", 
    ha = 'left', va = 'bottom')
ax.text((C[0]-O[0])/2.+.5,(C[1]-O[1])/2.,(C[2]-O[2])/2.,
    r"$d$", 
    bbox = dict(ec='w', fc='w'), 
    ha = 'center', va = 'center', color='grey',zorder=-1)
ax.text((B[0]-O[0])/2.+2*CBun[0],(B[1]-O[1])/2.+2*CBun[1],(B[2]-O[2])/2.+2*CBun[2],
    r"$d'$", 
    bbox = dict(ec='w', fc='w'), 
    ha = 'center', va = 'center', color='grey',zorder=-1)
ax.text((A[0]-O[0])/2.,(A[1]-O[1])/2.,(A[2]-O[2])/2.+.6,
    r'$\delta P$', 
    bbox = dict(ec='w', fc='w'), 
    ha = 'center', va = 'center', color='grey',zorder=-1)
ax.text(O[0]-.3,O[1]+1.3,O[2], 
    r'$\Phi$', size=18, 
    ha = 'center', va = 'center', color='grey')
ax.text(O[0]-1,O[1]+1.5,O[2]+.6, 
    r'$\Delta$', size=18,
    ha = 'center', va = 'center', color='grey')

# Mark length
al1 = Arrow3D([.5,.5], 
    [O[1],C[1]], 
    [0,0], 
    mutation_scale=20, lw=1, arrowstyle="<->", color="grey",zorder=-2)
ax.add_artist(al1)
al2 = Arrow3D([O[0]+2*CBun[0],B[0]+2*CBun[0]],
    [O[1]+2*CBun[1],B[1]+2*CBun[1]], 
    [O[2]+2*CBun[2],B[2]+2*CBun[2]], 
    mutation_scale=20, lw=1, arrowstyle="<->", color="grey",zorder=-2)
ax.add_artist(al2)
al3 = Arrow3D([O[0]+2*BAun[0],A[0]+2*BAun[0]], 
    [O[1]+2*BAun[1],A[1]+2*BAun[1]], 
    [O[2]+2*BAun[2],A[2]+2*BAun[2]], 
    mutation_scale=20, lw=1, arrowstyle="<->", color="grey",zorder=-2)
ax.add_artist(al3)
ax.set_axis_off()
savefig('elevation.pdf')

show()
