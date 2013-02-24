import numpy as np
import h5py
bag = h5py.File('mlh101117g.002.hdf5')
bag.values()
#[Out]# <h5py._hl.base.ValueView object at 0x107f27190>
bag.items()
#[Out]# <h5py._hl.base.ItemView object at 0x107f27110>
bag.name
#[Out]# '/'
bag['/'].items()
#[Out]# <h5py._hl.base.ItemView object at 0x107f27710>
list(bag)
#[Out]# ['Data', 'Metadata']
bag['/Data'].items()
#[Out]# <h5py._hl.base.ItemView object at 0x107f27850>
bag.close
#[Out]# <bound method File.close of <Closed HDF5 file>>
bag.close()
logstart?
# Tue, 05 Jun 2012 15:15:06
ls -l
# Tue, 05 Jun 2012 15:15:15
!head testlog.py
# Tue, 05 Jun 2012 15:18:39
!wget http://surveys.ngdc.noaa.gov/mgg/NOS/coast/W00001-W02000/W00215/BAG/W00215_MB_1m_MLLW_1of1.bag.gz
# Tue, 05 Jun 2012 15:19:38
exit
# Tue, 05 Jun 2012 15:30:23
!head testlog.py
# Tue, 05 Jun 2012 15:30:30
!wget http://surveys.ngdc.noaa.gov/mgg/NOS/coast/W00001-W02000/W00215/BAG/W00215_MB_1m_MLLW_1of1.bag.gz
# Tue, 05 Jun 2012 16:34:31
!gunzip *.gz
# Tue, 05 Jun 2012 16:34:51
ls -l
# Tue, 05 Jun 2012 16:35:08
!file W00215_MB_1m_MLLW_1of1.bag
# Tue, 05 Jun 2012 16:35:23
!md5sum W00215_MB_1m_MLLW_1of1.bag
# Tue, 05 Jun 2012 16:36:24
history
# Tue, 05 Jun 2012 16:36:31
import h5py
# Tue, 05 Jun 2012 16:37:55
bag = h5py.File('W00215_MB_1m_MLLW_1of1.bag')
# Tue, 05 Jun 2012 16:38:07
bag.name
#[Out]# '/'
# Tue, 05 Jun 2012 16:38:11
bag.items()
#[Out]# <h5py._hl.base.ItemView at 0x1038167d0>
# Tue, 05 Jun 2012 16:39:39
!md5 W00215_MB_1m_MLLW_1of1.bag
# Tue, 05 Jun 2012 16:41:15
list(bag)
#[Out]# ['BAG_root']
# Tue, 05 Jun 2012 16:41:40
bag.items()[0][1]
# Tue, 05 Jun 2012 16:41:58
bag.close()
# Tue, 05 Jun 2012 16:42:01
exit
