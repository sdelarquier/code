pro mlh_read, data, altitude, juls

restore, '~/Documents/code/milh/mlh_templ.sav'
data = read_ascii('~/Documents/code/milh/mlh101117g.002.txt', template=sTempl)

Re = 6370.
altitude = sqrt( data.range^2 + Re^2 + 2.*data.range*Re*sin(data.el1*!dtor) ) - Re 
juls = julday(data.month, data.day, data.year, data.hour, data.min, data.sec)

end