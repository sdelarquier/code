c $Log: mrg_name.f,v $
c Revision 1.1  1996/09/19 13:34:50  senior
c Initial revision
c

	character*50 function MRG_NAME(name)
	character name*(*)
	character*80 prefix
	call getenv_('SD_MRGOPEN_PATH', prefix)
	mrg_name = prefix(:lnblnk_(prefix)) // '/' // name(:lnblnk_(name))
	return
	end


