# Copyright (C) 2012  VT SuperDARN Lab
# Full license can be found in LICENSE.txt
"""
*********************
**Module**: utils.profileUtils
*********************
This module is a simple wrapper for cProfile

**Functions**:
        * :func:`utils.profileUtils.run`: Run and display a profile

"""
import cProfile
import pstats

def run(fun):
    """Run and display a profile of the function FUN

    **Args**:
        * **fun** (str): function to be profiled
    **Returns**:
        * **p** (str): pstat output
    """
    from os import remove
    
    # Generate profile
    cProfile.run(fun,'funprof')
    # Read profile
    p = pstats.Stats('funprof')
    pOut = p
    # Print profile in the most useful format
    p.strip_dirs().sort_stats('cumulative').print_callees()
    # Remove temp file
    remove('funprof')
    
    return pOut
