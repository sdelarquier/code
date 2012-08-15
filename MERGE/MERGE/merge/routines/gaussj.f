      subroutine GAUSSJ(a,n,np,b,m,mp)
c     ********************************

c*******************************************************************
c This routine solves a linear equations system by Gauss-Jordan 
c elimination (A.x = b). A is an input matrix of N*N elements, stored
c in an array of physical dimensions NP*NP. B is an input matrix of
c N*M containing the M right-hand side vectors, stored in an array of
c physical dimensions NP*MP. On output, A is replaced by its matrix
c inverse, and B is replaced by the corresponding set of solution 
c vectors.
c
c	From Numerical Recepies (p.28)
c
c	$Log:	gaussj.f,v $
c Revision 1.1  94/09/01  16:13:47  16:13:47  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c*******************************************************************

      parameter (nmax=10)
      dimension a(np,np),b(np,mp),ipiv(nmax),indxr(nmax),indxc(nmax)

      do j=1,n
        ipiv(j) = 0
      enddo
      do i=1,n
        big = 0.
        do j=1,n
          if(ipiv(j).ne.1) then
            do k=1,n
              if(ipiv(k).eq.0) then
                if(abs(a(j,k)).ge.big) then
                  big = abs(a(j,k))
                  irow = j
                  icol = k
                endif
              else if(ipiv(k).gt.1) then
                pause 'Singular matrix'
              endif
            enddo
          endif
        enddo
        ipiv(icol) = ipiv(icol) + 1
        if(irow.ne.icol) then
          do l=1,n
            dum = a(irow,l)
            a(irow,l) = a(icol,l)
            a(icol,l) = dum
          enddo
          do l=1,m
            dum = b(irow,l)
            b(irow,l) = b(icol,l)
            b(icol,l) = dum
          enddo
        endif
        indxr(i) = irow
        indxc(i) = icol
        if(a(icol,icol).eq.0.) pause 'Singular matrix.'
        pivinv = 1./a(icol,icol)
        a(icol,icol) = 1.
        do l=1,n
          a(icol,l) = a(icol,l)*pivinv
        enddo
        do l=1,m
          b(icol,l) = b(icol,l)*pivinv
        enddo
        do ll=1,n
          if(ll.ne.icol) then
            dum = a(ll,icol)
            a(ll,icol) = 0.
            do l=1,n
              a(ll,l) = a(ll,l) - a(icol,l)*dum
            enddo
            do l=1,m
              b(ll,l) = b(ll,l) - b(icol,l)*dum
            enddo
          endif
        enddo
      enddo
      do l=n,1,-1
        if(indxr(l).ne.indxc(l)) then
          do k=1,n
            dum = a(k,indxr(l))
            a(k,indxr(l)) = a(k,indxc(l))
            a(k,indxc(l)) = dum
          enddo
        endif
      enddo
      return
      end
