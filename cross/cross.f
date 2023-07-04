c---------------------------------------------------------------------
      program cross
c     markov model cross-validation
c---------------------------------------------------------------------
c
      include 'input.h'

      real ee(nx1,nc),tt(n1,nc),aa(nc,nc,12),zz(13,nx1,n1),dp(13,3,n1)
      real xy(ix1,jy1)
     
      character*10 f1
      data f1/'1-8x20    '/

c      print *,'read in # of meofs'
c      read (5,*) nc
       write(f1(1:3),'(a3)') '1-8'
       write(f1(4:8),'(a5)') '_s50s'
c
c**** open input files
c
c***** meof eigenvectors AND pcs here
      open(unit=28,file='/d6/bxw2101/model_files/fortran_inputs/meofs',
     %  form='unformatted',access='direct',recl=nxt*4)

      open(unit=29,file='/d6/bxw2101/model_files/fortran_inputs/pcs',
     %  form='unformatted',access='direct',recl=nt*4)    
        

 
c**** open output files
c     open(unit=30,file='map_'//f1//'',
c    %  form='unformatted',access='direct',recl=nx1*4)
c      open(unit=31,file='dp_'//f1//'',
c     %  form='unformatted',access='direct',recl=3*4)
 
c**** read in meofs and pcs
        do m=1,nc
           read(28,rec=m) (ee(i,m),i=1,nx1)
           read(29,rec=m) (tt(i,m),i=1,n1)
        enddo

      do it=1,nt

c***** construct transition matrices
        call qa(tt,aa,it)

c***** construct the cross validation matrix for that one year
        call mk(tt,ee,aa,zz,it)
      enddo

      open(unit=98, file="cross_matrix", form='unformatted')
      write(98) zz

      stop
      end

c---------------------------------------------------------------------
	subroutine qa(t,aa,itc)
c---------------------------------------------------------------------
	include 'input.h'

	real t(nt,nc),x(nc,nty,12),a(nc,nc),w(n1),ww(nty,12),
     %   d(nc,nc),c(nc,nc),di(nc,nc),aa(nc,nc,12),wk(nc**2+3*nc)

	if(itc.le.nt-12) then
	   do j=1,nt
	      if(j.gt.itc.and.j.lt.itc+12) then
		 w(j)=0.
	      else
		 w(j)=1.
	      endif
	   enddo
	else
	   do j=1,nt
	      if(j.gt.itc) then
		 w(j)=0.
	      else
		 w(j)=1.
	      endif
	   enddo
	endif

	do k=1,nty
	do j=1,12
	   do i=1,nc
	      x(i,k,j)=t(j+(k-1)*12,i)
	   enddo
	   ww(k,j)=w(j+(k-1)*12)
	enddo
	enddo

	do nsea=1,12

	   do i=1,nc
	      do j=1,nc

 		 sum=0.
		 pp=0.
 		 if(nsea.eq.12) then
 		    do it=1,nty-1
  		       sum=sum+x(i,it,nsea)*x(j,it,nsea)
     $                    *ww(it,nsea)*ww(it+1,1)
		       pp=pp+ww(it,nsea)*ww(it+1,1)
 		    enddo
		 else
 		    do it=1,nty-1
  		       sum=sum+x(i,it,nsea)*x(j,it,nsea)
     $                    *ww(it,nsea)*ww(it,nsea+1)
		       pp=pp+ww(it,nsea)*ww(it,nsea+1)
 		    enddo
		 endif
 		 d(i,j)=sum/pp
 
 		 sum=0.
		 pp=0.
 		 if(nsea.eq.12) then
 		    do it=1,nty-1
 		       sum=sum+x(i,it+1,1)*x(j,it,nsea)
     $		          *ww(it+1,1)*ww(it,nsea)
		       pp=pp+ww(it+1,1)*ww(it,nsea)
      		    enddo	
 		 else
 		    do it=1,nty-1
 		       sum=sum+x(i,it,nsea+1)*x(j,it,nsea)
     $		          *ww(it,nsea)*ww(it,nsea+1)
		       pp=pp+ww(it,nsea)*ww(it,nsea+1)
 		    enddo
 		 endif
 		 c(i,j)=sum/pp

	      enddo
	   enddo

           idgt=0
           call linv2f(d,nc,nc,di,idgt,wk,ier)

	   do i=1,nc
	      do j=1,nc
	         sum=0.
	         do k=1,nc
		    sum=sum+d(i,k)*di(k,j)
	         enddo
	         a(i,j)=sum
	      enddo
	   enddo
c	   print *, ier
           call vmulff(c,di,nc,nc,nc,nc,nc,a,nc,ier)

	   do i=1,nc
	      do j=1,nc
		 aa(i,j,nsea)=a(i,j)
	      enddo
	   enddo

	enddo

c	print *, itc, pp

	return
	end

c---------------------------------------------------------------------
        subroutine mk(t,g,aa,zz,itc)
c---------------------------------------------------------------------
	include 'input.h'

        real aa(nc,nc,12),tx0(nc),c(nc),a(nc,nc),
     $    t(nt,nc),g(nx1,nc),zz(nl+1,nx1,nt)
        
c        print *, itc
         do i=1,nc
            tx0(i)=t(itc,i)
         enddo

         zz_add = -1
         do lnl=0,nl
           zz_add = zz_add + 1
c               zz_add is ln1.
           zz_i = itc + zz_add
            
         if(int(zz_i).LE.nt) then
           if(lnl.eq.0) goto 777
              jm=mod(itc+lnl-1,12)
              if(jm.eq.0) jm=12
              do i=1,nc
                 do j=1,nc
                    a(i,j)=aa(i,j,jm)
                 enddo
              enddo
              call vmulff(a,tx0,nc,nc,1,nc,nc,c,nc,ier)
              do ix=1,nc
                 tx0(ix)=c(ix)
              enddo
 777       continue
           jm=mod(itc+lnl,12)
           if(jm.eq.0) jm=12

           do i=1,nx1
              sum=0.
              do j=1,nc
                 sum=sum+g(i,j)*tx0(j)
              enddo
               zz(int(zz_add+1),i,int(zz_i))=sum
c              zz(lnl+1,i,itc)=sum
           enddo
        endif

        enddo

        return
        end































