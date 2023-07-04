c---------------------------------------------------------------------
      program hcast
c     markov model hindcast
c---------------------------------------------------------------------
c
      include 'input.h'

      real ee(n2,nc),tt(n1,nc),zz(nl+1,nxt,nt),aa(nc,nc,12)
      real xy(ix1,jy1),dp(13,3,nt),to(nxt,nt),s(nv)
     
      character*13 vn(8)
      character*11 f1
      data f1/'1-8x20    '/
      data vn/
     $  'ice_a','sat_a_sm','h300_a_sm','u300_a_sm','v300_a_sm',
     $  'slp_a_sm','u10_a_sm','v10_a_sm'/
c      data s/3.774071,3.233883,9.703116,2.707891,2.173748/
       

c      print *,'read in # of meofs'
c      read (5,*) nc
       write(f1(1:3),'(a3)') '1-5'
       write(f1(4:8),'(a5)') '_s50s'
c
c**** open input files
c
c***** meof eigenvectors AND pcs here
      open(unit=28,file='/d6/bxw2101/model_files/fortran_inputs/meofs',
     %  form='unformatted',access='direct',recl=nxt*4)

      open(unit=29,file='/d6/bxw2101/model_files/fortran_inputs/pcs',
     %  form='unformatted',access='direct', recl=nt*4)


c IDK WHAT THESE R.... NOT RLY USEDHERE.  BUT IM JUST LEAVIN EM.
c**** open output files
c      open(unit=30,file='map_'//f1//'',
c     %  form='unformatted',access='direct',recl=nxt*4)
c      open(unit=31,file='dp_'//f1//'',
c     %  form='unformatted',access='direct',recl=3*4)
 
c**** read in meofs and pcs
        do m=1,nc
           read(28,rec=m) (ee(i,m),i=1,n2)
c                do i=1, n2
c                  print *, ee(i, m)
c                enddo
        enddo
c  
        do m=1,nc
           read(29, rec=m) (tt(i,m),i=1,n1)
c               do i=1, n1
c                 print *, tt(i,m)
c               enddo
        enddo

c       Notes after reading in the data:
c       - meofs (ee) are read in properly. 100% good, memory is good.
c               
c       - pcs   (tt) are not read in properly, because NT = # months + 1
c                    for some reason. WHY? WE MUST FIX THIS.


c       - so, we change the NT to just the # months in the input.h file.

c***** construct transition matrices
      call qa(tt,aa)

        open(unit=99,file='trans_matrices',form='unformatted')
        write(99) aa

c just wanna print aa to see if it shows similar values as python
                

c***** do hindcast
      call mk(tt,ee,aa,zz)

        open(unit=98,file='hcast_matrix', form='unformatted')
        write(98) zz

      stop
      end

c---------------------------------------------------------------------
	subroutine qa(t,aa)
c---------------------------------------------------------------------
	include 'input.h'

	real t(nt,nc),x(nc,nty,12),a(nc,nc),
     %   d(nc,nc),c(nc,nc),di(nc,nc),aa(nc,nc,12),wk(nc**2+3*nc)

	do k=1,nty
	do j=1,12
	   do i=1,nc
	      x(i,k,j)=t(j+(k-1)*12,i)

c              after changing NT in the input file to just # months
c              rather than # months + 1, things seem to work.
c              print *, x(i,k,j)

	   enddo
	enddo
	enddo

	do nsea=1,12

	   do i=1,nc
	      do j=1,nc

 		 sum=0.
 		 do it=1,nty-1
  		    sum=sum+x(i,it,nsea)*x(j,it,nsea)
 		 enddo
 		 d(i,j)=sum/(nty-1)
c just tryna print the d's here
c                print *, d(i, j)
 
 		 sum=0.
 		 if(nsea.eq.12) then
 		    do it=1,nty-1
 		       sum=sum+x(i,it+1,1)*x(j,it,nsea)
 		    enddo	
 		 else
 		    do it=1,nty-1
 		       sum=sum+x(i,it,nsea+1)*x(j,it,nsea)
 		    enddo
 		 endif
 		 c(i,j)=sum/(nty-1)

c		 sum1=0.
c		 sum2=0.
c		 if(nsea.eq.1) then
c		 do it=2,nty-1
c		    sum1=sum1+x(i,it,1)*x(j,it,1)
c     $			   +x(i,it-1,12)*x(j,it-1,12)
c     $			   +x(i,it,2)*x(j,it,2)
c		    sum2=sum2+x(i,it,1)*x(j,it,2)
c     $			   +x(i,it-1,12)*x(j,it,1)
c     $			   +x(i,it,2)*x(j,it,3)
c		 enddo
c		 else if(nsea.eq.12) then
c		 do it=2,nty-1
c		    sum1=sum1+x(i,it,12)*x(j,it,12)
c     $			   +x(i,it,11)*x(j,it,11)
c     $			   +x(i,it+1,1)*x(j,it+1,1)
c		    sum2=sum2+x(i,it,12)*x(j,it+1,1)
c     $			   +x(i,it,11)*x(j,it,12)
c     $			   +x(i,it+1,1)*x(j,it+1,2)
c		 enddo
c		 else if(nsea.eq.11) then
c		 do it=2,nty-1
c		    sum1=sum1+x(i,it,11)*x(j,it,11)
c     $			   +x(i,it,10)*x(j,it,10)
c     $			   +x(i,it,12)*x(j,it,12)
c		    sum2=sum2+x(i,it,11)*x(j,it,12)
c     $			   +x(i,it,10)*x(j,it,11)
c     $			   +x(i,it,12)*x(j,it+1,1)
c		 enddo
c		 else
c		 do it=2,nty-1
c		    sum1=sum1+x(i,it,nsea)*x(j,it,nsea)
c     $			   +x(i,it,nsea-1)*x(j,it,nsea-1)
c     $			   +x(i,it,nsea+1)*x(j,it,nsea+1)
c		    sum2=sum2+x(i,it,nsea)*x(j,it,nsea+1)
c     $			   +x(i,it,nsea-1)*x(j,it,nsea)
c     $			   +x(i,it,nsea+1)*x(j,it,nsea+2)
c		 enddo
c		 endif
c		 d(i,j)=sum1/(nty-2)/3.
c		 c(i,j)=sum2/(nty-2)/3.

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

	return
	end

c---------------------------------------------------------------------
        subroutine mk(t,g,aa,zz)
c---------------------------------------------------------------------
        include 'input.h'

        real aa(nc,nc,12),tx0(nc),c(nc),a(nc,nc),
     $    t(nt,nc),g(nxt,nc),zz(nl+1,nxt,nt)

c fortran is kinda weird, let's set actual variables here for the zz_i.
        zz_base = 0
        do it=1,nt
           zz_base = zz_base + 1

           do i=1,nc
              tx0(i)=t(it,i)
           enddo

         zz_add = -1
         do lnl=0,nl
           zz_add = zz_add + 1
           
c          zz_i is the index of the zzarray: it + ln1.
           zz_i = zz_base + zz_add

c new if statement here.
           if(int(zz_i).LE.nt) then

           if(lnl.eq.0) goto 777
              jm=mod(it+lnl-1,12)
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
           jm=mod(it+lnl,12)
           if(jm.eq.0) jm=12

           do i=1,nxt
              sum=0.
              do j=1,nc
                 sum=sum+g(i,j)*tx0(j)
              enddo
              zz(int(zz_add+1),i,int(zz_i))=sum
c             zz(int(zz_add+1),i,int(zz_base))=sum (OG CODE)
           enddo

c new endif statement here.           
          endif
                
         enddo

        enddo
        return
        end























