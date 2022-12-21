PROGRAM swfvm

   IMPLICIT NONE

   INTEGER              :: je,jk,ji,j1,j2
   INTEGER              :: irow,jcol

   DOUBLE PRECISION     :: fL           !: domain size
   DOUBLE PRECISION     :: fdx          !: resolution
   DOUBLE PRECISION     :: fdepth       !: depth                 
   DOUBLE PRECISION     :: fgrav        !: gravity acceleration      
   DOUBLE PRECISION     :: fcor0        !: Coriolis Parameter
   DOUBLE PRECISION     :: ftau0        !: wind stess [N/m**2]

   DOUBLE PRECISION     :: fdt          !: time step [s]
   DOUBLE PRECISION     :: ftime_total  !: simulation length [s]
   
   INTEGER              :: nel, nkn     !: elements, nodes
   INTEGER              :: nx,ny        !: cells along x,y
   INTEGER              :: nx2          !: simply nx+1
   INTEGER              :: nsteps       !: # of time steps

   INTEGER, ALLOCATABLE                 :: nfe(:,:)     !: nodes for element
   INTEGER, ALLOCATABLE                 :: neae(:,:)    !: elements around element

   DOUBLE PRECISION, ALLOCATABLE        :: xk(:),yk(:)  !: coordinates of nodes
   DOUBLE PRECISION, ALLOCATABLE        :: xe(:),ye(:)  !: coordinates of cell centers

   DOUBLE PRECISION, ALLOCATABLE        :: Uo(:),Un(:)
   DOUBLE PRECISION, ALLOCATABLE        :: Vo(:),Vn(:)
   DOUBLE PRECISION, ALLOCATABLE        :: ssho(:),sshn(:)

   INTEGER              :: irc,iunit

   !! Read namelist 
   NAMELIST /params/ fL,fdx,fdepth,fgrav,fcor0,ftau0,fdt,ftime_total

   OPEN (action='read', file='paramlist.str', iostat=irc, newunit=iunit)
   READ (nml=params, iostat=irc, unit=iunit)
   IF (irc /= 0) WRITE (6, '("Error: invalid Namelist format")')

   WRITE(6,*) 'params', fL,fdx,fdepth,fgrav,fcor0,ftau0,fdt,ftime_total
   
   !! Calculate time steps necessary
   nsteps       = ftime_total / fdt

   WRITE(6,*) 'nsteps: ', nsteps

   !! Build Mesh and allocate arrays        
   nx   = NINT( fL / fdx ) 
   
   nel  = nx**2

   nx2  = nx+1

   nkn  = (nx2)**2 

   WRITE(6,*) 'nel,nkn', nel,nkn 

   ALLOCATE( nfe (nel,4) )      !: 4 nodes for each cell
   ALLOCATE( neae(nel,4) )


!   ----------------------------
!
!
!   ----------------------------
!   .            
!   .
!   .
!   .
!  2nx+3---2nx+4---2nx+5--...3nx+3        
!   |        |       |         |  
!   |  nx+1  |  nx+2 |         |
!   |        |       |         |  
!   nx+2---nx+3----nx+4--....2nx+2       
!   |        |       |         |  
!   |   1    |   2   |    nx   |  
!   |        |       |         |  
!   1--------2-------3---.....nx+1

   ALLOCATE ( xk(nkn), yk(nkn) )
   ALLOCATE ( xe(nel), ye(nel) )

   !: node coordinates
   DO jk=1,nkn
      xk(jk)    = fdx * MOD (jk-1,nx2)
      yk(jk)    = fdx * ((jk-1) / nx2)
   END DO

   !: build pointer to cell nodes (ordered anti-clockwise )
   !:         x--------x      
   !:         |        |
   !:         | neae(3)|     
   !:         |        |     
   !: x-------4--------3-------x               
   !: |       |        |       |     
   !: |neae(4)|   je   |neae(2)|     
   !: |       |        |       |     
   !: x-------1--------2-------x
   !:         |        |                  
   !:         | neae(1)|     
   !:         |        |     
   !:         x--------x            

   DO je=1,nel
      irow              = (je-1) / nx + 1
      jcol              = MOD(je-1,nx) + 1
      nfe(je,1)         = (irow-1)*nx2 + jcol
      nfe(je,2)         = (irow-1)*nx2 + jcol + 1
      nfe(je,4)         = nfe(je,1) + nx2
      nfe(je,3)         = nfe(je,4) + 1
      neae(je,1)        = irow - nx
      IF ( irow .EQ. 1 ) neae(je,1)     = je 
      neae(je,2)        = je + 1 
      IF ( jcol .EQ. nx) neae(je,2)     = je 
      neae(je,3)        = je + nx
      IF ( irow .EQ. nx) neae(je,3)     = je 
      neae(je,4)        = je - 1 
      IF ( jcol .EQ. 1 ) neae(je,4)     = je 
   END DO

   write(6,*) 'nfe'
   do je=1,nel
      write(6,*) je, nfe(je,:)
   end do
   write(6,*) 'neae'
   do je=1,nel
      write(6,*) je, neae(je,:)
   end do

   !: calculate centroids coordinates   
   DO je=1,nel
      xe(je) = 0.25D0 * SUM( xk(nfe(je,:)) )
      ye(je) = 0.25D0 * SUM( yk(nfe(je,:)) )
   END DO  

 

   !! Deallocate arrays

   DEALLOCATE ( nfe )
   DEALLOCATE ( xe, ye )
   DEALLOCATE ( xk, yk )

 

   STOP

END PROGRAM swfvm
