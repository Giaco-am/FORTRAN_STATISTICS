	
	
	module HistoRoutines
		implicit none
		
		private
		public Histo
		public FillHisto
		public ConstructHisto

		public DestructHisto
		public PrintHisto
		public PrintHisto_1
		public DrawHisto
		public DrawHisto_1
		
		type Histo
			integer                             :: nBin
			real                                :: xmin, xmax
			integer                             :: underflow, overflow
			integer, dimension(:), allocatable  :: counts
			real, dimension(:), allocatable     :: BinCenters
			real, dimension(:), allocatable     :: errors
		
			contains
		
			procedure           :: FillHisto
			procedure           :: ConstructHisto
			procedure           :: DestructHisto
			procedure           :: PrintHisto		
		end type
		
		contains

		
		subroutine ConstructHisto(thisHisto,nBin,xmin,xmax)
			implicit none
			class(Histo), intent(inout) :: thisHisto
			integer, intent(in)         :: nBin
			real, intent(in)            :: xmin, xmax
			real                        :: dx, xx
			integer                     :: iNumb
			
				thisHisto%nBin = nBin
			
				allocate(thisHisto%counts(nBin))
				allocate(thisHisto%BinCenters(nBin))
				allocate(thisHisto%errors(nBin))
			
				thisHisto%counts = 0
				thisHisto%xmin = xmin
				thisHisto%xmax = xmax
			
				dx = ( xmax - xmin ) / nBin
				do iNumb = 1, nBin
					xx = xmin + (iNumb - 0.5) * dx
					thisHisto%BinCenters(iNumb) = xx
				enddo
		
		end subroutine ConstructHisto
		

				
		subroutine fillHisto(thisHisto,xx)	                                
			implicit none
			
			class(Histo), intent(inout) :: thisHisto
			real, intent(in) 	    :: xx
			integer                 :: iNumb, iBin
			real                    :: dx
	  		

			real :: xmin, xmax
			integer :: nBin
			real :: underflow, overflow
			
			xmin = thisHisto%xmin
			xmax = thisHisto%xmax
			nBin = thisHisto%nBin
			
				dx = ( xmax - xmin ) / nBin
				iBin = int( ( xx - xmin ) / dx ) + 1
				
				if( iBin > 0 .and. iBin < Nbin + 1 ) then
					thisHisto%counts(iBin) = thisHisto%counts(iBin) + 1         
				elseif( iBin < 1 ) then
					underflow = underflow + 1
				elseif( iBin > nBin ) then
					overflow = overflow + 1
				endif
				
				thisHisto%underflow=underflow
				thisHisto%overflow=overflow
				
		
		end subroutine fillHisto

		
	
		
		
		subroutine printHisto(thisHisto,fileName,fileUnit)
		implicit none
			class(Histo), intent(inout) :: thisHisto
			integer, intent(in)                     :: fileUnit
			character(len = 128), intent(in)        :: fileName
			integer                                 :: iNumb, ios
			character(len = 128)                    :: fileNameTemp

			fileNameTemp=trim(fileName)//".dat"

			open(unit=fileUnit,file=fileNameTemp,status='replace',iostat=ios)
				if (ios/=0) then
					print*,"Failed to open ",filename,"."
				end if

				do iNumb = 1, thisHisto%nBin
					write( unit = fileUnit , fmt = * )  thisHisto%BinCenters(iNumb),thisHisto%counts(iNumb)
					enddo

			close(fileUnit)
    
	    end subroutine printHisto
	    
	    		
		subroutine printHisto_1(thisHisto,fileName,fileUnit)
		implicit none
			class(Histo), intent(inout) :: thisHisto
			integer, intent(in)                     :: fileUnit
			character(len = 128), intent(in)        :: fileName
			integer                                 :: iNumb, ios
			character(len = 128)                    :: fileNameTemp

			fileNameTemp=trim(fileName)//".dat"

			open(unit=fileUnit,file=fileNameTemp,status='replace',iostat=ios)
				if (ios/=0) then
					print*,"Failed to open ",filename,"."
				end if

				do iNumb = 1, thisHisto%nBin
					write( unit = fileUnit , fmt = * )  thisHisto%BinCenters(iNumb), thisHisto%counts(iNumb),&
						thisHisto%errors(iNumb)
				enddo

			close(fileUnit)
    
	    end subroutine printHisto_1

		
		
		
		subroutine drawHisto(fileHisto, loadFile, loadFileUnit, xlabel, xmin, & 
							 xmax , ylabel, histoTitle)
			implicit none
			character(len=128), intent(in)  :: fileHisto
			character(len=128), intent(in) :: loadFile
			character(len=128), intent(in)  :: xlabel
			character(len=128), intent(in)  :: ylabel
			character(len=128), intent(in)  :: histoTitle
			real, intent(in)        :: xmin, xmax
			integer, intent(in)     :: loadFileUnit
			integer                 :: ios
			character(len = 128)    :: fileHistoTemp, loadFileTemp, printFile
			character(len= 128) ::GnuplotLoadFile
			
			fileHistoTemp   = trim(fileHisto)//".dat"
			loadFileTemp    = trim(loadFile)//".txt"
			printFile       = trim(loadFile)//".png"
			
			
		   open(unit=loadFileUnit,file=loadFileTemp,status='replace',iostat=ios)
				if (ios/=0) then
					print*,"Failed to open ",loadFile,"."
				end if
				
				    write(loadFileUnit,fmt=*) "set xlabel '",trim(xlabel),"'"
					write(loadFileUnit,fmt=*) "set ylabel '",trim(ylabel),"'"
					write(loadFileUnit,fmt=*) "set xrange [",xmin,":",xmax,"]"
                   ! Per selezionare ymax togliere il commento alla riga successiva	
					!write(loadFileUnit,fmt=*) "set yrange [",ymin,":",ymax,"]"
					write(loadFileUnit,fmt=*) "set style fill solid 0.5 "
					write(loadFileUnit,fmt=*) "set palette rgb 7,5,15"
					write(loadFileUnit,fmt=*) "set linetype 1 lc rgb 'black'"
					write(loadFileUnit,fmt=*) "set xtics out nomirror"
					write(loadFileUnit,fmt=*) "set ytics out nomirror"
					write(loadFileUnit,fmt=*) "set xtics font ',15'"
					write(loadFileUnit,fmt=*) "set ytics font ',15'"
					write(loadFileUnit,fmt=*) "set xlabel offset 18,-1"
					write(loadFileUnit,fmt=*) "set ylabel offset 0,5.8"
					write(loadFileUnit,fmt=*) "set xlabel font ',15'"
					write(loadFileUnit,fmt=*) "set ylabel font ',15'"
					write(loadFileUnit,fmt=*) "set term png"
					write(loadFileUnit,fmt=*) "set key"
					write(loadFileUnit,fmt=*) "set output '",trim(printFile)
					write(loadFileUnit,fmt=*) &
						"plot '",trim(fileHistoTemp),"' u 1:2 w boxes fs solid 0.5 fillcolor 'orange' title '",&
							trim(histoTitle),"'"
					write(loadFileUnit,fmt=*) "unset term"
					write(loadFileUnit,fmt=*) "q"
					
			close(loadFileUnit)
			
		end subroutine drawHisto
		
		subroutine drawHisto_1(fileHisto, fileHisto1, loadFile, loadFileUnit, xlabel, xmin, & 
							 xmax , ylabel, histoTitle)
			implicit none
			character(len=128), intent(in)  :: fileHisto, fileHisto1
			character(len=128), intent(in) :: loadFile
			character(len=128), intent(in)  :: xlabel
			character(len=128), intent(in)  :: ylabel
			character(len=128), intent(in)  :: histoTitle
			real, intent(in)        :: xmin, xmax
			integer, intent(in)     :: loadFileUnit
			integer                 :: ios
			character(len = 128)    :: fileHistoTemp, loadFileTemp, printFile, fileHistoTemp1
			character(len= 128) ::GnuplotLoadFile
			
			fileHistoTemp   = trim(fileHisto)//".dat"
			fileHistoTemp1   = trim(fileHisto1)//".dat"
			loadFileTemp    = trim(loadFile)//".txt"
			printFile       = trim(loadFile)//".png"
			
			
		   open(unit=loadFileUnit,file=loadFileTemp,status='replace',iostat=ios)
				if (ios/=0) then
					print*,"Failed to open ",loadFile,"."
				end if
				
				    write(loadFileUnit,fmt=*) "set xlabel '",trim(xlabel),"'"
					write(loadFileUnit,fmt=*) "set ylabel '",trim(ylabel),"'"
					write(loadFileUnit,fmt=*) "set xrange [",xmin,":",xmax,"]"
                   ! Per selezionare ymax togliere il commento alla riga successiva	
					!write(loadFileUnit,fmt=*) "set yrange [",ymin,":",ymax,"]"
					write(loadFileUnit,fmt=*) "set style fill solid 0.5 "
					write(loadFileUnit,fmt=*) "set palette rgb 7,5,15"
					write(loadFileUnit,fmt=*) "set linetype 1 lc rgb 'black'"
					write(loadFileUnit,fmt=*) "set xtics out nomirror"
					write(loadFileUnit,fmt=*) "set ytics out nomirror"
					write(loadFileUnit,fmt=*) "set xtics font ',15'"
					write(loadFileUnit,fmt=*) "set ytics font ',15'"
					write(loadFileUnit,fmt=*) "set xlabel offset 18,-1"
					write(loadFileUnit,fmt=*) "set ylabel offset 0,5.8"
					write(loadFileUnit,fmt=*) "set xlabel font ',15'"
					write(loadFileUnit,fmt=*) "set ylabel font ',15'"
					write(loadFileUnit,fmt=*) &
						"plot '",trim(fileHistoTemp),"' u 1:2 w boxes fs solid 0.5 fillcolor 'orange' title '",&
						trim(histoTitle),"'"
					write(loadFileUnit,fmt=*) "replot '",trim(fileHistoTemp1),"' u 1:2 w p, '' u 1:2:3 w errors"
					write(loadFileUnit,fmt=*) "set term png"
					write(loadFileUnit,fmt=*) "set key"
					write(loadFileUnit,fmt=*) "set output '",trim(printFile)		
					write(loadFileUnit,fmt=*) "unset term"
					write(loadFileUnit,fmt=*) "q"
					
			close(loadFileUnit)
			
		end subroutine drawHisto_1
		
		subroutine DestructHisto(thisHisto)
			class(Histo), intent(inout) :: thisHisto
			
				if( allocated(thisHisto%counts) ) then
					deallocate(thisHisto%counts)
				endif
				if( allocated(thisHisto%BinCenters) ) then
					deallocate(thisHisto%BinCenters)
				endif
				if( allocated(thisHisto%errors) ) then
					deallocate(thisHisto%errors)
				endif
			
			
		end subroutine
		
		
	end module HistoRoutines
	
	
	
	
	
	
	
	
	program es5_2                                                         !PROGRAMMA
		
		use HistoRoutines
		implicit none
	
		integer             :: i,j,k, nBin, nDati, err2, err3, counter
		real                :: xmin, xmax, rnd, rnd1, s_x, s_y, s_xx, s_xy, s, probt
		real                :: errM, errQ, M, Q, t, aa, dx
		real, dimension(6)  :: X,Y, y_k, O, U, H
		type(Histo)         :: H_t, H_tasp
		character(len=128)  :: d_t, d_tasp, counts
		
		nDati=10000
		xmin=0 
		xmax=15
!		print*, "nDati?"
!		read*, nDati
!		print*, "xmin?"
!		read*, xmin
!		print*,"xmax"
!		read*, xmax
		print*, "nBin?"
		read*, nBin
		
		X=[0.50, 0.70, 0.80, 1.00, 1.20, 1.60]									!VALORI VERI
		O=[0.05, 0.05, 0.05, 0.10, 0.10, 0.10]
		
		do i=1,6
			Y(i)=X(i)*5.0
		enddo
				
		
		call ConstructHisto(H_t,nBin,xmin,xmax)
		call ConstructHisto(H_tasp,nBin,xmin,xmax)
		

		do k=1, nDati		                                                    !COSTRUTTORE DATI
		
			do i=1,6                                                            	
				rnd1=0
				do j=1,12
					call random_seed()
					call random_number(rnd)
					rnd = rnd-0.5
					rnd1 = rnd1 + rnd				
				enddo				
				U(i)=O(i)*rnd1
				Y_k(i)=Y(i)+U(i)				
			enddo			
			
			s_x=0
			s_y=0
			s_xx=0
			s_xy=0
			s=0			
			do i=1,6                                                            
				s_x=s_x+(X(i)/O(i)**2)
				s_y=s_y+(Y_k(i)/O(i)**2)
				s_xx=s_xx+(X(i)**2/O(i)**2)
				s_xy=s_xy+((X(i)*Y_k(i))/O(i)**2)
				s=s+(1/O(i)**2)
			enddo	
		
			M=((s*s_xy-s_x*s_y)/(s*s_xx-(s_x**2)))
			Q=((s_xx*s_y-s_x*s_xy)/(s*s_xx-(s_x**2)))
			errQ=sqrt(s_xx/(s_xx*s-s_x**2))
			errM=sqrt(s/(s_xx*s-s_x**2))

				do i=1,6                                                          !TEST D'IPOTESI
					H(i)=M*X(i)+Q
				enddo
	
				t=0
				do i=1,6
					t=t+((y_k(i)-H(i))**2)/(O(i)**2)
				enddo
	
			call FillHisto(H_t, t)		
		enddo
		

		dx=(xmax-xmin)/nBin
		do i=1,H_tasp%nBin                            
			aa=(1./4.)*exp(-(H_tasp%BinCenters(i)/2))*H_tasp%BinCenters(i)
			probt=aa*((xmax-xmin)/H_tasp%nBin)*nDati
			print*, aa
			H_tasp%counts(i)=nint(probt)
			H_tasp%errors(i)=sqrt(aa*dx*nDati*(1-aa*dx))
		enddo

	
		
		d_t= "datit"
		d_tasp="dati_tasp"
		call printHisto(H_t, d_t, 1)
		call printHisto_1(H_tasp, d_tasp, 2)
		call drawHisto_1( d_t, d_tasp, d_t, 1, d_t, xmin, xmax, counts, d_t )
		call DestructHisto(H_t)

	end program es5_2