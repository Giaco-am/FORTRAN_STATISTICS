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
					xx = xmin + (iNumb) * dx
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
				iBin = int( ( xx - xmin ) / dx )+1
				
				if( iBin > 0 .and. iBin < Nbin+1) then
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
					write(loadFileUnit,fmt=*) "replot '",trim(fileHistoTemp1),"' u 1:2 w p, '' u 1:2:3 with yerror"
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
	
	
	
	module factorial
		implicit none
		
		contains
		
			function fact(n) result(m)
				integer, intent(in):: n
				integer, parameter :: ik=selected_int_kind(15)
				integer (kind=ik)  :: m
				integer            :: i
				m=1
				do i=1,n
					m=m*i
				enddo
			end function
	
	end module factorial
	
!-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	
	
	program es4_6
		
		use HistoRoutines
		use factorial
		implicit none
		
		type(Histo)         :: H_b, H_basp
		integer             :: i, j, nBin, nk, k, n, nDati
		real                :: rnd, xmin, xmax, aa, p_asp, x, med, c, dx
		character(len=128)  :: d_b, d_basp, counts

		print*, "nDati?"
		read*, nDati
		print*, "Numero di bin?"
		read*, nBin
		print*, "xmin e xmax"
		read*, xmin, xmax
		print*, "c?"
		read*, c

		
		call constructHisto (H_b,nBin,xmin,xmax)
		call constructHisto (H_basp,nBin,xmin,xmax)
		
		
		do i=1,nDati
			med=0
			call random_seed()
			do j=1,50			
				call random_number(rnd)
				med=med - c*log(1-rnd)
			enddo
			med=med/50
		call fillHisto(H_b,med)
		enddo
	
	
		do i=1,H_basp%nBin                                                         
			aa=(1/sqrt(2*(4*atan(1.0))*2))*exp(-((H_basp%BinCenters(i)-c)**2)/(2*2))
			dx=(xmax-xmin)/H_basp%nBin
			p_asp=aa*dx*nDati
			H_basp%counts(i)=nint(p_asp)
			H_basp%errors(i)=sqrt(aa*dx*nDati*(1-aa*dx))
		enddo

	
		d_b="dati6"
		d_basp="dati6asp"
		
		call printHisto(H_b, d_b, 1)
		call printHisto_1(H_basp, d_basp, 2)
		call drawHisto_1( d_b, d_basp, d_b, 1, d_b, xmin, xmax, counts, d_b )
		call destructHisto(H_b)
	
end program es4_6
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	