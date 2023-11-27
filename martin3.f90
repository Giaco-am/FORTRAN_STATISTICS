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
	
	
	
	
	
	
	
	
	program esercizio3                                                          !PROGRAMMA
		
		use HistoRoutines
		implicit none
	
		integer             :: i,j,k, nBin, nDati, err2, err3, counter
		real                :: xmin, xmax, rnd, rnd1, s_x, s_y, s_xx, s_xy, s, rho1, cov1, varM, varQ, probM, probQ
		real                :: rho, errM, errQ, M, Q, cov, media_M, media_Q, sumM, sumQ, sum1M, sum1Q, sumC, aa, bb
		real, dimension(6)  :: X,Y, y_k, O, U
		type(Histo)         :: H_m, H_q, H_om, H_oq, H_rho, H_inv, H_invq, H_masp, H_qasp
		character(len=128)  :: d_m, d_q, d_om, d_oq, d_rho, d_inv, d_invq, counts, d_masp, d_qasp
		real, dimension(:), allocatable  :: arrM, arrQ
		

		print*, "nDati?"
		read*, nDati     !1000
		print*, "xmin?"
		read*, xmin
		print*,"xmax"
		read*, xmax
		print*, "nBin?"
		read*, nBin
		
		allocate(arrM(nDati),arrQ(nDati))
				
		X=[0.50, 0.70, 0.80, 1.00, 1.20, 1.60]									!VALORI VERI
		do i=1,6
			Y(i)=X(i)*5.0       !m_0= 5.0
		enddo
		O=[0.05, 0.05, 0.05, 0.10, 0.10, 0.10]   ! su x
				
		
		call ConstructHisto(H_m,nBin,xmin,xmax)
		call ConstructHisto(H_q,nBin,xmin,xmax)
		call ConstructHisto(H_om,nBin,xmin,xmax)
		call ConstructHisto(H_oq,nBin,xmin,xmax)
		call ConstructHisto(H_rho,nBin,xmin,xmax)
		call ConstructHisto(H_inv,nBin,xmin,xmax)
		call ConstructHisto(H_invq,nBin,xmin,xmax)
		call ConstructHisto(H_masp,nBin,xmin,xmax)
		call ConstructHisto(H_qasp,nBin,xmin,xmax)
		
		
		do k=1, nDati		                                                    !COSTRUTTORE DATI
			do i=1,6                                                            	
				rnd1=0
				do j=1,12
					call random_seed()
					call random_number(rnd)
					rnd = rnd-0.5         !rnd possono essere negativi, così centro la distribuzione sugli assi
					rnd1 = rnd1 + rnd				
				enddo				
				U(i)=O(i)*rnd1           !errori casuali che definisco con i rnd
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
			
			cov=-(s_x/(s*s_xx-(s_x**2)))			
			M=((s*s_xy-s_x*s_y)/(s*s_xx-(s_x**2)))
			Q=((s_xx*s_y-s_x*s_xy)/(s*s_xx-(s_x**2)))
			errQ=sqrt(s_xx/(s_xx*s-s_x**2))
			errM=sqrt(s/(s_xx*s-s_x**2))
			Rho=cov/(errM*errQ)			
			arrM(k)=M
			arrQ(k)=Q

			call FillHisto(H_m, M)		
			call FillHisto(H_q, Q)	
			call FillHisto(H_om, errM)	
			call FillHisto(H_oq, errQ)	
			call FillHisto(H_rho, rho)	
			call FillHisto(H_inv, 1/M)
			call FillHisto(H_invq, 1/Q)
		enddo
		

		sumM=0                                                                  !MEDIA
		sumQ=0
		sum1M=0
		sum1Q=0
		sumC=0
		do i=1,nDati                                                            
			sumM=sumM+arrM(i)
			sumQ=sumQ+arrQ(i)
		enddo		
		media_M=sumM/nDati
		media_Q=sumQ/nDati
		
		do i=1,nDati
			sum1M=sum1M+(arrM(i)-media_M)**2        ! sommatoria dei (x-mu)^2   
			sum1Q=sum1Q+(arrQ(i)-media_Q)**2
			sumC=sumC+((arrM(i)-media_M)*(arrQ(i)-media_Q))
		enddo
	
		varM=sum1M/(nDati-1)       !  per ottenere una stima centrata per n qualsiasi devo moltiplicare per 1/n-1
		varQ=sum1Q/(nDati-1)
		sumC=sumC/(nDati-1)
		Cov1=sumC
		Rho1=sumC/(sqrt(varM)*sqrt(varQ))
		

		
		do i=1,H_masp%nBin                                                       ! sono le x
			aa=(1/sqrt(2*(4*atan(1.0))*varM))*&              						 !! aa è Gauss
					(exp(-((H_masp%BinCenters(i)-media_M)**2)/(2*varM)))
			print*, H_masp%BinCenters(i)
			probM=aa*((xmax-xmin)/H_masp%nBin)*nDati			!configura la gaussiana
			H_masp%counts(i)=nint(probM) 					 ! trasformo un reale nell' intero più vicino
			H_masp%errors(i)=sqrt(varM)
		enddo

		
		do i=1,H_qasp%nBin
			bb=(1/sqrt(2*(4*atan(1.0))*varQ))*&
					(exp(-((H_qasp%BinCenters(i)-media_Q)**2)/(2*varQ)))
			probQ=bb*((xmax-xmin)/H_qasp%nBin)*nDati
			H_qasp%counts(i)=nint(probQ)
			H_qasp%errors(i)=sqrt(varQ)
		enddo
		
		
		print*, "Media di M =", media_M, "+-", errM                             !VERIFICA DATI
		print*, "Media di Q =", media_q, "+-", errQ
		print*, "Coefficiente di correlazione =", rho
		print*, "Rho1=", rho1
		print*, "Covarianza =", cov
		print*, "Covarianza1 =", Cov1
		print*, "dev_std M", sqrt(varM)
		print*, "dev_std Q", sqrt(varQ)
			
		
		
		
!		print*,"underflow_M: ", H_m%underflow                                   !COSTRUZIONE ISTOGRAMMI
!		print*,"overflow_M: ", H_m%overflow
!		print*,"underflow_Q: ", H_q%underflow
!		print*,"overflow_Q: ", H_q%overflow
!		print*,"underflow_Om: ", H_om%underflow
!		print*,"overflow_Om: ", H_om%overflow
!		print*,"underflow_Oq: ", H_oq%underflow
!		print*,"overflow_Oq: ", H_oq%overflow
!		print*,"underflow_RHO: ", H_rho%underflow
!		print*,"overflow_RHO: ", H_rho%overflow
!		print*,"underflow_INV: ", H_inv%underflow
!		print*,"overflow_INV: ", H_inv%overflow					
		
		d_m= "datim"
		d_q="datiq"
		d_om="datiom"
		d_oq="datioq"
		d_rho="datirho"
		d_inv="datiinv"
		d_invq="datiinvq"
		counts= "counts"
		d_masp= "datimasp"
		d_qasp= "datiqasp"
	
		call printHisto(H_m, d_m, 1)
		call printHisto(H_q, d_q, 2)
		call printHisto(H_om, d_om, 3)
		call printHisto(H_oq, d_oq, 4)
		call printHisto(H_rho, d_rho, 5)
		call printHisto(H_inv, d_inv, 6)
		call printHisto(H_invq, d_invq, 7)
		call printHisto_1(H_masp, d_masp, 8)
		call printHisto_1(H_qasp, d_qasp, 9)
	
		call drawHisto_1( d_m, d_masp, d_m, 1, d_m, xmin, xmax, counts, d_m )
		call drawHisto_1( d_q, d_qasp, d_q, 2, d_q, xmin, xmax, counts,  d_q)
		call drawHisto( d_om, d_om, 3, d_om, xmin, xmax, counts, d_om )
		call drawHisto( d_oq, d_oq, 4, d_oq, xmin, xmax, counts, d_oq )
		call drawHisto( d_rho, d_rho, 5, d_rho, xmin, xmax, counts, d_rho )
		call drawHisto( d_inv, d_inv, 6, d_inv, xmin, xmax, counts, d_inv )
		call drawHisto( d_invq, d_invq, 7, d_invq, xmin, xmax, counts, d_invq )

		call DestructHisto(H_m)
		call DestructHisto(H_q)
		call DestructHisto(H_om)
		call DestructHisto(H_oq)
		call DestructHisto(H_rho)
		call DestructHisto(H_inv)
		call DestructHisto(H_invq)
	
		
		
			open(unit=21,file="dati.dat",iostat=err2)                            ! COPPIE DI M E Q
				if (err2/=0) then
					print*,"Failed to open dati.dat"
				end if	
				
				do i = 1,nDati
					write( unit = 21, fmt = * ) arrM(i), arrQ(i)
				enddo			
			close(21)                                                         		
			
			open(unit=2,file="coppie.txt",iostat=err3)                
				if (err3/=0) then
					print*,"Failed to open comandi.dat"
				end if

					write(2,*) "set xlabel 'M'"
					write(2,*) "set ylabel 'Q'"
					write(2,*) "set xrange[",xmin,":",xmax,"]"
!					write(2,*) "set yrange[",ymin,":",ymax,"]"
					write(2,*) "set palette rgb 7,5,15"
					write(2,*) "set linetype 1 lc rgb 'black'"
					write(2,*) "set xtics out nomirror"
					write(2,*) "set ytics out nomirror"
					write(2,*) "set xtics font ',15'"
					write(2,*) "set ytics font ',15'"
					write(2,*) "set xlabel offset 18,-1"
					write(2,*) "set ylabel offset 0,5.8"
					write(2,*) "set xlabel font ',15'"
					write(2,*) "set ylabel font ',15'"
					write(2,*) "set term png"
					write(2,*) "set key"
					write(2,*) "set output 'coppie.png"
					write(2,*) "plot 'dati.dat' u 1:2 w p title 'dati'"			
					write(2,*) "unset term"
					write(2,*) "q"					                                 
			close(2)
			
	
			
			open(unit=3,file="datiB.dat",iostat=err2)                            ! COPPIE DENTRO L'ELLISSE
					if (err2/=0) then
						print*,"Failed to open datiB.dat"
					end if
				
				counter=0	
				do i = 1,nDati
					if (((((1/(errM**2))*(arrM(i)-media_M)**2)-2*rho*(1/(errM*errQ))*(arrM(i)-media_M)*&
						(arrQ(i)-media_Q)+((1/(errQ**2))*((arrQ(i)-media_q)**2)))*(1/(1-(rho**2))))<1.0) then
					write( unit = 3, fmt = * ) arrM(i), arrQ(i)
					counter=counter+1
					endif
				enddo			
			close(3)                                                         		
			
			open(unit=4,file="coppieB.txt",iostat=err3)                         
				if (err3/=0) then
					print*,"Failed to open comandi.dat"
				end if
				
					write(4,*) "set xlabel 'M'"
					write(4,*) "set ylabel 'Q'"
					write(4,*) "set xrange[",xmin,":",xmax,"]"
!					write(4,*) "set yrange[",ymin,":",ymax,"]"
					write(4,*) "set palette rgb 7,5,15"
					write(4,*) "set linetype 1 lc rgb 'black'"
					write(4,*) "set xtics out nomirror"
					write(4,*) "set ytics out nomirror"
					write(4,*) "set xtics font ',15'"
					write(4,*) "set ytics font ',15'"
					write(4,*) "set xlabel offset 18,-1"
					write(4,*) "set ylabel offset 0,5.8"
					write(4,*) "set xlabel font ',15'"
					write(4,*) "set ylabel font ',15'"
					write(4,*) "set term png"
					write(4,*) "set key"
					write(4,*) "set output 'coppieB.png"
					write(4,*) "plot 'datiB.dat' u 1:2 w p title '",counter,"'"			
					write(4,*) "unset term"
					write(4,*) "q"					                                 
			close(4)
																									!datirho(-1,-0.8) daticov(-0.001,0)
	end program esercizio3       ! i grafici sono da centrare in xmin e xmax specifici per ogni dato: datim(4.5,5.5) datiq(-0.5,0.5) datiinvq(-100,100) datiinv(-0.3,0.3)