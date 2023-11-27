module precisione
	implicit none
	integer, parameter :: rk = selected_real_kind(15)
end module


module all_item_lin
	use precisione
	implicit none
	
	contains
	
	subroutine all_parameters (data_1, data_2, error_2, s00, s11, s10, s01, s20, D)
		real (kind = rk), intent (in), dimension(:) :: data_1, data_2, error_2
		real (kind = rk), intent (out) :: s00, s11, s10, s01, s20, D
		
		integer :: i
		
		s00 = 0
		s11 = 0
		s10 = 0
		s01 = 0
		s20 = 0
		
		do i = 1, size(data_1)
			s00 = s00 + 1 / error_2(i)**2
			s10 = s10 + data_1(i) / error_2(i)**2
			s01 = s01 + data_2(i) / error_2(i)**2
			s20 = s20 + data_1(i)**2 / error_2(i)**2
			s11 = s11 + (data_1(i) * data_2(i)) / error_2(i)**2
		end do
		
		D = s00 * s20 - s10 ** 2
	end subroutine
		
	subroutine index_estimated (s00, s11, s10, s01, s20, D, m, q, sig2_m, sig2_q, cov)
		real (kind = rk), intent (in) :: s00, s11, s10, s01, s20, D
		real (kind = rk), intent (out) :: m, q, sig2_m, sig2_q, cov
		
		m = (s00 * s11 - s10 * s01) / D
		q = (s20 * s01 - s10 * s11) / D
		sig2_m = sqrt(s00 / D)
		sig2_q = sqrt(s20 / D)
		cov = - s10 / D
	end subroutine
	end module
	
program lineare
use precisione
use all_item_lin
implicit none

real (kind = rk), dimension(:), allocatable :: x, y, err_y, yt ,dy
real (kind = rk) :: ms00, ms11, ms10, ms01, ms20, mD, mm, mq, msig2_m, msig2_q, mcov
integer :: i, j, num_dati
character (len = 1) :: uncertainty
real, parameter :: R=0.178_rk 		


write (unit = *, fmt = "(a)", advance = "no") "Quanti dati devi inserire? "
read*, num_dati

allocate (x(num_dati), y(num_dati), err_y(num_dati), yt(num_dati), dy(num_dati))

open (unit = 10, file = 'dat.txt', form = 'formatted', status = 'old', action = 'read')

do i = 1, size(x)																					
	read (unit = 10, fmt = *) x(i), y(i), err_y(i)
	print*, x(i),y(i),err_y(i)
	print*, '---------------------'
	
end do

call all_parameters(x, y, err_y, ms00, ms11, ms10, ms01, ms20, mD)
call index_estimated (ms00, ms11, ms10, ms01, ms20, mD, mm, mq, msig2_m, msig2_q, mcov)

do i = 1, size(x)
yt(i)= mm*x(i) + mq
dy(i)=(y(i) - yt(i))
	write(unit = 2, fmt = *) x(i), y(i), err_y(i)
	write(unit = 3, fmt = *) y(i), yt(i), dy(i), err_y(i)
end do
!.............................................................................
open (unit = 9, file = 'gnuplot.txt', status = 'replace', action = 'write')
					write(9,*) "set xrange [0:2] "
					write(9,*) "set yrange [0:10]"					 
					write(9,*) "set style fill solid 0.5 "
					write(9,*) "set palette rgb 7,5,15"
					write(9,*) "set linetype 1 lc rgb 'black'"
					write(9,*) "set xtics out nomirror"
					write(9,*) "set ytics out nomirror"
					write(9,*) "set xtics font ',15'"
					write(9,*) "set ytics font ',15'"
					write(9,*) "set xlabel offset 18,-1"
					write(9,*) "set ylabel offset 0,5.8"
					write(9,*) "set xlabel font ',15'"
					write(9,*) "set ylabel font ',15'"
					write(9,*) "set term png"
					write(9,*) "set key"
					!per avere una scala logaritmica togliere il commento successivo
					!write(loadFileUnit,fmt=*) "set logscale y"
					! Per personalizzazione scala logaritmica in y togliere il commento alla riga successiva
					!write(loadFileUnit,fmt=*) "set format y '%2.0t{/Symbol \327}10^{%L}'"
					write(9,*) "set output 'grafico.png"	
					write(9,*) "q=", mq
					write(9,*) "m=", mm
					write(9,*) "plot 'dat.txt' u 1:2 w p title 'dati','' u 1:2:3 w error title 'incertezze', m*x+q t 'funzione di fit'"
					write(9,*) "unset term"
close(9)
!............................................B................................
!	open (unit = 8, file = 'gnuplotB.txt', status = 'replace', action = 'write')
!                  write(8,*) "set xrange [0:2] "
!					write(8,*) "set yrange [0:10]"
!					write(8,*) "set style fill solid 0.5 "
!					write(8,*) "set palette rgb 7,5,15"
!					write(8,*) "set linetype 1 lc rgb 'black'"
!					write(8,*) "set xtics out nomirror"
!					write(8,*) "set ytics out nomirror"
!					write(8,*) "set xtics font ',15'"
!					write(8,*) "set ytics font ',15'"
!					write(8,*) "set xlabel offset 18,-1"
!					write(8,*) "set ylabel offset 0,5.8"
!					write(8,*) "set xlabel font ',15'"
!					write(8,*) "set ylabel font ',15'"
!					write(8,*) "set term png"
!					write(8,*) "set key"
!					!per avere una scala logaritmica togliere il commento successivo
!					!write(loadFileUnit,fmt=*) "set logscale y"
!					! Per personalizzazione scala logaritmica in y togliere il commento alla riga successiva
!					!write(loadFileUnit,fmt=*) "set format y '%2.0t{/Symbol \327}10^{%L}'"
!					write(8,*) "set output 'graficoB.png"	
					
!					write(8,*) "m=", mm
	!				write(8,*) "plot 'dat.txt' u 1:2 w p title 'dati','' u 1:2:3 w error title 'incertezze', m*x t 'funzione di fit'"
	!				write(8,*) "unset term"
!close(8)
print*, 
print*, mm, mq, msig2_m, msig2_q, mcov
end program
	