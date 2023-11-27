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
		
	subroutine index_estimated (s00, s11, s10, s01, s20, D, m, q, sig2_m, sig2_q)
		real (kind = rk), intent (in) :: s00, s11, s10, s01, s20, D
		real (kind = rk), intent (out) :: m, q, sig2_m, sig2_q
		
		m = (s00 * s11 - s10 * s01) / D
		q = (s20 * s01 - s10 * s11) / D
		sig2_m = sqrt(s00 / D)
		sig2_q = sqrt(s20 / D)
		
	end subroutine
	end module
	
program lineare
use precisione
use all_item_lin
implicit none

real (kind = rk), dimension(:), allocatable :: x, y, err_y,err_x,err_y1
real (kind = rk) :: ms00, ms11, ms10, ms01, ms20, mD, mm, mq, msig2_m, &
msig2_q, mcov,b00,b11,b10,b01,b20,m1,q1,sig_m1,sig_q1,D
integer :: i, j, num_dati
character (len = 1) :: uncertainty

write (unit = *, fmt = "(a)", advance = "no") "Quanti dati devi inserire? "
read*, num_dati

allocate (x(num_dati), y(num_dati), err_y(num_dati),err_y1(num_dati),err_x(num_dati))

open (unit = 10, file = 'dat.txt', form = 'formatted', status = 'old', action = 'read')

do i = 1, size(x)																					
	read (unit = 10, fmt = *) x(i), y(i), err_y(i)
end do

call all_parameters(x, y, err_y, ms00, ms11, ms10, ms01, ms20, mD)
call index_estimated (ms00, ms11, ms10, ms01, ms20, mD, mm, mq, msig2_m, msig2_q)


err_x= 0.2*x 									!20% di incertezza su x
 err_y1 = sqrt(mm**2*err_x**2)
 err_y = sqrt(err_y1**2+err_y**2)
 
 b00 = sum(1/err_y**2)
 b10 = sum(x/err_y**2)
 b01 = sum(y/err_y**2)  ! S calcolate insieme all'errore di x
 b11 = sum(x*y/err_y**2)
 b20 = sum(x**2/err_y**2)
 D = b00*b20 - b10**2
 
 m1 = (b00*b11 - b10*b01)/D
 q1 = (b01*b20 - b11*b10)/D
 sig_m1 = sqrt(b00/D)
 sig_q1 = sqrt(b20/D)  !incertezze delle nuove stime
 
do i = 1, size(x)
	write(unit = 2, fmt = *) x(i), y(i), err_y(i),err_x(i)
end do


print*,

open (unit = 8, file = 'gnuplotC.txt', status = 'replace', action = 'write')
					write(8,*) "set xrange[0:2]"
					write(8,*) "set yrange[0:10]"
					write(8,*) "set style fill solid 0.5 "
					write(8,*) "set palette rgb 7,5,15"
					write(8,*) "set linetype 1 lc rgb 'black'"
					write(8,*) "set xtics out nomirror"
					write(8,*) "set ytics out nomirror"
					write(8,*) "set xtics font ',15'"
					write(8,*) "set ytics font ',15'"
					write(8,*) "set xlabel offset 18,-1"
					write(8,*) "set ylabel offset 0,5.8"
					write(8,*) "set xlabel font ',15'"
					write(8,*) "set ylabel font ',15'"
					write(8,*) "set term png"
					write(8,*) "set key"
					write(8,*) "set output 'graficoC.png"
					write(8,*) "m=", m1
					write(8,*) "q=", q1
					write(8,*) "plot 'fort.2' u 1:2 w p title 'dati','' u 1:2:4 w xerrorbars title 'incertezze',&
					'' u 1:2:3 w yerrorbars, m*x+q t 'funzione di fit'"
					write(8,*) "unset term"
					
close(8)

print*, m1,q1,sig_m1,sig_q1
end program
	