program es5_1
	implicit none
	
	real,dimension(6) :: X,Y,Z,o_y,U
	integer :: i
	real:: x_2, m, q
	
	m=5.86
	q=-0.20
	
	X=(/0.5,0.7,0.8,1.00,1.20,1.50/)
	Y=(/2.80,3.90,4.45,5.40,6.65,8.90/)
	o_y=(/0.05,0.05,0.05,0.1,0.1,0.1/)
	
	do i=1,6
		U(i)=m*X(i)+q
	enddo
	
	x_2=0
	do i=1,6
		x_2=x_2+((Y(i)-U(i))**2)/(o_y(i)**2)
		print*, i,Y(i),U(i),o_y(i)
		
	enddo
	
	print*, "t*=", x_2
	print*, "t_0.05=12.6"
	
end program es5_1