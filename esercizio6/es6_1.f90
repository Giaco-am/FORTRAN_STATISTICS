
module precisione
integer, parameter :: rk = selected_real_kind(8)
real(kind = rk), parameter :: pi = acos(-1.0)
end module precisione

module abc
use precisione
contains

 function fun(x,a) result(funct)
  implicit none									
  real :: a, x, funct
 
  funct = (1+a*cos(x))/(2*pi)						!distrib teorica
 
 end function fun
 
 function gfun(x,a,b) result(gfunct)
  implicit none
  real :: a, b, x, gfunct
 
  gfunct = (1+a*cos(x)+b*sin(x))/(2*pi)
 
 end function gfun
 
 function prob(x,dx,a,b) result(pl)  !!pl è la prob che il sing evento cada nell'intervallo
  implicit none
  real,intent(in) :: x,dx,a,b
  real :: pl,int_a,int_b
     
  int_a = sin(x+dx) - sin(x)
  int_b = cos(x) - cos(x+dx)
  pl = (dx + a*int_a + b*int_b)/(2*pi) !! faccio l'integrale senza approssimare
 
  end function prob
 
 function fatt(n) result(fact)
  implicit none
  integer :: i, fact, n
 
  fact = 1.0
 
  do i = 1,n
     fact = fact*i
  end do
 
  end function fatt
 
  function lfat(n) result (lfact)
   implicit none
   integer :: i, lfact
   real :: n
   
   lfact = (((n+0.5)*log(n))-n+(0.5*log(2*pi)))
 
  end function lfat
 
  function S(x,y,z,j,k) result(somma) !serve a trovare S_00, ecc...
   implicit none
   real,dimension(:),intent(in) :: x,y,z
   integer,intent(in) :: j,k
   real :: somma
 
   somma = sum((x**j * y**k) / z)
     
  end function S  
 
end module abc
 
program es6
use abc
implicit none

!variabili per montecarlo

real :: a
real :: x, y, z
real :: xinf, xmin, xmax, ymin, ymax
integer :: i, j, l, n, k, m
integer :: Ndati, conteggi

!variabili per la costruzione dell'istogramma

real :: dx, xl
integer :: ind, fuori, nbin
real,dimension(:),allocatable :: histo, vmedio

!variabili per il metodo del maximum likelihood

integer :: fat_cont, fattoriale
integer :: h, N_file, n_fatt
real :: passo, mu
real :: Li_grafico, Li_massimo
real :: a_grafico, b_grafico
real :: a_stimato_l, b_stimato_l, Li_stimato
real,dimension(32) :: fat_n
real,dimension(:),allocatable :: a_file_l, b_file_l, Li_file

!variabili per il metodo dei minimi quadrati

integer :: r
real :: D, e, asp, diff
real :: X_2_grafico, X_2_minimo,X_2
real :: a_stimato_m, b_stimato_m, X_2_stimato
real :: var_a, var_b, cov, corr
real,dimension(:),allocatable :: a_file_m, b_file_m, X_2_file
real,dimension(:),allocatable :: ci, si

!variabili per il test d'ipotesi

real,parameter :: alfa=0.05, t_alfa=42.557  ! a 29 gradi di liberta
! perchè chi quadro di pearson è nbin - 3
!cioè  nbin -1 - 2 parametri stimati( a e b)



Ndati = 2000

nbin = 32

dx = (2*pi)/nbin

allocate(vmedio(nbin),histo(nbin))

a = 0.25

xinf = 0.0
xmin = pi !! coincide con xsup
xmax = 2*pi

ymin = 0.0
ymax = (1+ abs(a))/2*pi

!print*, "il minimo e il massimo della funzione sono:"
print*, ymin, ymax

fuori = 0.0
histo = 0.0
     
do j = 1,Ndati           	!loop per simulazione Montecarlo

   call random_number(x)
   call random_number(y)
   x = x*2*pi
   y = y*ymax
   
   !costruisco istogramma
   if (y .lt. fun(x,a)) then
       open(1,file='conteggio')
      write(1,*) x, y
       ind = int(x/dx) + 1
       if (ind .gt. nbin .or. ind .lt. 1) then
          fuori = fuori + 1
       else
           histo(ind) = histo(ind) + 1
       end if    
   end if

end do


do l = 1,nbin

   open(10,file='isto_mont')  			!istogramma 1:3 funzione teorica 
   z = l*dx - 0.5*dx  
   write(10,*) z, histo(l), fun(z,a)*dx

end do

close(unit=10)


open(10,file='isto_mont',status='old')
open(11,file='montecarlo')

conteggi = sum(histo)

do l = 1,nbin

   read(10,*) vmedio(l), histo(l)
   
   write(11,*) vmedio(l), histo(l),conteggi*dx*fun(vmedio(l),a)

end do

!print*, "il numero totale di conteggi e'", conteggi


!calcolo del fattoriale

fat_cont = lfat(1.0*conteggi)  !mi fa il fattoriale del numero totale di conteggi

!fat_n = 1

do l = 1,nbin

   fat_n(l) = lfat(histo(l))  !mi fa il fattoriale dei conteggi nel l-esimo bin

end do

fattoriale = fat_cont - sum(fat_n)     !logaritmo del fattoriale del numero di conteggi


!metodo del maximum likelihood e MMQ

open(13,file='mml')
open(14,file='mmq')

a_grafico = 0.0
passo = 0.001

do i = 1,200

   b_grafico = -0.25

   do k = 1,200
   
      Li_grafico = 0.0
      X_2_grafico = 0.0
     
      xl = dx/2
       
      do l = 1,nbin
                 
         mu = prob(xl,dx,a_grafico,b_grafico) !! serve per ML
         asp = conteggi*prob(xl,dx,a_grafico,b_grafico) !! serve per MMQ
     
      Li_grafico = Li_grafico + (histo(l)*log(mu))		!somma dei logaritmi delle probabilità previste per ogni bin dell'istogramma pesati dal numero di conteggi
      													!osservati in quel bin
      
      
      X_2_grafico = X_2_grafico + ((histo(l)-asp)**2/(histo(l))) !! appr asp con histo(l)
     
      xl = xl + dx
                     
      end do
     
      Li_grafico = Li_grafico + fattoriale
     
      write(13,*) Li_grafico, a_grafico, b_grafico
      write(14,*) X_2_grafico, a_grafico, b_grafico
     
      b_grafico = b_grafico + passo
     
  end do
   
  a_grafico = a_grafico + passo
   
end do

close(unit=13)
close(unit=14)

N_file = 40000
allocate(a_file_l(N_file),b_file_l(N_file),Li_file(N_file))
allocate(a_file_m(N_file),b_file_m(N_file),X_2_file(N_file))
open(13,file='mml',status='old')
open(14,file='mmq',status='old')
Li_massimo = -300
X_2_minimo = 200
h = 0
r = 0

do i = 1,N_file

   read(13,*) Li_file(i), a_file_l(i), b_file_l(i)
   
   if(Li_file(i) .gt. Li_massimo) then
     Li_massimo = Li_file(i)
     h = i
   else
   end if
   
   read(14,*) X_2_file(i), a_file_m(i), b_file_m(i)
   

   if(X_2_file(i) .lt. X_2_minimo) then
     X_2_minimo = X_2_file(i)
     r = i
   else
   end if
   
   
   
end do
!print*,X_2_minimo
X_2 = X_2_minimo + 1.0
open(10,file='ellisse')
do i=1,N_file
   if(X_2_file(i) < X_2) then
     write(10,*)a_file_m(i),b_file_m(i),X_2_file(i)
   else
   end if
end do

a_stimato_l = a_file_l(h)
b_stimato_l = b_file_l(h)

a_stimato_m = a_file_m(r)
b_stimato_m = b_file_m(r)


allocate(ci(nbin),si(nbin))

do l = 1,nbin

   ci(l) = sin(vmedio(l)+dx)-sin(vmedio(l))
   si(l) = cos(vmedio(l))-cos(vmedio(l)+dx)
   
   e = (conteggi**2)/(4*pi**2)
   D = e*(S(ci,si,vmedio,2,0)*S(ci,si,vmedio,0,2)-(S(ci,si,vmedio,1,1))**2)
   var_a = S(ci,si,vmedio,0,2)/D
   var_b = S(ci,si,vmedio,2,0)/D
   cov = -S(ci,si,vmedio,1,1)/D

end do

corr = cov/(sqrt(var_a)*sqrt(var_b))

print*, "--con il metodo dei minimi quadrati:"
print*, "-il valore stimato di a e'", a_stimato_m, "con una deviazione standard di", sqrt(var_a)
print*, "-il valore di b stimato e'", b_stimato_m, "con una deviazione standard di", sqrt(var_b)
print*, "-i due parametri hanno una correlazione di", corr
print*, "-il valore di X**2 con a e b stimati e':", X_2_minimo
print*, "-la covarianza: ", cov

!test d'ipotesi

print*, "il t_alfa scelto e':", t_alfa

if(X_2_minimo .lt. t_alfa) then
   print*, "l'ipotesi non viene rigettata con un alfa del 5 per cento"
   else
   print*, "l'ipotesi viene rigettata"
end if

print*, "--con il metodo del maximum likelihood:"
print*, "-il valore stimato di a e':", a_stimato_l,"il valore di b stimato e':", b_stimato_l
print*, "-il valore di likelihood con a e b stimati e':", Li_massimo



close(unit=10)
close(unit=11)
close(unit=13)
close(unit=14)
close(unit=15)

end program es6

