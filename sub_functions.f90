! standard deviation function
function std(x,n)
	implicit none
	real :: std
	integer :: n
	real, dimension(n) :: x
	std = sqrt((sum(x**2)-sum(x)**2/size(x))/(size(x)-1))
end function std

! arithmetic mean function
function avg(x,n)
	implicit none
	real :: avg
	integer :: n
	real, dimension(n) :: x
	avg = sum(x)/(max(1,size(x)))
end function avg

! distance function
function dist(x1,y1,z1,x2,y2,z2)
	implicit none
	real :: dist
	real :: x1,y1,z1,x2,y2,z2
	dist = sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
end function dist

! uniform random generator function
subroutine uniform_gen(x,n,xmin,xmax)
	implicit none
	integer :: n, i
	real :: xmin,xmax
	real, dimension(n) :: x
	call random_number(x)
	do i = 1, n
		x(i) = xmin + x(i)*(xmax-xmin)
	end do
end subroutine uniform_gen

! normal random generator function
subroutine normal_gen(x,n,xmin,xmax)
	implicit none
	integer :: n, i
	real :: xmin,xmax
	real, dimension(n) :: x
	call random_number(x)
	do i = 1, n
		x(i) = xmin + x(i)*(xmax-xmin)
	end do
end subroutine normal_gen

! weibull random generator function
subroutine weibull_gen(x,n,xmin,xmax)
	implicit none
	integer :: n, i
	real :: xmin,xmax
	real, dimension(n) :: x
	call random_number(x)
	do i = 1, n
		x(i) = xmin + x(i)*(xmax-xmin)
	end do
end subroutine weibull_gen
