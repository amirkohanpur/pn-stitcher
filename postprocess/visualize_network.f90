! 
! This Fortran script produces network visualization file
! The output of this code is a TCl format file 'prefix_network.tcl' 
! You need to use VMD (Visual Molecular Dynamics) to open the written TCL file
! Please load the 'state.vmd' file first in VMD, then type 'source prefix_network.tcl' in the VMD console
! Visit 'www.ks.uiuc.edu/Research/vmd/' to learn more about VMD
! Code by Amir Kohanpur (kohanpu2@illinois.edu)
!

program tclwriter
	implicit none
	
	character(len=*), parameter :: network_prefix="mtsimon-s"
	character(len=*), parameter :: network_address="../example/"
	integer :: np,nt,i,io
	real :: l_ext,tmp, lx, ly, lz
	integer, allocatable, dimension(:) :: p1,p2
	real, allocatable, dimension(:) :: xp,yp,zp,rp,rt,lt
	real, allocatable, dimension(:) :: xt1,xt2,yt1,yt2,zt1,zt2
	real, allocatable, dimension(:) :: xt,yt,zt
	
	open(10,file=''//trim(adjustl(network_address))//''//trim(adjustl(network_prefix))//'_node1.dat',status='old')
	open(20,file=''//trim(adjustl(network_address))//''//trim(adjustl(network_prefix))//'_node2.dat',status='old')
	open(30,file=''//trim(adjustl(network_address))//''//trim(adjustl(network_prefix))//'_link1.dat',status='old')
	open(40,file=''//trim(adjustl(network_address))//''//trim(adjustl(network_prefix))//'_link2.dat',status='old')
	read(10,*) np, lx, ly, lz
	read(30,*) nt
	
	allocate(xp(1:np))
	allocate(yp(1:np))
	allocate(zp(1:np))
	allocate(rp(1:np))
	allocate(rt(1:nt))
	allocate(p1(1:nt))
	allocate(p2(1:nt))
	allocate(lt(1:nt))
	allocate(xt1(1:nt))
	allocate(xt2(1:nt))
	allocate(yt1(1:nt))
	allocate(yt2(1:nt))
	allocate(zt1(1:nt))
	allocate(zt2(1:nt))
	allocate(xt(1:nt))
	allocate(yt(1:nt))
	allocate(zt(1:nt))
	
	! read data
	do i = 1, np
		read(10,*) tmp, xp(i), yp(i), zp(i)
		read(20,*) tmp, tmp, rp(i)
	end do
	do i = 1, nt
		read(30,*) tmp, p1(i), p2(i), rt(i)
		read(40,*) tmp, tmp, tmp, tmp, tmp, lt(i)
	end do
	
	close(10)
	close(20)
	close(30)
	close(40)
	
	! throats coordinates
	l_ext = 0.0e-5
	do i = 1, nt
		if (p1(i) /= -1 .and. p2(i) /= 0 .and. p2(i) /= -1 .and. p1(i) /= 0) then ! inner throats
			xt1(i) = xp(p1(i))
			yt1(i) = yp(p1(i))
			zt1(i) = zp(p1(i))
			xt2(i) = xp(p2(i))
			yt2(i) = yp(p2(i))
			zt2(i) = zp(p2(i))
		else if (p1(i) == -1) then ! inlet throats
			xt1(i) = xp(p2(i))-lt(i)-l_ext
			yt1(i) = yp(p2(i))
			zt1(i) = zp(p2(i))
			xt2(i) = xp(p2(i))
			yt2(i) = yp(p2(i))
			zt2(i) = zp(p2(i))
		else if (p2(i) == -1) then ! inlet throats
			xt1(i) = xp(p1(i))
			yt1(i) = yp(p1(i))
			zt1(i) = zp(p1(i))
			xt2(i) = xp(p1(i))-lt(i)-l_ext
			yt2(i) = yp(p1(i))
			zt2(i) = zp(p1(i))
		else if (p1(i) == 0) then ! outlet throats
			xt1(i) = xp(p2(i))+lt(i)+l_ext
			yt1(i) = yp(p2(i))
			zt1(i) = zp(p2(i))
			xt2(i) = xp(p2(i))
			yt2(i) = yp(p2(i))
			zt2(i) = zp(p2(i))
		else if (p2(i) == 0) then ! outlet throats
			xt1(i) = xp(p1(i))
			yt1(i) = yp(p1(i))
			zt1(i) = zp(p1(i))
			xt2(i) = xp(p1(i))+lt(i)+l_ext
			yt2(i) = yp(p1(i))
			zt2(i) = zp(p1(i))
		end if
	end do
	
	do i = 1, nt
		xt(i) = 0.5*(xt1(i)+xt2(i))
		yt(i) = 0.5*(yt1(i)+yt2(i))
		zt(i) = 0.5*(zt1(i)+zt2(i))
	end do
	
	! header
	open(unit=50, file=''//trim(adjustl(network_prefix))//'_network.tcl',status='unknown')
	write(50,'(a)') adjustl('# TCL file for visualizing Statoil format PN in VMD (Code by Amir Kohanpur)')
	write(50,'(a)') 
	
	! pores
	write(50,'(a)') adjustl('graphics top color 4')
	do i = 1, np
		write(50,'(a)', advance='no', iostat=io) adjustl('graphics top sphere "')
		write(50, '(3e13.6)',    advance='no', iostat=io) xp(i), yp(i), zp(i)
		write(50,'(a)', advance='no', iostat=io) adjustl('" radius "')
		write(50, '(3e13.6)',    advance='no', iostat=io) rp(i)
		write(50,'(a)') adjustl('" resolution 60')
	end do
	write(50,'(a)')
	
	! throats
	write(50,'(a)') adjustl('graphics top color 4')
	do i = 1, nt
		write(50,'(a)', advance='no', iostat=io) adjustl('graphics top cylinder "')
		write(50, '(3e13.6)',    advance='no', iostat=io) xt1(i), yt1(i), zt1(i)
		write(50,'(a)', advance='no', iostat=io) adjustl('" "')
		write(50, '(3e13.6)',    advance='no', iostat=io) xt2(i), yt2(i), zt2(i)
		write(50,'(a)', advance='no', iostat=io) adjustl('" radius "')
		write(50, '(3e13.6)',    advance='no', iostat=io) rt(i)
		write(50,'(a)') adjustl('" resolution 60 filled yes')
	end do
	write(50,'(a)')
	
	close(50)
end program tclwriter
