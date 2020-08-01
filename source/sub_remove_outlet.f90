subroutine sub_remove_outlet
	use sub_variables
	
	!!! read input network
	! open files
	open(10,file=''//trim(adjustl(l_address))//''//trim(adjustl(l_images_name))//'_node1.dat',status='old')
	open(20,file=''//trim(adjustl(l_address))//''//trim(adjustl(l_images_name))//'_node2.dat',status='old')
	open(30,file=''//trim(adjustl(l_address))//''//trim(adjustl(l_images_name))//'_link1.dat',status='old')
	open(40,file=''//trim(adjustl(l_address))//''//trim(adjustl(l_images_name))//'_link2.dat',status='old')
	read(10,*) lo_np, lo_lx, lo_ly, lo_lz
	read(30,*) lo_nt
	! define arrays
	allocate(lo_ip(1:lo_np))
	allocate(lo_xp(1:lo_np))
	allocate(lo_yp(1:lo_np))
	allocate(lo_zp(1:lo_np))
	allocate(lo_rp(1:lo_np))
	allocate(lo_gp(1:lo_np))
	allocate(lo_vp(1:lo_np))
	allocate(lo_vp_clay(1:lo_np))
	allocate(lo_connp(1:lo_np))
	allocate(lo_inp(1:lo_np))
	allocate(lo_outp(1:lo_np))
	allocate(lo_it(1:lo_nt))
	allocate(lo_rt(1:lo_nt))
	allocate(lo_p1(1:lo_nt))
	allocate(lo_p2(1:lo_nt))
	allocate(lo_gt(1:lo_nt))
	allocate(lo_vt(1:lo_nt))
	allocate(lo_vt_clay(1:lo_nt))
	allocate(lo_lt(1:lo_nt))
	allocate(lo_lt_ij(1:lo_nt))
	allocate(lo_lt_tot(1:lo_nt))
	allocate(lo_lp1(1:lo_nt))
	allocate(lo_lp2(1:lo_nt))
	allocate(lo_int(1:lo_nt))
	allocate(lo_outt(1:lo_nt))
	allocate(lo_ipnearp(1:lo_np,1:maxconnp))
	allocate(lo_itnearp(1:lo_np,1:maxconnp))
	allocate(lo_xt1(1:lo_nt))
	allocate(lo_yt1(1:lo_nt))
	allocate(lo_zt1(1:lo_nt))
	allocate(lo_xt2(1:lo_nt))
	allocate(lo_yt2(1:lo_nt))
	allocate(lo_zt2(1:lo_nt))
	! fill arrays
	do i = 1, lo_np
		do j = 1, maxconnp
			lo_ipnearp(i,j) = -9 ! -9 means no index there
			lo_itnearp(i,j) = -9 ! -9 means no index there
		end do
	end do
	do i = 1, lo_np
		read(10,*) lo_ip(i), lo_xp(i), lo_yp(i), lo_zp(i), lo_connp(i), (lo_ipnearp(i,j), j = 1, lo_connp(i)), &
				   lo_inp(i), lo_outp(i), (lo_itnearp(i,j), j = 1, lo_connp(i))
		read(20,*) tmp, lo_vp(i), lo_rp(i), lo_gp(i), lo_vp_clay(i)
	end do
	do i = 1, lo_nt
		read(30,*) lo_it(i), lo_p1(i), lo_p2(i), lo_rt(i), lo_gt(i), lo_lt_tot(i)
		read(40,*) tmp, tmp, tmp, lo_lp1(i), lo_lp2(i), lo_lt(i), lo_vt(i), lo_vt_clay(i)
	end do
	! in/outlet status
	lo_np_in = sum(lo_inp)
	lo_np_out = sum(lo_outp)
	count_in = 0
	count_out = 0
	count_mid = 0
	do i=1,lo_nt
		if (lo_p1(i) == -1 .or. lo_p2(i) == -1) then
			count_in = count_in+1
			lo_int(i) = 1
			lo_outt(i) = 0
		else if (lo_p1(i) == 0 .or. lo_p2(i) == 0) then
			count_out = count_out+1
			lo_int(i) = 0
			lo_outt(i) = 1
		else
			count_mid = count_mid+1
			lo_int(i) = 0
			lo_outt(i) = 0
		end if
	end do
	! storing pore-to-pore distance for throats
	do i = 1, lo_nt
		lo_lt_ij(i) = lo_lp1(i) + lo_lt(i) + lo_lp2(i)
	end do
	close(10)
	close(20)
	close(30)
	close(40)
	!!!
	
	! removing outlet throats
	! define new network variables
	l_lx = lo_lx
	l_ly = lo_ly
	l_lz = lo_lz
	l_np = lo_np
	l_nt = lo_nt - lo_np_out
	l_np_in = lo_np_in
	l_np_out = lo_np_out
	allocate(l_ip(1:l_np))
	allocate(l_xp(1:l_np))
	allocate(l_yp(1:l_np))
	allocate(l_zp(1:l_np))
	allocate(l_rp(1:l_np))
	allocate(l_gp(1:l_np))
	allocate(l_vp(1:l_np))
	allocate(l_vp_clay(1:l_np))
	allocate(l_connp(1:l_np))
	allocate(l_connp0(1:l_np))
	allocate(l_inp(1:l_np))
	allocate(l_outp(1:l_np))
	allocate(l_outp0(1:l_np))
	allocate(l_it(1:l_nt))
	allocate(l_rt(1:l_nt))
	allocate(l_p1(1:l_nt))
	allocate(l_p2(1:l_nt))
	allocate(l_gt(1:l_nt))
	allocate(l_vt(1:l_nt))
	allocate(l_vt_clay(1:l_nt))
	allocate(l_lt(1:l_nt))
	allocate(l_lt_tot(1:l_nt))
	allocate(l_lt_ij(1:l_nt))
	allocate(l_lp1(1:l_nt))
	allocate(l_lp2(1:l_nt))
	allocate(l_ipnearp(1:l_np,1:maxconnp))
	allocate(l_itnearp(1:l_np,1:maxconnp))
	allocate(l_xt1(1:l_nt))
	allocate(l_yt1(1:l_nt))
	allocate(l_zt1(1:l_nt))
	allocate(l_xt2(1:l_nt))
	allocate(l_yt2(1:l_nt))
	allocate(l_zt2(1:l_nt))
	allocate(l_itold(1:l_nt))  ! means the old index of throat in the old format
	allocate(l_itnew(1:lo_nt)) ! means the new index of throat in the new format
	! relation of new and old throat indices
	count_l = 0
	do i=1,lo_nt
		if (lo_outt(i) == 1) then
			count_l = count_l+1
			l_itnew(i) = -9 ! -9 means no corresponding new index
		else
			l_itnew(i) = i-count_l ! having the new index from the old one
			l_itold(i-count_l) = i ! having the old index from the new one
		end if
	end do
	! rewriting
	do i=1,l_np
		l_ip(i) = lo_ip(i)
		l_xp(i) = lo_xp(i)
		l_yp(i) = lo_yp(i)
		l_zp(i) = lo_zp(i)
		l_rp(i) = lo_rp(i)
		l_gp(i) = lo_gp(i)
		l_vp(i) = lo_vp(i)
		l_vp_clay(i) = lo_vp_clay(i)
		l_connp(i) = lo_connp(i)
		l_inp(i) = lo_inp(i)
		l_outp(i) = lo_outp(i)
	end do
	do i=1,l_nt
		l_it(i) = i
		l_rt(i) = lo_rt(l_itold(i))
		l_p1(i) = lo_p1(l_itold(i))
		l_p2(i) = lo_p2(l_itold(i))
		l_gt(i) = lo_gt(l_itold(i))
		l_vt(i) = lo_vt(l_itold(i))
		l_vt_clay(i) = lo_vt_clay(l_itold(i))
		l_lt(i) = lo_lt(l_itold(i))
		l_lt_tot(i) = lo_lt_tot(l_itold(i))
		l_lp1(i) = lo_lp1(l_itold(i))
		l_lp2(i) = lo_lp2(l_itold(i))
		l_lt_ij(i) = lo_lt_ij(l_itold(i))
	end do
	! modify connection number due to removing
	do i = 1, l_np
		l_outp0(i) = l_outp(i)
		if (l_outp(i) == 1) then
			l_connp(i) = lo_connp(i)-1
			l_outp0(i) = 0
		end if
		l_connp0(i) = l_connp(i)
	end do
	! modify near indices
	do i = 1, l_np
		do j = 1, maxconnp
			l_ipnearp(i,j) = -9 ! -9 means no such a connection
			l_itnearp(i,j) = -9 ! -9 means no such a connection
		end do
	end do
	do i = 1, l_np
		j_out = 0
		do j = 1, lo_connp(i)
			if (lo_ipnearp(i,j) == 0) then
				j_out = j_out+1
			else
				l_ipnearp(i,j-j_out) = lo_ipnearp(i,j)
				l_itnearp(i,j-j_out) = l_itnew(lo_itnearp(i,j))
			end if
		end do
	end do
	!!!
	
end subroutine sub_remove_outlet
