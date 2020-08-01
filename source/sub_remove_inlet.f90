subroutine sub_remove_inlet
	use sub_variables
	
	!!! read input network
	! open files
	open(10,file=''//trim(adjustl(r_address))//''//trim(adjustl(r_images_name))//'_node1.dat',status='old')
	open(20,file=''//trim(adjustl(r_address))//''//trim(adjustl(r_images_name))//'_node2.dat',status='old')
	open(30,file=''//trim(adjustl(r_address))//''//trim(adjustl(r_images_name))//'_link1.dat',status='old')
	open(40,file=''//trim(adjustl(r_address))//''//trim(adjustl(r_images_name))//'_link2.dat',status='old')
	read(10,*) ro_np, ro_lx, ro_ly, ro_lz
	read(30,*) ro_nt
	! define arrays
	allocate(ro_ip(1:ro_np))
	allocate(ro_xp(1:ro_np))
	allocate(ro_yp(1:ro_np))
	allocate(ro_zp(1:ro_np))
	allocate(ro_rp(1:ro_np))
	allocate(ro_gp(1:ro_np))
	allocate(ro_vp(1:ro_np))
	allocate(ro_vp_clay(1:ro_np))
	allocate(ro_connp(1:ro_np))
	allocate(ro_inp(1:ro_np))
	allocate(ro_outp(1:ro_np))
	allocate(ro_it(1:ro_nt))
	allocate(ro_rt(1:ro_nt))
	allocate(ro_p1(1:ro_nt))
	allocate(ro_p2(1:ro_nt))
	allocate(ro_gt(1:ro_nt))
	allocate(ro_vt(1:ro_nt))
	allocate(ro_vt_clay(1:ro_nt))
	allocate(ro_lt(1:ro_nt))
	allocate(ro_lt_ij(1:ro_nt))
	allocate(ro_lt_tot(1:ro_nt))
	allocate(ro_lp1(1:ro_nt))
	allocate(ro_lp2(1:ro_nt))
	allocate(ro_int(1:ro_nt))
	allocate(ro_outt(1:ro_nt))
	allocate(ro_ipnearp(1:ro_np,1:maxconnp))
	allocate(ro_itnearp(1:ro_np,1:maxconnp))
	allocate(ro_xt1(1:ro_nt))
	allocate(ro_yt1(1:ro_nt))
	allocate(ro_zt1(1:ro_nt))
	allocate(ro_xt2(1:ro_nt))
	allocate(ro_yt2(1:ro_nt))
	allocate(ro_zt2(1:ro_nt))
	! fill arrays
	do i = 1, ro_np
		do j = 1, maxconnp
			ro_ipnearp(i,j) = -9 ! -9 means no index there
			ro_itnearp(i,j) = -9 ! -9 means no index there
		end do
	end do
	do i = 1, ro_np
		read(10,*) ro_ip(i), ro_xp(i), ro_yp(i), ro_zp(i), ro_connp(i), (ro_ipnearp(i,j), j = 1, ro_connp(i)), &
				   ro_inp(i), ro_outp(i), (ro_itnearp(i,j), j = 1, ro_connp(i))
		read(20,*) tmp, ro_vp(i), ro_rp(i), ro_gp(i), ro_vp_clay(i)
	end do
	do i = 1, ro_nt
		read(30,*) ro_it(i), ro_p1(i), ro_p2(i), ro_rt(i), ro_gt(i), ro_lt_tot(i)
		read(40,*) tmp, tmp, tmp, ro_lp1(i), ro_lp2(i), ro_lt(i), ro_vt(i), ro_vt_clay(i)
	end do
	! in/outlet status
	ro_np_in = sum(ro_inp)
	ro_np_out = sum(ro_outp)
	count_in = 0
	count_out = 0
	count_mid = 0
	do i=1,ro_nt
		if (ro_p1(i) == -1 .or. ro_p2(i) == -1) then
			count_in = count_in+1
			ro_int(i) = 1
			ro_outt(i) = 0
		else if (ro_p1(i) == 0 .or. ro_p2(i) == 0) then
			count_out = count_out+1
			ro_int(i) = 0
			ro_outt(i) = 1
		else
			count_mid = count_mid+1
			ro_int(i) = 0
			ro_outt(i) = 0
		end if
	end do
	! storing pore-to-pore distance for throats
	do i = 1, ro_nt
		ro_lt_ij(i) = ro_lp1(i) + ro_lt(i) + ro_lp2(i)
	end do
	close(10)
	close(20)
	close(30)
	close(40)
	!!!

	!!! removing inlet throats
	! define new network variables
	r_lx = ro_lx
	r_ly = ro_ly
	r_lz = ro_lz
	r_np = ro_np
	r_nt = ro_nt - ro_np_in
	r_np_in = ro_np_in
	r_np_out = ro_np_out
	allocate(r_ip(1:r_np))
	allocate(r_xp(1:r_np))
	allocate(r_yp(1:r_np))
	allocate(r_zp(1:r_np))
	allocate(r_rp(1:r_np))
	allocate(r_gp(1:r_np))
	allocate(r_vp(1:r_np))
	allocate(r_vp_clay(1:r_np))
	allocate(r_connp(1:r_np))
	allocate(r_connp0(1:r_np))
	allocate(r_inp(1:r_np))
	allocate(r_inp0(1:r_np))
	allocate(r_outp(1:r_np))
	allocate(r_it(1:r_nt))
	allocate(r_rt(1:r_nt))
	allocate(r_p1(1:r_nt))
	allocate(r_p2(1:r_nt))
	allocate(r_gt(1:r_nt))
	allocate(r_vt(1:r_nt))
	allocate(r_vt_clay(1:r_nt))
	allocate(r_lt(1:r_nt))
	allocate(r_lt_tot(1:r_nt))
	allocate(r_lt_ij(1:r_nt))
	allocate(r_lp1(1:r_nt))
	allocate(r_lp2(1:r_nt))
	allocate(r_ipnearp(1:r_np,1:maxconnp))
	allocate(r_itnearp(1:r_np,1:maxconnp))
	allocate(r_xt1(1:r_nt))
	allocate(r_yt1(1:r_nt))
	allocate(r_zt1(1:r_nt))
	allocate(r_xt2(1:r_nt))
	allocate(r_yt2(1:r_nt))
	allocate(r_zt2(1:r_nt))
	allocate(r_itold(1:r_nt))  ! means the old index of throat in the old format
	allocate(r_itnew(1:ro_nt)) ! means the new index of throat in the new format
	! relation of new and old throat indices
	count_r = 0
	do i=1,ro_nt
		if (ro_int(i) == 1) then
			count_r = count_r+1
			r_itnew(i) = -9 ! -9 means no corresponding new index
		else
			r_itnew(i) = i-count_r ! having the new index from the old one
			r_itold(i-count_r) = i ! having the old index from the new one
		end if
	end do
	! rewriting
	do i=1,r_np
		r_ip(i) = ro_ip(i)
		r_xp(i) = ro_xp(i)
		r_yp(i) = ro_yp(i)
		r_zp(i) = ro_zp(i)
		r_rp(i) = ro_rp(i)
		r_gp(i) = ro_gp(i)
		r_vp(i) = ro_vp(i)
		r_vp_clay(i) = ro_vp_clay(i)
		r_connp(i) = ro_connp(i)
		r_inp(i) = ro_inp(i)
		r_outp(i) = ro_outp(i)
	end do
	do i=1,r_nt
		r_it(i) = i
		r_rt(i) = ro_rt(r_itold(i))
		r_p1(i) = ro_p1(r_itold(i))
		r_p2(i) = ro_p2(r_itold(i))
		r_gt(i) = ro_gt(r_itold(i))
		r_vt(i) = ro_vt(r_itold(i))
		r_vt_clay(i) = ro_vt_clay(r_itold(i))
		r_lt(i) = ro_lt(r_itold(i))
		r_lt_tot(i) = ro_lt_tot(r_itold(i))
		r_lp1(i) = ro_lp1(r_itold(i))
		r_lp2(i) = ro_lp2(r_itold(i))
		r_lt_ij(i) = ro_lt_ij(r_itold(i))
	end do
	! modify connection number due to removing
	do i = 1, r_np
		r_inp0(i) = r_inp(i)
		if (r_inp(i) == 1) then
			r_connp(i) = ro_connp(i)-1
			r_inp0(i) = 0
		end if
		r_connp0(i) = r_connp(i)
	end do
	! modify near indices
	do i = 1, r_np
		do j = 1, maxconnp
			r_ipnearp(i,j) = -9 ! -9 means no such a connection
			r_itnearp(i,j) = -9 ! -9 means no such a connection
		end do
	end do
	do i = 1, r_np
		j_in = 0
		do j = 1, ro_connp(i)
			if (ro_ipnearp(i,j) == -1) then
				j_in = j_in+1
			else
				r_ipnearp(i,j-j_in) = ro_ipnearp(i,j)
				r_itnearp(i,j-j_in) = r_itnew(ro_itnearp(i,j))
			end if
		end do
	end do
	!!!
	
end subroutine sub_remove_inlet
