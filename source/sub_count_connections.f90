subroutine sub_count_connections
	use sub_variables
	
	!!!
	
	! define a threshold
	l_rp_std = std(l_rp,l_np)
	r_rp_std = std(r_rp,r_np)
	l_lt_std = std(l_lt,l_nt)
	r_lt_std = std(r_lt,r_nt)
	std_factor = 2.0
	lt_threshold_m = ((l_lt_avg+std_factor*l_lt_std) + (r_lt_avg+std_factor*r_lt_std))/2.0
	lt_threshold = 1.0*(maxval(l_lt)+maxval(r_lt))/2.0
	
	! process of connecting middle part to left and right parts -- counting number of connections
	count_l = 0
	count_m = 0
	count_r = 0
	do i = 1, m_np
		do j = 1, l_np ! over left part's outlet
			if (l_outp(j)>0) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),l_xp(j),l_yp(j),l_zp(j))
				if (tmp-(m_rp(i)+l_rp(j)) < lt_threshold) then
					count_l = count_l+1
				end if
			end if
		end do
		do j = i, m_np ! over middle part
			if (i .ne. j) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),m_xp(j),m_yp(j),m_zp(j))
				if (tmp-(m_rp(i)+m_rp(j)) < lt_threshold_m) then
					count_m = count_m+1
				end if	
			end if
		end do		
		do j = 1, r_np ! over right part's inlet
			if (r_inp(j)>0) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),r_xp(j),r_yp(j),r_zp(j))
				if (tmp-(m_rp(i)+r_rp(j)) < lt_threshold) then
					count_r = count_r+1
				end if
			end if	
		end do
	end do
	m_nt = count_l+count_r+count_m
	
	! allocation of memory after having number of throats in the middle part
	allocate(m_it(1:m_nt))
	allocate(m_rt(1:m_nt))
	allocate(m_p1(1:m_nt))
	allocate(m_p2(1:m_nt))
	allocate(m_gt(1:m_nt))
	allocate(m_vt(1:m_nt))
	allocate(m_vt_clay(1:m_nt))
	allocate(m_lt(1:m_nt))
	allocate(m_lt_tot(1:m_nt))
	allocate(m_lt_ij(1:m_nt))
	allocate(m_lp1(1:m_nt))
	allocate(m_lp2(1:m_nt))
	allocate(m_ipnearp(1:m_np,1:maxconnp))
	allocate(m_itnearp(1:m_np,1:maxconnp))
	allocate(m_xt1(1:m_nt))
	allocate(m_yt1(1:m_nt))
	allocate(m_zt1(1:m_nt))
	allocate(m_xt2(1:m_nt))
	allocate(m_yt2(1:m_nt))
	allocate(m_zt2(1:m_nt))
	
	!!!
	
end subroutine sub_count_connections
