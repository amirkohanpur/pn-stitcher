subroutine sub_final_allocations
	use sub_variables
	
	!!!
	! final resulting network arrays
	f_np = l_np+m_np+r_np
	f_nt = l_nt+m_nt+r_nt
	allocate(f_ip(1:f_np))
	allocate(f_xp(1:f_np))
	allocate(f_yp(1:f_np))
	allocate(f_zp(1:f_np))
	allocate(f_rp(1:f_np))
	allocate(f_gp(1:f_np))
	allocate(f_vp(1:f_np))
	allocate(f_vp_clay(1:f_np))
	allocate(f_connp(1:f_np))
	allocate(f_inp(1:f_np))
	allocate(f_outp(1:f_np))
	allocate(f_it(1:f_nt))
	allocate(f_rt(1:f_nt))
	allocate(f_p1(1:f_nt))
	allocate(f_p2(1:f_nt))
	allocate(f_gt(1:f_nt))
	allocate(f_vt(1:f_nt))
	allocate(f_vt_clay(1:f_nt))
	allocate(f_lt(1:f_nt))
	allocate(f_lt_tot(1:f_nt))
	allocate(f_lt_ij(1:f_nt))
	allocate(f_lp1(1:f_nt))
	allocate(f_lp2(1:f_nt))
	allocate(f_ipnearp(1:f_np,1:maxconnp))
	allocate(f_itnearp(1:f_np,1:maxconnp))
	allocate(f_xt1(1:f_nt))
	allocate(f_yt1(1:f_nt))
	allocate(f_zt1(1:f_nt))
	allocate(f_xt2(1:f_nt))
	allocate(f_yt2(1:f_nt))
	allocate(f_zt2(1:f_nt))
	f_lx = l_lx + m_lx + r_lx
	f_ly = (l_ly + r_ly) / 2.0
	f_lz = (l_lz + r_lz) / 2.0
	!!!
	
end subroutine sub_final_allocations
