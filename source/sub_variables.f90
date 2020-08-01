module sub_variables
	implicit none
	
	!!!
	! define all variables
	
	! network names
	character(len=50) :: l_images_name
	character(len=50) :: r_images_name
	character(len=50) :: f_images_name
	character(len=100) :: l_address
	character(len=100) :: r_address
	character(len=100) :: f_address
	
	! scalars
	integer :: i,j,k,p1,p2,io, staterr, maxconnp=50
	real :: l_ext, r_ext, resolution=3.20d-6
	real :: tmp, tmpa, tmpb, tmpa0, tmpb0
	real :: m_rmin, m_rmax, m_ravg, m_rdelta
	real :: l_maxedge, r_minedge
	real :: m_lx_frac, m_lx_actual, m_lx_ltavg, m_lx_ltijavg
	real :: l_lt_avg, r_lt_avg, l_lt_ij_avg, r_lt_ij_avg
	real :: lt_threshold, lt_threshold_m, std_factor, frac_mid
	real :: l_rp_std, l_lt_std
	real :: r_rp_std, r_lt_std
	integer :: j_in, j_out, j_m
	integer :: count_l, count_m, count_r, count_in, count_mid, count_out
	
	! left original
	integer :: lo_np,lo_nt, lo_np_in, lo_np_out
	real :: lo_lx, lo_ly, lo_lz
	integer, allocatable, dimension(:) :: lo_p1,lo_p2,lo_ip,lo_it,lo_connp, &
										  lo_inp,lo_outp,lo_int,lo_outt
	real, allocatable, dimension(:) :: lo_xp,lo_yp,lo_zp,lo_rp,lo_rt, &
									   lo_vp,lo_vt,lo_vp_clay,lo_vt_clay,lo_gp,lo_gt, &
									   lo_lt,lo_lt_tot,lo_lt_ij,lo_lp1,lo_lp2
	integer, allocatable, dimension(:,:) :: lo_ipnearp,lo_itnearp
	real, allocatable, dimension(:) :: lo_xt1, lo_yt1, lo_zt1, lo_xt2, lo_yt2, lo_zt2
	
	! left
	integer :: l_np, l_nt, l_np_in, l_np_out
	real :: l_lx, l_ly, l_lz
	integer, allocatable, dimension(:) :: l_p1,l_p2,l_ip,l_it,l_connp,l_inp,l_outp,l_itold,l_itnew, &
										  l_connp0,l_outp0
	real, allocatable, dimension(:) :: l_xp,l_yp,l_zp,l_rp,l_rt, &
									   l_vp,l_vt,l_vp_clay,l_vt_clay,l_gp,l_gt, &
									   l_lt,l_lt_tot,l_lt_ij,l_lp1,l_lp2
	integer, allocatable, dimension(:,:) :: l_ipnearp,l_itnearp
	real, allocatable, dimension(:) :: l_xt1, l_yt1, l_zt1, l_xt2, l_yt2, l_zt2
	integer, allocatable, dimension(:) :: l_ipback, l_itback

	! right original
	integer :: ro_np, ro_nt, ro_np_in, ro_np_out
	real :: ro_lx, ro_ly, ro_lz
	integer, allocatable, dimension(:) :: ro_p1,ro_p2,ro_ip,ro_it,ro_connp, &
										  ro_inp,ro_outp,ro_int,ro_outt
	real, allocatable, dimension(:) :: ro_xp,ro_yp,ro_zp,ro_rp,ro_rt, &
									   ro_vp,ro_vt,ro_vp_clay,ro_vt_clay,ro_gp,ro_gt, &
									   ro_lt,ro_lt_tot,ro_lt_ij,ro_lp1,ro_lp2
	integer, allocatable, dimension(:,:) :: ro_ipnearp,ro_itnearp
	real, allocatable, dimension(:) :: ro_xt1, ro_yt1, ro_zt1, ro_xt2, ro_yt2, ro_zt2
	
	! right
	integer :: r_np, r_nt, r_np_in, r_np_out
	real :: r_lx, r_ly, r_lz
	integer, allocatable, dimension(:) :: r_p1,r_p2,r_ip,r_it,r_connp,r_inp,r_outp,r_itold,r_itnew, &
										  r_connp0,r_inp0
	real, allocatable, dimension(:) :: r_xp,r_yp,r_zp,r_rp,r_rt, &
									   r_vp,r_vt,r_vp_clay,r_vt_clay,r_gp,r_gt, &
									   r_lt,r_lt_tot,r_lt_ij,r_lp1,r_lp2
	integer, allocatable, dimension(:,:) :: r_ipnearp,r_itnearp
	real, allocatable, dimension(:) :: r_xt1, r_yt1, r_zt1, r_xt2, r_yt2, r_zt2
	integer, allocatable, dimension(:) :: r_ipback, r_itback
	
	! middle
	integer :: m_np, m_nt
	integer :: m_yz_n, m_nx, m_ny, m_nz
	real :: m_lx, m_ly, m_lz
	real :: m_yz_delta, m_xdelta, m_ydelta, m_zdelta
	integer, allocatable, dimension(:) :: m_p1,m_p2,m_ip,m_it,m_connp,mo_connp,m_inp,m_outp
	real, allocatable, dimension(:) :: m_xp,m_yp,m_zp,m_rp,m_rt, &
									   m_vp,m_vt,m_vp_clay,m_vt_clay,m_gp,m_gt, &
									   m_lt,m_lt_tot,m_lt_ij,m_lp1,m_lp2
	integer, allocatable, dimension(:,:) :: m_ipnearp,m_itnearp
	real, allocatable, dimension(:) :: m_xt1, m_yt1, m_zt1, m_xt2, m_yt2, m_zt2
	integer, allocatable, dimension(:) :: m_ipback, m_itback
	
	! final
	integer :: f_np, f_nt
	real :: f_lx, f_ly, f_lz
	integer, allocatable, dimension(:) :: f_p1,f_p2,f_ip,f_it,f_connp,f_inp,f_outp
	real, allocatable, dimension(:) :: f_xp,f_yp,f_zp,f_rp,f_rt, &
									   f_vp,f_vt,f_vp_clay,f_vt_clay,f_gp,f_gt, &
									   f_lt,f_lt_tot,f_lt_ij,f_lp1,f_lp2
	integer, allocatable, dimension(:,:) :: f_ipnearp,f_itnearp
	real, allocatable, dimension(:) :: f_xt1, f_yt1, f_zt1, f_xt2, f_yt2, f_zt2
	
	!!!

end module sub_variables
