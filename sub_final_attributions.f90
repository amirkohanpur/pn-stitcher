subroutine sub_final_attributions
	use sub_variables
	
	!!!
	! index relations that relates global pore index ot its old index in that very part (left, mid, right)
	! -9 means that the old index was not in this part
	allocate(l_ipback(1:f_np))
	allocate(m_ipback(1:f_np))
	allocate(r_ipback(1:f_np))
	allocate(l_itback(1:f_nt))
	allocate(m_itback(1:f_nt))
	allocate(r_itback(1:f_nt))
	do i = 1, f_np
		l_ipback(i) = -9
		m_ipback(i) = -9
		r_ipback(i) = -9
	end do
	do i = 1, f_nt
		l_itback(i) = -9
		m_itback(i) = -9
		r_itback(i) = -9
	end do
	! left
	do i = 1, l_np
		l_ipback(i) = i
	end do
	do i = 1, l_nt
		l_itback(i) = i
	end do
	! middle
	do i = 1, m_np
		m_ipback(l_np+i) = i
	end do
	do i = 1, m_nt
		m_itback(l_nt+i) = i
	end do
	! right
	do i = 1, r_np
		r_ipback(l_np+m_np+i) = i
	end do
	do i = 1, r_nt
		r_itback(l_nt+m_nt+i) = i
	end do
	! easy attributions
	! pores
	do i = 1, f_np
		f_ip(i) = i
		if (i <= l_np) then ! left
			f_rp(i) = l_rp(l_ipback(i))
			f_xp(i) = l_xp(l_ipback(i))
			f_yp(i) = l_yp(l_ipback(i))
			f_zp(i) = l_zp(l_ipback(i))
			f_gp(i) = l_gp(l_ipback(i))
			f_vp(i) = l_vp(l_ipback(i))
			f_vp_clay(i) = l_vp_clay(l_ipback(i))
			f_connp(i) = l_connp(l_ipback(i))
			f_inp(i) = l_inp(l_ipback(i))
			f_outp(i) = l_outp0(l_ipback(i))
		else if (i <= l_np+m_np) then ! middle
			f_rp(i) = m_rp(m_ipback(i))
			f_xp(i) = m_xp(m_ipback(i))
			f_yp(i) = m_yp(m_ipback(i))
			f_zp(i) = m_zp(m_ipback(i))
			f_gp(i) = m_gp(m_ipback(i))
			f_vp(i) = m_vp(m_ipback(i))
			f_vp_clay(i) = m_vp_clay(m_ipback(i))
			f_connp(i) = m_connp(m_ipback(i))
			f_inp(i) = m_inp(m_ipback(i))
			f_outp(i) = m_outp(m_ipback(i))
		else ! right
			f_rp(i) = r_rp(r_ipback(i))
			f_xp(i) = r_xp(r_ipback(i))
			f_yp(i) = r_yp(r_ipback(i))
			f_zp(i) = r_zp(r_ipback(i))
			f_gp(i) = r_gp(r_ipback(i))
			f_vp(i) = r_vp(r_ipback(i))
			f_vp_clay(i) = r_vp_clay(r_ipback(i))
			f_connp(i) = r_connp(r_ipback(i))
			f_inp(i) = r_inp0(r_ipback(i))
			f_outp(i) = r_outp(r_ipback(i))
		end if
	end do
	! throats
	do i = 1, f_nt
		f_it(i) = i
		if (i <= l_nt) then ! left
			f_rt(i) = l_rt(l_itback(i))
			f_lt(i) = l_lt(l_itback(i))
			f_lt_tot(i) = l_lt_tot(l_itback(i))
			f_lt_ij(i) = l_lt_ij(l_itback(i))
			f_p1(i) = l_p1(l_itback(i)) ! no need anything to be added
			f_p2(i) = l_p2(l_itback(i)) ! no need anything to be added
			f_lp1(i) = l_lp1(l_itback(i))
			f_lp2(i) = l_lp2(l_itback(i))
			f_gt(i) = l_gt(l_itback(i))
			f_vt(i) = l_vt(l_itback(i))
			f_vt_clay(i) = l_vt_clay(l_itback(i))
			f_xt1(i) = l_xt1(l_itback(i))
			f_yt1(i) = l_yt1(l_itback(i))
			f_zt1(i) = l_zt1(l_itback(i))
			f_xt2(i) = l_xt2(l_itback(i))
			f_yt2(i) = l_yt2(l_itback(i))
			f_zt2(i) = l_zt2(l_itback(i))
		else if (i <= l_nt+m_nt) then ! middle
			f_rt(i) = m_rt(m_itback(i))
			f_lt(i) = m_lt(m_itback(i))
			f_lt_tot(i) = m_lt_tot(m_itback(i))
			f_lt_ij(i) = m_lt_ij(m_itback(i))
			f_p1(i) = m_p1(m_itback(i)) ! already fixed
			f_p2(i) = m_p2(m_itback(i)) ! already fixed
			f_lp1(i) = m_lp1(m_itback(i))
			f_lp2(i) = m_lp2(m_itback(i))
			f_gt(i) = m_gt(m_itback(i))
			f_vt(i) = m_vt(m_itback(i))
			f_vt_clay(i) = m_vt_clay(m_itback(i))
			f_xt1(i) = m_xt1(m_itback(i))
			f_yt1(i) = m_yt1(m_itback(i))
			f_zt1(i) = m_zt1(m_itback(i))
			f_xt2(i) = m_xt2(m_itback(i))
			f_yt2(i) = m_yt2(m_itback(i))
			f_zt2(i) = m_zt2(m_itback(i))
		else ! right
			f_rt(i) = r_rt(r_itback(i))
			f_lt(i) = r_lt(r_itback(i))
			f_lt_tot(i) = r_lt_tot(r_itback(i))
			f_lt_ij(i) = r_lt_ij(r_itback(i))
			if (r_p1(r_itback(i)) == 0) then ! if p1 is outlet
				f_p1(i) = r_p1(r_itback(i)) ! index stays zero
				f_p2(i) = r_p2(r_itback(i)) + (l_np+m_np) ! index should be added
			else if (r_p2(r_itback(i)) == 0) then	! if p2 is outlet
				f_p1(i) = r_p1(r_itback(i)) + (l_np+m_np) ! index should be added
				f_p2(i) = r_p2(r_itback(i)) ! index stays zero
			else ! if neither are outlet
				f_p1(i) = r_p1(r_itback(i)) + (l_np+m_np) ! index should be added
				f_p2(i) = r_p2(r_itback(i)) + (l_np+m_np) ! index should be added
			end if
			f_lp1(i) = r_lp1(r_itback(i))
			f_lp2(i) = r_lp2(r_itback(i))
			f_gt(i) = r_gt(r_itback(i))
			f_vt(i) = r_vt(r_itback(i))
			f_vt_clay(i) = r_vt_clay(r_itback(i))
			f_xt1(i) = r_xt1(r_itback(i))
			f_yt1(i) = r_yt1(r_itback(i))
			f_zt1(i) = r_zt1(r_itback(i))
			f_xt2(i) = r_xt2(r_itback(i))
			f_yt2(i) = r_yt2(r_itback(i))
			f_zt2(i) = r_zt2(r_itback(i))
		end if
	end do
	! redefine indices of neighboring pores and throats in the final network
	do i = 1, f_np
		! left part
		if (i <= l_np) then
			if (l_outp(l_ipback(i)) > 0) then ! left part outlet
				do j = 1, l_connp0(l_ipback(i))
					f_ipnearp(i,j) = l_ipnearp(l_ipback(i),j)
					f_itnearp(i,j) = l_itnearp(l_ipback(i),j)
				end do
			else ! left part other than outlet 
				do j = 1, f_connp(i)
					f_ipnearp(i,j) = l_ipnearp(l_ipback(i),j)
					f_itnearp(i,j) = l_itnearp(l_ipback(i),j)
				end do
			end if
		! no middle part here
		! right part
		else if (i > l_np+m_np) then
			if (r_inp(r_ipback(i)) > 0) then ! right part inlet
				do j = 1, r_connp0(r_ipback(i))
					f_ipnearp(i,j) = r_ipnearp(r_ipback(i),j) + (l_np+m_np)
					f_itnearp(i,j) = r_itnearp(r_ipback(i),j) + (l_nt+m_nt)
				end do
			else ! right part other than inlet
				do j = 1, f_connp(i)
					f_ipnearp(i,j) = r_ipnearp(r_ipback(i),j) + (l_np+m_np)
					f_itnearp(i,j) = r_itnearp(r_ipback(i),j) + (l_nt+m_nt)
				end do
			end if
		end if
	end do
	!!!
	
end subroutine sub_final_attributions
