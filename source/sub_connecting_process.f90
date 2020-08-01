subroutine sub_connecting_process
	use sub_variables
	
	!!!
	! middle part values
	j_m = 0 ! local counter for middle part pores
	do i = 1, m_np
		do j = 1, l_np ! over left part's outlet
			if (l_outp(j)>0) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),l_xp(j),l_yp(j),l_zp(j))
				if (tmp-(m_rp(i)+l_rp(j)) < lt_threshold) then
					j_m = j_m+1
					m_it(j_m) = j_m
					m_p1(j_m) = l_np+m_ip(i)	! defined globally
					m_p2(j_m) = l_ip(j)			! defined globally
					m_connp(i) = m_connp(i)+1 ! connection number of middle part pores
					l_connp(j) = l_connp(j)+1 ! connections from left outlet pores to the middle part
					
					m_lp1(j_m) = m_rp(i)
					m_lp2(j_m) = l_rp(j)
					m_lt(j_m) = tmp-m_lp1(j_m)-m_lp2(j_m)
					m_lt_ij(j_m) = tmp
					m_lt_tot(j_m) = 1.0*tmp
					m_xt1(j_m) = m_xp(i)
					m_yt1(j_m) = m_yp(i)
					m_zt1(j_m) = m_zp(i)
					m_xt2(j_m) = l_xp(j)
					m_yt2(j_m) = l_yp(j)
					m_zt2(j_m) = l_zp(j)
					
					f_ipnearp(m_p1(j_m),m_connp(i)) = m_p2(j_m)
					f_ipnearp(m_p2(j_m),l_connp(j)) = m_p1(j_m)
					f_itnearp(m_p1(j_m),m_connp(i)) = j_m + l_nt
					f_itnearp(m_p2(j_m),l_connp(j)) = j_m + l_nt
				end if
			end if
		end do
		do j = i, m_np ! loop over middle part pores
			if (i .ne. j) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),m_xp(j),m_yp(j),m_zp(j))
				if (tmp-(m_rp(i)+m_rp(j)) < lt_threshold_m) then
					j_m = j_m+1
					m_it(j_m) = j_m
					m_p1(j_m) = l_np+m_ip(i)	! defined globally
					m_p2(j_m) = l_np+m_ip(j)	! defined globally
					m_connp(i) = m_connp(i)+1 ! connection number of the first  pore in the middle part
					m_connp(j) = m_connp(j)+1 ! connection number of the second pore in the middle part
					write(77,*) j_m, "   M", m_p1(j_m), m_p2(j_m)
					
					m_lp1(j_m) = m_rp(i)
					m_lp2(j_m) = m_rp(j)	
					m_lt(j_m) = tmp-m_lp1(j_m)-m_lp2(j_m)
					m_lt_ij(j_m) = tmp
					m_lt_tot(j_m) = 1.0*tmp
					m_xt1(j_m) = m_xp(i)
					m_yt1(j_m) = m_yp(i)
					m_zt1(j_m) = m_zp(i)
					m_xt2(j_m) = m_xp(j)
					m_yt2(j_m) = m_yp(j)
					m_zt2(j_m) = m_zp(j)
					
					f_ipnearp(m_p1(j_m),m_connp(i)) = m_p2(j_m)
					f_ipnearp(m_p2(j_m),m_connp(j)) = m_p1(j_m)
					f_itnearp(m_p1(j_m),m_connp(i)) = j_m + l_nt
					f_itnearp(m_p2(j_m),m_connp(j)) = j_m + l_nt
				end if	
			end if
		end do			
		do j = 1, r_np ! over right part's inlet
			if (r_inp(j)>0) then
				tmp = dist(m_xp(i),m_yp(i),m_zp(i),r_xp(j),r_yp(j),r_zp(j))
				if (tmp-(m_rp(i)+r_rp(j)) < lt_threshold) then
					j_m = j_m+1
					m_it(j_m) = j_m
					m_p1(j_m) = l_np+m_ip(i)		! defined globally
					m_p2(j_m) = l_np+m_np+r_ip(j)	! defined globally
					m_connp(i) = m_connp(i)+1 ! connection number of middle part pores
					r_connp(j) = r_connp(j)+1 ! connections from right inlet pores to the middle part
					write(77,*) j_m, "   R", m_p1(j_m), m_p2(j_m)
					
					m_lp1(j_m) = m_rp(i)
					m_lp2(j_m) = r_rp(j)
					m_lt(j_m) = tmp-m_lp1(j_m)-m_lp2(j_m)
					m_lt_ij(j_m) = tmp
					m_lt_tot(j_m) = 1.0*tmp
					m_xt1(j_m) = m_xp(i)
					m_yt1(j_m) = m_yp(i)
					m_zt1(j_m) = m_zp(i)
					m_xt2(j_m) = r_xp(j)
					m_yt2(j_m) = r_yp(j)
					m_zt2(j_m) = r_zp(j)
					
					f_ipnearp(m_p1(j_m),m_connp(i)) = m_p2(j_m)
					f_ipnearp(m_p2(j_m),r_connp(j)) = m_p1(j_m)
					f_itnearp(m_p1(j_m),m_connp(i)) = j_m + l_nt
					f_itnearp(m_p2(j_m),r_connp(j)) = j_m + l_nt
				end if	
			end if	
		end do
	end do
	!!!
	
end subroutine sub_connecting_process
