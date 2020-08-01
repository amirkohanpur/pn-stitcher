subroutine sub_neighbor_pores
	use sub_variables
	
	!!!
	! defining neighborig pores coordinations of throats
	l_ext = 0.0e-5
	r_ext = 0.0e-5
	! left part
	do i = 1, l_nt
		if (l_p1(i) /=-1 .and. l_p2(i) /=0 .and. l_p2(i) /=-1 .and. l_p1(i) /=0) then ! inner throats
			l_xt1(i) = l_xp(l_p1(i))
			l_yt1(i) = l_yp(l_p1(i))
			l_zt1(i) = l_zp(l_p1(i))
			l_xt2(i) = l_xp(l_p2(i))
			l_yt2(i) = l_yp(l_p2(i))
			l_zt2(i) = l_zp(l_p2(i))
		else if (l_p1(i) ==-1) then ! inlet throats
			l_xt1(i) = l_xp(l_p2(i))-l_lt(i)-l_ext
			l_yt1(i) = l_yp(l_p2(i))
			l_zt1(i) = l_zp(l_p2(i))
			l_xt2(i) = l_xp(l_p2(i))
			l_yt2(i) = l_yp(l_p2(i))
			l_zt2(i) = l_zp(l_p2(i))
		else if (l_p2(i) ==-1) then ! inlet throats
			l_xt1(i) = l_xp(l_p1(i))
			l_yt1(i) = l_yp(l_p1(i))
			l_zt1(i) = l_zp(l_p1(i))
			l_xt2(i) = l_xp(l_p1(i))-l_lt(i)-l_ext
			l_yt2(i) = l_yp(l_p1(i))
			l_zt2(i) = l_zp(l_p1(i))			
		else if (l_p2(i) ==0) then ! outlet throats
			l_xt1(i) = l_xp(l_p1(i))
			l_yt1(i) = l_yp(l_p1(i))
			l_zt1(i) = l_zp(l_p1(i))
			l_xt2(i) = l_xp(l_p1(i))+l_lt(i)+l_ext
			l_yt2(i) = l_yp(l_p1(i))
			l_zt2(i) = l_zp(l_p1(i))
		else if (l_p1(i) ==0) then ! outlet throats
			l_xt1(i) = l_xp(l_p2(i))+l_lt(i)+l_ext
			l_yt1(i) = l_yp(l_p2(i))
			l_zt1(i) = l_zp(l_p2(i))
			l_xt2(i) = l_xp(l_p2(i))
			l_yt2(i) = l_yp(l_p2(i))
			l_zt2(i) = l_zp(l_p2(i))
		end if
	end do
	! right part
	do i = 1, r_nt
		if (r_p1(i) /=-1 .and. r_p2(i) /=0 .and. r_p2(i) /=-1 .and. r_p1(i) /=0) then ! inner throats
			r_xt1(i) = r_xp(r_p1(i))
			r_yt1(i) = r_yp(r_p1(i))
			r_zt1(i) = r_zp(r_p1(i))
			r_xt2(i) = r_xp(r_p2(i))
			r_yt2(i) = r_yp(r_p2(i))
			r_zt2(i) = r_zp(r_p2(i))
		else if (r_p1(i) ==-1) then ! inlet throats
			r_xt1(i) = r_xp(r_p2(i))-r_lt(i)-r_ext
			r_yt1(i) = r_yp(r_p2(i))
			r_zt1(i) = r_zp(r_p2(i))
			r_xt2(i) = r_xp(r_p2(i))
			r_yt2(i) = r_yp(r_p2(i))
			r_zt2(i) = r_zp(r_p2(i))
		else if (r_p2(i) ==-1) then ! inlet throats
			r_xt1(i) = r_xp(r_p1(i))
			r_yt1(i) = r_yp(r_p1(i))
			r_zt1(i) = r_zp(r_p1(i))
			r_xt2(i) = r_xp(r_p1(i))-r_lt(i)-r_ext
			r_yt2(i) = r_yp(r_p1(i))
			r_zt2(i) = r_zp(r_p1(i))			
		else if (r_p2(i) ==0) then ! outlet throats
			r_xt1(i) = r_xp(r_p1(i))
			r_yt1(i) = r_yp(r_p1(i))
			r_zt1(i) = r_zp(r_p1(i))
			r_xt2(i) = r_xp(r_p1(i))+r_lt(i)+r_ext
			r_yt2(i) = r_yp(r_p1(i))
			r_zt2(i) = r_zp(r_p1(i))
		else if (r_p1(i) ==0) then ! outlet throats
			r_xt1(i) = r_xp(r_p2(i))+r_lt(i)+r_ext
			r_yt1(i) = r_yp(r_p2(i))
			r_zt1(i) = r_zp(r_p2(i))
			r_xt2(i) = r_xp(r_p2(i))
			r_yt2(i) = r_yp(r_p2(i))
			r_zt2(i) = r_zp(r_p2(i))
		end if
	end do
	! done with defining neighborig pores coordinations of throats
	!!!
	
end subroutine sub_neighbor_pores
