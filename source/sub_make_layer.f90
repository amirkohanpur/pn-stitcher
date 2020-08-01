subroutine sub_make_layer
	use sub_variables
	
	!!!
	
	! middle part size
	frac_mid = 0.9
	m_lx_frac = frac_mid*(l_lx+r_lx)/2
	
	l_lt_avg = avg(l_lt,l_nt)
	r_lt_avg = avg(r_lt,r_nt)
	m_lx_ltavg = (l_lt_avg+r_lt_avg)
	
	l_lt_ij_avg = avg(l_lt_ij,l_nt)
	r_lt_ij_avg = avg(r_lt_ij,r_nt)
	m_lx_ltijavg = (l_lt_ij_avg+r_lt_ij_avg)
	
	m_lx = m_lx_ltijavg*0.95
	m_ly = (l_ly + r_ly)/2.0
	m_lz = (l_lz + r_lz)/2.0
	
	! shift locations for the right part
	do i = 1, r_np
		r_xp(i) = r_xp(i) + m_lx + l_lx
	end do
	
	! find actual ends of parts
	l_maxedge = 0
	do i = 1, l_np
		if (l_outp(i)>0) then
			tmp = l_xp(i)+l_rp(i)
			if (tmp>l_maxedge) then
				l_maxedge = tmp
			end if
		end if
	end do
	r_minedge = 100
	do i = 1, r_np
		if (r_inp(i)>0) then
			tmp = r_xp(i)-r_rp(i)
			if (tmp<r_minedge) then
				r_minedge = tmp
			end if
		end if
	end do
	m_lx_actual = r_minedge-l_maxedge
	
	! middle part number of pores
	m_nx = 1
	m_ny = 10
	m_nz = 10
	m_yz_n = int(((l_np+r_np)/2.0)**(1.001/3))
	m_ny = m_yz_n
	m_nz = m_yz_n
	m_np = m_nx*m_ny*m_nz
	allocate(m_ip(1:m_np))
	allocate(m_xp(1:m_np))
	allocate(m_yp(1:m_np))
	allocate(m_zp(1:m_np))
	allocate(m_rp(1:m_np))
	allocate(m_gp(1:m_np))
	allocate(m_vp(1:m_np))
	allocate(m_vp_clay(1:m_np))
	allocate(m_connp(1:m_np))
	allocate(mo_connp(1:m_np))
	allocate(m_inp(1:m_np))
	allocate(m_outp(1:m_np))
	
	! middle part initial arrays
	do i = 1, m_np
		m_ip(i) = i
		m_connp(i)=0
	end do
	
	! middle part pore radii
	m_rmax = (maxval(l_rp)+maxval(r_rp))/2.0
	m_rmin = (minval(l_rp)+minval(r_rp))/2.0
    m_rdelta = m_rmax-m_rmin
    m_ravg = (m_rmin+m_rmax)/2.0
    call random_number(m_rp)
    do i = 1, m_np
		tmpa = m_ravg-0.3*m_rdelta
		tmpb = m_ravg+0.3*m_rdelta
		m_rp(i) = tmpa + m_rp(i)*(tmpb-tmpa)
    end do
    
	! x-locations of pores in the middle part
	call random_number(m_xp)
	do i = 1, m_np	
		tmpa0 = l_maxedge+m_rp(i)
		tmpb0 = r_minedge-m_rp(i)
		tmpa = tmpa0+0.3*(tmpb0-tmpa0)
		tmpb = tmpb0-0.3*(tmpb0-tmpa0)
		m_xp(i) = tmpa + m_xp(i)*(tmpb-tmpa)
	end do
	
	! y/z-locations for the middle part pore
	m_ydelta = m_ly/m_ny
	m_zdelta = m_lz/m_nz
	m_yz_delta = (m_ydelta+m_zdelta)/2.0
	call random_number(m_yp)
	m_yp = m_yp*m_ly
	call random_number(m_zp)
	m_zp = m_zp*m_lz
	do i = 1, m_ny
		do j = 1, m_nz
			k = j+(i-1)*m_nz
			m_yp(k) = (i-0.5)*m_ydelta
			m_zp(k) = (j-0.5)*m_zdelta
		end do
	end do
	
	!!!

end subroutine sub_make_layer
