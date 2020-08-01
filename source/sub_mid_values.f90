subroutine sub_mid_values
	use sub_variables
	
	!!!
	
	! in/outlet status of pores
	do i = 1, m_np
		m_inp(i) = 0
		m_outp(i) = 0
	end do
	! throat radius
	m_rmax = (maxval(l_rt)+maxval(r_rt))/2.0
	m_rmin = (minval(l_rt)+minval(r_rt))/2.0
    m_rdelta = m_rmax-m_rmin
    m_ravg = (m_rmin+m_rmax)/2.0
    call random_number(m_rt)
    do i = 1, m_nt
		tmpa = m_ravg-0.4*m_rdelta
		tmpb = m_ravg+0.3*m_rdelta
		m_rt(i) = tmpa + m_rt(i)*(tmpb-tmpa)
    end do
	! pore shape factor
	tmpa = 0.001
	tmpb = 0.0481
	call uniform_gen(m_gp,m_np,tmpa,tmpb)
	! throat shape factor
	tmpa = 0.001
	tmpb = 0.0481
	call uniform_gen(m_gt,m_nt,tmpa,tmpb)
	! pore volume
	do i = 1, m_np
		m_vp(i) = (4.0/3.0)*3.14159*(m_rp(i)**3)
	end do
	! throat volume
	do i = 1, m_np
		m_vt(i) = (4.0/3.0)*3.14159*(m_rt(i)**3)
	end do
	! pore clay volume
	tmpa = (minval(l_vp_clay)+minval(r_vp_clay))/2.0
	tmpb = (maxval(l_vp_clay)+maxval(r_vp_clay))/2.0
	tmpb = 1.0*tmpb
	call uniform_gen(m_vp_clay,m_np,tmpa,tmpb)
	! throat clay volume
	tmpa = (minval(l_vt_clay)+minval(r_vt_clay))/2.0
	tmpb = (maxval(l_vt_clay)+maxval(r_vt_clay))/2.0
	tmpb = 1.0*tmpb
	call uniform_gen(m_vt_clay,m_nt,tmpa,tmpb)
	
	!!!

end subroutine sub_mid_values
