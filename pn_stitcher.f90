include 'sub_variables.f90'
include 'sub_functions.f90'
include 'sub_remove_outlet.f90'
include 'sub_remove_inlet.f90'
include 'sub_write_network.f90'
include 'sub_make_layer.f90'
include 'sub_final_allocations.f90'
include 'sub_count_connections.f90'
include 'sub_connecting_process.f90'
include 'sub_mid_values.f90'
include 'sub_neighbor_pores.f90'
include 'sub_final_attributions.f90'
include 'sub_deallocate.f90'

program stitcher
	
	
	use sub_variables
	implicit none
	real :: std, avg, dist ! used fortran functions
	
	open(5,file='input.txt',status='old')
	read(5,*)
	read(5,*) l_images_name
	read(5,*) r_images_name
	read(5,*) f_images_name
	read(5,*)
	read(5,*) l_address
	read(5,*) r_address
	read(5,*) f_address
	close(5)
	
	call sub_remove_outlet()
	call sub_remove_inlet()
	call sub_make_layer()
	
	write(*,*)
	write(*,*) "thickness, fractional rule      :", m_lx_frac
	write(*,*) "thickness, sum of avg Lt's      :", (l_lt_avg+r_lt_avg)
	write(*,*) "thickness, sum of avg Lt_ij's   :", (l_lt_ij_avg+r_lt_ij_avg)
	write(*,*) "thickness, final                :", m_lx
	write(*,*)
	write(*,*) "diameter, max of middle pores   :", 2*maxval(m_rp)
	write(*,*) "distance, side edges distance   :", m_lx_actual
	
	call sub_count_connections()
	
	write(*,*)
	write(*,*) "threshold of Lt toward layer    :", lt_threshold
	write(*,*) "threshold of Lt     in layer    :", lt_threshold_m
	write(*,*) "grid size in y in the middle    :", m_ydelta
	write(*,*) "grid size in z in the middle    :", m_zdelta
	write(*,*)
	write(*,*) "number of generated left   connections  :", count_l
	write(*,*) "number of generated middle connections  :", count_m
	write(*,*) "number of generated right  connections  :", count_r
	
	call sub_final_allocations()
	call sub_connecting_process()
	call sub_mid_values()
	
	write(*,*) 
	write(*,*) "elements in   left network (p/t):", l_np, l_nt
	write(*,*) "elements in middle network (p/t):", m_np, m_nt
	write(*,*) "elements in  right network (p/t):", r_np, r_nt
	write(*,*) "elements in  final network (p/t):", f_np, f_nt
	
	call sub_neighbor_pores()
	call sub_final_attributions()
	call sub_write_network()
	call sub_deallocate()
	
end program stitcher
