!start_surface_4_fline.f90
!
!      module start_surface_4_fline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_4_fline(node, ele, surf,             &
!!     &          fln_prm, fln_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module start_surface_4_fline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_4_fline(node, ele, surf,               &
     &          fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_int
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: iele, isf, isurf
      real(kind = kreal) :: vec_surf(3), xi(2)
!
!
      do i = 1, fln_src%num_line_local
        iele = fln_prm%id_surf_start_fline(1,i)
        isf =  fln_prm%id_surf_start_fline(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf))
        fln_src%xx4_initial_fline(1:3,i) = surf%x_surf(isurf,1:3)
        fln_src%xx4_initial_fline(4,i) =   0.0d0
        xi(1:2) = zero
        call cal_field_on_surf_vector                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf, xi, fln_src%vector_nod_fline, vec_surf)
!
        fln_src%flux_start_fline(i)                                     &
     &                     = (vec_surf(1) * surf%vnorm_surf(isurf,1)    &
     &                      + vec_surf(2) * surf%vnorm_surf(isurf,2)    &
     &                      + vec_surf(3) * surf%vnorm_surf(isurf,3))   &
     &                     * dble(surf%isf_4_ele(iele,isf) / isurf)
!
        if(fln_src%flux_start_fline(i) .gt. zero) then
          fln_prm%iflag_outward_flux_fline(i) = 1
          fln_src%flux_start_fline(i) = -fln_src%flux_start_fline(i)
        end if
      end do
!
      call calypso_mpi_allgather_one_int                                &
     &   (fln_src%num_line_local, fln_tce%num_current_fline)
!
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fln_tce%num_current_fline(1:nprocs)                             &
     &        = 2 * fln_tce%num_current_fline(1:nprocs)
      end if
!
      fln_tce%istack_current_fline(0) = 0
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &        = fln_tce%istack_current_fline(i-1)                       &
     &         + fln_tce%num_current_fline(i)
      end do
!
      call set_fline_start_surf(my_rank, node%numnod, ele%numele,       &
     &    surf%numsurf, surf%nnod_4_surf, surf%ie_surf,                 &
     &    surf%isf_4_ele, surf%iele_4_surf, fln_prm, fln_src, fln_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'num_current_fline',                        &
     &                   fln_tce%num_current_fline(:)
        write(50+my_rank,*) 'istack_current_fline',                     &
     &                   fln_tce%istack_current_fline(:)
!
        write(50+my_rank,*) 'num_line_local', fln_src%num_line_local
        do i = 1, fln_src%num_line_local
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                  fln_prm%id_surf_start_fline(1:2,i)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                  fln_src%xx4_initial_fline(1:3,i),               &
     &                  fln_src%flux_start_fline(i)
        end do
!
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        do inum = ist, ied
          write(50+my_rank,*) 'isf_fline_start', inum,                  &
     &                         fln_tce%isf_fline_start(1:3,inum)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point',               &
     &      fln_tce%xx_fline_start(1:4,inum)
        end do
      end if
!
      end subroutine s_start_surface_4_fline
!
!  ---------------------------------------------------------------------
!
      end module start_surface_4_fline
