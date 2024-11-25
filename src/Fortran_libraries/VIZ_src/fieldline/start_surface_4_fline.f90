!start_surface_4_fline.f90
!
!      module start_surface_4_fline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_4_fline(node, surf, nod_fld,         &
!!     &          fln_prm, fln_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
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
      use t_tracing_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_4_fline(node, ele, surf, nod_fld,      &
     &          isf_4_ele_dbl, fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_int
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
      use cal_field_on_surf_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ist, ied, inum
!
!
      fln_tce%num_current_fline                                         &
     &        = count_fline_start_surf(node, ele, surf, isf_4_ele_dbl,  &
     &                                nod_fld, fln_prm, fln_src)
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int                                &
     &   (fln_tce%num_current_fline, fln_tce%istack_current_fline(1))
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &        = fln_tce%istack_current_fline(i-1)                       &
     &         + fln_tce%istack_current_fline(i)
      end do
!
      call set_fline_start_surf(node, ele, surf, isf_4_ele_dbl,         &
     &                          nod_fld, fln_prm, fln_src, fln_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'num_current_fline',                        &
     &                   fln_tce%num_current_fline
        write(50+my_rank,*) 'istack_current_fline',                     &
     &                   fln_tce%istack_current_fline(:)
!
        write(50+my_rank,*) 'num_line_local', fln_src%num_line_local
        do i = 1, fln_src%num_line_local
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                  fln_prm%id_surf_start_fline(1:2,i)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                  fln_src%xx4_initial_fline(1:3,i)
        end do
!
!
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        do inum = ist, ied
          write(50+my_rank,*) 'isf_dbl_start', inum,                    &
     &                         fln_tce%isf_dbl_start(1:3,inum)
          write(50+my_rank,'(a,i16, 1p4e16.5)') 'start_point',          &
     &      fln_tce%iline_original(inum),                               &
     &      fln_tce%xx_fline_start(1:4,inum)
        end do
      end if
!
      end subroutine s_start_surface_4_fline
!
!  ---------------------------------------------------------------------
!
      end module start_surface_4_fline
