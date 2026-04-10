!>@file   start_surface_4_fline.f90
!!@brief  module start_surface_4_fline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Set start point list for field line
!!
!!@verbatim
!!      subroutine s_start_surface_4_fline(node, surf, nod_fld,         &
!!     &          isf_4_ele_dbl, num_line_local, fln_prm, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_line_local
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
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
      use t_tracing_data
!
      implicit  none
!
        real(kind = kreal), private, allocatable                        &
     &                       :: xx4_initial_fline(:,:)
!>        outward flux flag
        integer(kind = kint), private, allocatable                      &
     &                       :: iflag_outward_flux_fline(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_4_fline(node, ele, surf, nod_fld,      &
     &          isf_4_ele_dbl, num_line_local, fln_prm, fln_tce)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
      use cal_field_on_surf_viz
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: num_line_local
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ist, ied, inum, num
      integer(kind = kint) :: num_fline
!
!
      num = fln_prm%num_each_field_line
      allocate(iflag_outward_flux_fline(num))
      allocate(xx4_initial_fline(4,num))
!
      iflag_outward_flux_fline(1:num) = 0
      xx4_initial_fline(1:4,1:num) = 0.0d0
!
      num_fline = count_fline_start_surf                                &
     &                               (node, ele, surf, isf_4_ele_dbl,   &
     &                                nod_fld, fln_prm, num_line_local, &
     &                                iflag_outward_flux_fline,         &
     &                                xx4_initial_fline)
      call count_parallel_current_fline(num_fline, fln_tce)
      call resize_line_start_fline(fln_tce%num_current_fline, fln_tce)
!
      call set_fline_start_surf(node, ele, surf, isf_4_ele_dbl,         &
     &                        nod_fld, fln_prm, num_line_local,         &
     &                        iflag_outward_flux_fline, fln_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        do i = 1, num_line_local
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                  xx4_initial_fline(1:3,i)
        end do
      end if
!
      deallocate(xx4_initial_fline)
      deallocate(iflag_outward_flux_fline)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'num_current_fline',                        &
     &                   fln_tce%num_current_fline
        write(50+my_rank,*) 'istack_smp_cur_fline',                     &
     &                   fln_tce%istack_smp_cur_fline(:)
        write(50+my_rank,*) 'istack_current_fline',                     &
     &                   fln_tce%istack_current_fline(:)
!
        write(50+my_rank,*) 'num_line_local', num_line_local
        do i = 1, num_line_local
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                  fln_prm%id_surf_start_fline(1:2,i)
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
