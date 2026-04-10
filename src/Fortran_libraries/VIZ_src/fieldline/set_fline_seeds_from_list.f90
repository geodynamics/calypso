!>@file   set_fline_seeds_from_list.f90
!!@brief  module set_fline_seeds_from_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine const_FLINE_seed_from_list(fln_prm, fln_src, fln_tce)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine set_veclocity_at_each_tracer(node, ele, nod_fld,     &
!!     &          iphys_4_fline, iele_seed, xx4_seed,                   &
!!     &          xi4_fline_start, v_fline_start, itp_ele_work)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: iphys_4_fline
!!        integer(kind = kint), intent(in) :: iele_seed(1)
!!        real(kind = kreal), intent(inout) :: xi4_fline_start(4)
!!        real(kind = kreal), intent(in) :: xx4_seed(4)
!!        real(kind = kreal), intent(inout) :: v_fline_start(4)
!!        type(cal_interpolate_coefs_work), intent(inout)               &
!!     &                                   :: itp_ele_work
!!
!!      subroutine find_seed_point_in_each_ele                          &
!!     &         (node, ele, xx_surf_start_fline, idx_fln_dist,         &
!!     &          num_search, itp_ele_work, ip_surf_start_fline,        &
!!     &          iele_surf_start_fline, xi_surf_start_fline,           &
!!     &          ierr_inter)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!!        integer(kind = kint), intent(in) :: idx_fln_dist(ele%numele)
!!        integer(kind = kint), intent(in) :: num_search
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!        integer(kind = kint), intent(inout) :: ip_surf_start_fline
!!        integer(kind = kint), intent(inout) :: iele_surf_start_fline
!!        real(kind = kreal), intent(inout) :: xi_surf_start_fline(3)
!!        integer(kind = kint), intent(inout) :: ierr_inter
!!@endverbatim
!
      module set_fline_seeds_from_list
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: maxitr = 20
      real(kind = kreal), parameter, private ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter, private :: iflag_nomessage = 0
      real(kind = kreal), parameter, private ::   error_level = 1.0d-9
!
      private :: count_FLINE_seed_from_list
      private :: set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_FLINE_seed_from_list(fln_prm, fln_src, fln_tce)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      call count_FLINE_seed_from_list(fln_src%num_line_local,           &
     &                                fln_prm, fln_tce)
      call set_FLINE_seed_field_from_list(fln_prm, fln_src, fln_tce)
!
      end subroutine const_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine count_FLINE_seed_from_list(num_line_local,             &
     &                                      fln_prm, fln_tce)
!
      integer(kind = kint), intent(in) :: num_line_local
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: num_fline
!
!
      num_fline = num_line_local
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        num_fline = 2 * num_fline
      end if
      call count_parallel_current_fline(num_fline, fln_tce)
      call resize_line_start_fline(fln_tce%num_current_fline, fln_tce)
!
      end subroutine count_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_field_from_list(fln_prm, fln_src,       &
     &                                          fln_tce)
!
      use sel_interpolate_scalar
      use trace_in_element
      use tracer_field_interpolate
      use field_at_each_seed_point
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: icou, inum
!
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
          if(fln_src%ip_surf_start_fline(inum) .ne. my_rank) cycle
          icou = icou + 1
!
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou)                                 &
     &            = fln_src%iele_surf_start_fline(inum)
          fln_tce%isf_dbl_start(3,icou)                                 &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
          fln_tce%iline_original(icou) = inum
          fln_tce%xx_fline_start(1:3,icou)                              &
     &         = fln_prm%xx_surf_start_fline(1:3,inum)
          fln_tce%xx_fline_start(4,icou) = one
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          
          if     (fln_prm%id_fline_direction                            &
     &                  .eq. iflag_forward_trace) then
           fln_tce%iflag_direction(icou) = 1
          else if(fln_prm%id_fline_direction                            &
     &                  .eq. iflag_backward_trace) then

            fln_tce%iflag_direction(icou) = -1
          else
            fln_tce%iflag_direction(icou) = 1
!
            icou = icou + 1
            fln_tce%iflag_direction(icou) = -1
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou)                               &
     &            = fln_src%iele_surf_start_fline(inum)
            fln_tce%isf_dbl_start(3,icou)                               &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
            fln_tce%trace_length(icou) = 0.0d0
            fln_tce%icount_fline(icou) = 0
            call copy_global_start_fline(icou, (icou-1), fln_tce)

          end if
        end do
!
      end subroutine set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_veclocity_at_each_tracer(node, ele, nod_fld,       &
     &          iphys_4_fline, iele_seed, xx4_seed,                     &
     &          xi4_fline_start, v_fline_start, itp_ele_work)
!
      use t_find_interpolate_in_ele
      use field_at_each_seed_point
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: iphys_4_fline
!
      integer(kind = kint), intent(in) :: iele_seed(1)
      real(kind = kreal), intent(in) :: xx4_seed(4)
!
      real(kind = kreal), intent(inout) :: xi4_fline_start(4)
      real(kind = kreal), intent(inout) :: v_fline_start(4)
      type(cal_interpolate_coefs_work), intent(inout)                   &
     &                                  :: itp_ele_work
!
      integer(kind = kint) :: ierr_inter
!
!
      xi4_fline_start(1:3) = -2.0
      call find_interpolate_in_ele(xx4_seed, maxitr, eps_iter,          &
     &    my_rank, iflag_nomessage, error_level, node, ele,             &
     &    iele_seed(1), itp_ele_work, xi4_fline_start(1), ierr_inter)
      call cal_each_seed_velocity_in_ele(ele,                           &
     &    nod_fld%n_point, nod_fld%d_fld(1,iphys_4_fline),              &
     &    iele_seed, xi4_fline_start(1), v_fline_start)
!
      end subroutine set_veclocity_at_each_tracer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine find_seed_point_in_each_ele                            &
     &         (node, ele, xx_surf_start_fline, idx_fln_dist,           &
     &          num_search, itp_ele_work, ip_surf_start_fline,          &
     &          iele_surf_start_fline, xi_surf_start_fline,             &
     &          ierr_inter)
!
      use t_find_interpolate_in_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
      integer(kind = kint), intent(in) :: idx_fln_dist(ele%numele)
      integer(kind = kint), intent(in) :: num_search
!
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
      integer(kind = kint), intent(inout) :: ip_surf_start_fline
      integer(kind = kint), intent(inout) :: iele_surf_start_fline
      real(kind = kreal), intent(inout) :: xi_surf_start_fline(3)
      integer(kind = kint), intent(inout) :: ierr_inter
!
      real(kind = kreal) :: xi(3)
      integer(kind = kint) :: iele, inum
!
!
      ip_surf_start_fline = -1
      iele_surf_start_fline = 0
      xi_surf_start_fline(1:3) = -2.0
      do inum = 1, num_search
        iele = idx_fln_dist(inum)
        if(ele%interior_ele(iele) .le. 0) cycle
!
        ierr_inter = 0
        xi(1:3) = -2.0
        call find_interpolate_in_ele                                    &
     &     (xx_surf_start_fline(1), maxitr, eps_iter,                   &
     &      my_rank, iflag_nomessage, error_level,                      &
     &      node, ele, iele, itp_ele_work, xi, ierr_inter)
        if(ierr_inter.gt.1 .and. ierr_inter.le.maxitr) then
          ip_surf_start_fline =      my_rank
          iele_surf_start_fline =    iele
          xi_surf_start_fline(1:3) = xi(1:3)
          exit
        end if
      end do
!
      end subroutine find_seed_point_in_each_ele
!
!  ---------------------------------------------------------------------
!
      end module set_fline_seeds_from_list
