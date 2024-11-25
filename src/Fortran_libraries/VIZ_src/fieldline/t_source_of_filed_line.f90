!>@file   t_source_of_filed_line.f90
!!@brief  module t_source_of_filed_line
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Structure of start point data for line tracing iteration
!!
!!@verbatim
!!      subroutine alloc_start_point_fline(num_pe, fln_prm, fln_src)
!!      subroutine alloc_init_tracer_position(fln_prm, fln_src)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!
!!      subroutine dealloc_start_point_fline(fln_src)
!!      subroutine dealloc_init_tracer_position(fln_prm)
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!@endverbatim
!
      module t_source_of_filed_line
!
      use m_precision
      use m_constants
      use t_control_params_4_fline
!
      implicit  none
!
!
      type each_fieldline_source
        integer(kind = kint) :: num_line_local = 0
        real(kind = kreal), allocatable :: xx4_initial_fline(:,:)
!
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!>        outward flux flag
        integer(kind = kint), allocatable                               &
     &                       :: iflag_outward_flux_fline(:)
!
        integer(kind = kint) :: n_points_prev = 0
!>        velocity of previous step
        real(kind = kreal), allocatable :: v_prev(:,:)
!
!>        Position list of seed point in start element
        real(kind = kreal), allocatable :: xi_surf_start_fline(:,:)
!>        domain list of seed point
        integer(kind = kint), allocatable :: ip_surf_start_fline(:)
!>        element list of seed point
        integer(kind = kint), allocatable :: iele_surf_start_fline(:)
      end type each_fieldline_source
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_velocity_at_previous(numnod, fln_src)
!
      integer(kind = kint), intent(in) :: numnod
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      fln_src%n_points_prev = numnod
      allocate(fln_src%v_prev(fln_src%n_points_prev,3))
      if(fln_src%n_points_prev .gt. 0) then
!$omp parallel workshare
        fln_src%v_prev = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_velocity_at_previous
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_velocity_at_previous(fln_src)
      type(each_fieldline_source), intent(inout) :: fln_src
!
      deallocate(fln_src%v_prev)
!
      end subroutine dealloc_velocity_at_previous
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_start_point_fline(num_pe, fln_prm, fln_src)
!
      integer, intent(in) :: num_pe
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: num
!
!
      allocate(fln_src%flux_stack_fline(0:num_pe))
      fln_src%flux_stack_fline = 0.0d0
!
      num = fln_prm%num_each_field_line
      allocate(fln_src%iflag_outward_flux_fline(num))
      allocate(fln_src%xx4_initial_fline(4,num))
!
      fln_src%iflag_outward_flux_fline(1:num) =   0
      fln_src%xx4_initial_fline = 0.0d0
!
      end subroutine alloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_init_tracer_position(fln_prm, fln_src)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: num
!
!
      num = fln_prm%num_each_field_line
      allocate(fln_src%xi_surf_start_fline(3,num))
      allocate(fln_src%ip_surf_start_fline(num))
      allocate(fln_src%iele_surf_start_fline(num))
!
      if(num .gt. 0) then
        fln_src%xi_surf_start_fline(1:3,1:num) = 0.0d0
        fln_src%ip_surf_start_fline(1:num) =        0
        fln_src%iele_surf_start_fline(1:num) =      0
      end if
!
      end subroutine alloc_init_tracer_position
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_start_point_fline(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      deallocate(fln_src%xx4_initial_fline)
      deallocate(fln_src%flux_stack_fline)
      deallocate(fln_src%iflag_outward_flux_fline)
!
      end subroutine dealloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_init_tracer_position(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      deallocate(fln_src%xi_surf_start_fline)
      deallocate(fln_src%ip_surf_start_fline)
      deallocate(fln_src%iele_surf_start_fline)
!
      end subroutine dealloc_init_tracer_position
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
