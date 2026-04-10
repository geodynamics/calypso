!>@file   t_source_of_filed_line.f90
!!@brief  module t_source_of_filed_line
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Structure of start point data for line tracing iteration
!!
!!@verbatim
!!      subroutine alloc_init_tracer_position(fln_prm, fln_src)
!!      subroutine dealloc_init_tracer_position(fln_src)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!
!!      subroutine alloc_FLINE_element_size(ele, fln_dist)
!!      subroutine dealloc_FLINE_element_size(fln_dist)
!!        type(element_data), intent(in) :: ele
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!
!!      subroutine check_each_fieldline_source(i_fln, numele, fln_src)
!!        integer(kind = kint), intent(in) :: i_fln, numele
!!        type(each_fieldline_source), intent(in) :: fln_src
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
!
        integer(kind = kint) :: num_line_global = 0
!>        Position list of seed point in start element
        real(kind = kreal), allocatable :: xi_surf_start_fline(:,:)
!>        domain list of seed point
        integer(kind = kint), allocatable :: ip_surf_start_fline(:)
!>        element list of seed point
        integer(kind = kint), allocatable :: iele_surf_start_fline(:)
      end type each_fieldline_source
!
      type FLINE_element_size
        integer(kind = kint) :: numele_dist
        real(kind = kreal), allocatable :: ele_size(:)
!
        real(kind = kreal), allocatable :: distance(:)
        integer(kind = kint), allocatable :: index(:)
      end type FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      contains
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
      fln_src%num_line_global = fln_prm%num_each_field_line
      allocate(fln_src%xi_surf_start_fline(3,fln_src%num_line_global))
      allocate(fln_src%ip_surf_start_fline(fln_src%num_line_global))
      allocate(fln_src%iele_surf_start_fline(fln_src%num_line_global))
!
      num = fln_src%num_line_global
      if(num .gt. 0) then
        fln_src%xi_surf_start_fline(1:3,1:num) = 0.0d0
        fln_src%ip_surf_start_fline(1:num) =        0
        fln_src%iele_surf_start_fline(1:num) =      0
      end if
!
      end subroutine alloc_init_tracer_position
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_FLINE_element_size(ele, fln_dist)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(FLINE_element_size), intent(inout) :: fln_dist
!
!
      fln_dist%numele_dist = ele%numele
      allocate(fln_dist%ele_size(fln_dist%numele_dist))
      allocate(fln_dist%distance(fln_dist%numele_dist))
      allocate(fln_dist%index(fln_dist%numele_dist))
!
      if(fln_dist%numele_dist .le. 0) return
!$omp parallel workshare
      fln_dist%ele_size(1:fln_dist%numele_dist) = 0.0d0
      fln_dist%distance(1:fln_dist%numele_dist) = 0.0d0
      fln_dist%index(1:fln_dist%numele_dist) =    0
!$omp end parallel workshare 
!
      end subroutine alloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
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
      subroutine dealloc_FLINE_element_size(fln_dist)
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      if(allocated(fln_dist%ele_size) .eqv. .FALSE.) return
      deallocate(fln_dist%ele_size)
      deallocate(fln_dist%distance, fln_dist%index)
!
      end subroutine dealloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_each_fieldline_source(i_fln, numele, fln_src)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_fln, numele
      type(each_fieldline_source), intent(in) :: fln_src
!
      integer(kind = kint) :: i, ip
!
!
      do ip = 1, nprocs
        call calypso_mpi_barrier
        if(my_rank .ne. ip-1) cycle
        write(*,*) my_rank, i_fln, 'fln_src%num_line_global',           &
    &             fln_src%num_line_global
        do i = 1, fln_src%num_line_global
          if(fln_src%ip_surf_start_fline(i) .ge. 0) then
          write(*,*) i, 'xi_surf_start_fline',                          &
     &              fln_src%ip_surf_start_fline(i),                     &
     &              fln_src%iele_surf_start_fline(i),                   &
     &              fln_src%xi_surf_start_fline(1:3,i),                 &
     &              numele
          end if
        end do
      end do
      call calypso_mpi_barrier()
!
      end subroutine check_each_fieldline_source
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
