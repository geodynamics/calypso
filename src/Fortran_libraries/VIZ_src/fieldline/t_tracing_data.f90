!>@file   t_tracing_data.f90
!!@brief  module t_tracing_data
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Structure of start point data for line tracing iteration
!!
!!@verbatim
!!      subroutine alloc_num_gl_start_fline(num_pe, viz_fields, fln_tce)
!!      subroutine resize_line_start_fline(num_each_field_line, fln_tce)
!!        integer, intent(in) :: num_pe
!!        integer(kind = kint), intent(in) :: num_each_field_line
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine copy_global_start_fline(i_copied, i_org, fln_tce)
!!        integer(kind = kint), intent(in) :: i_copied, i_org
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine dealloc_line_start_fline(fln_tce)
!!      subroutine dealloc_num_gl_start_fline(fln_tce)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine alloc_velocity_at_previous(numnod, fln_tce)
!!      subroutine dealloc_velocity_at_previous(fln_tce)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine count_parallel_current_fline(num_fline, fln_tce)
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module t_tracing_data
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use t_control_params_4_fline
!
      implicit  none
!
      type each_fieldline_trace
!>  Number of trace in each PE
        integer(kind = kint) :: num_current_fline
!>  Stack of number of trace in each PE
        integer(kind = kint), allocatable :: istack_current_fline(:)
!>  Stack of number of trace for SMP parallelization
        integer(kind = kint), allocatable :: istack_smp_cur_fline(:)
!
!>  Size of array for trace in each PE
        integer(kind = kint) :: num_trace_buf
!>  Global trace ID
        integer(kind= kint_gl), allocatable :: iline_original(:)
!>  Trace direction flag
        integer(kind= kint), allocatable :: iflag_direction(:)
!>  Counts of line trace
        integer(kind= kint), allocatable :: icount_fline(:)
!>  Trace communication flag
        integer(kind= kint), allocatable :: iflag_comm_start(:)
!>  Double numbering for trace element ID for starting point
!!         isf_dbl_start(1,:):: Belonged domain ID
!!         isf_dbl_start(2,:):: Belonged local element ID
!!         isf_dbl_start(3,:):: Belonged surface ID in each element
        integer(kind= kint), allocatable :: isf_dbl_start(:,:)
!>  Position of starting point
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
!>  position of starting point in each element coordinate
        real(kind = kreal), allocatable ::  xi_fline_start(:,:)
!>  Field data at starting point
        real(kind = kreal), allocatable ::  c_fline_start(:,:)
!>  Trace length for each tracing
        real(kind = kreal), allocatable ::  trace_length(:)
!
!>  Size of array for previous velocity in each PE
        integer(kind = kint) :: n_points_prev = 0
!>  Velocity at previous step
        real(kind = kreal), allocatable :: v_prev(:,:)
      end type each_fieldline_trace
!
      private :: alloc_line_start_fline, dealloc_line_field_fline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_gl_start_fline(num_pe, viz_fields, fln_tce)
!
      integer, intent(in) :: num_pe
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      allocate(fln_tce%istack_current_fline(0:num_pe))
      allocate(fln_tce%istack_smp_cur_fline(0:np_smp))
      fln_tce%istack_current_fline(0:num_pe) = 0
      fln_tce%istack_smp_cur_fline(0:np_smp) = 0
      fln_tce%num_current_fline =    0
!
      call alloc_line_start_fline(ione, fln_tce)
      call alloc_line_field_fline(viz_fields, fln_tce)
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_line_start_fline(num_each_field_line, fln_tce)
!
      integer(kind = kint), intent(in) :: num_each_field_line
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i
!
!
      fln_tce%num_trace_buf = 2 * num_each_field_line
      allocate(fln_tce%iline_original(fln_tce%num_trace_buf))
      allocate(fln_tce%iflag_direction(fln_tce%num_trace_buf))
      allocate(fln_tce%iflag_comm_start(fln_tce%num_trace_buf))
      allocate(fln_tce%icount_fline(fln_tce%num_trace_buf))
      allocate(fln_tce%isf_dbl_start(3,fln_tce%num_trace_buf))
!
!$omp parallel
!$omp workshare
      fln_tce%iflag_direction(1:fln_tce%num_trace_buf) =   0
      fln_tce%iflag_comm_start(1:fln_tce%num_trace_buf) =  0
      fln_tce%icount_fline(1:fln_tce%num_trace_buf) =      0
!$omp end workshare nowait
!$omp workshare
      fln_tce%isf_dbl_start(1:3,1:fln_tce%num_trace_buf) = 0
!$omp end workshare nowait
!
!$omp do
      do i = 1, fln_tce%num_trace_buf
        fln_tce%iline_original(i) = i
      end do
!$omp end do
!$omp end parallel
!
      allocate(fln_tce%xx_fline_start(4,fln_tce%num_trace_buf))
      allocate(fln_tce%xi_fline_start(4,fln_tce%num_trace_buf))
      allocate(fln_tce%trace_length(fln_tce%num_trace_buf))
!
!$omp parallel
!$omp workshare
      fln_tce%xx_fline_start(1:4,1:fln_tce%num_trace_buf) = 0.0d0
      fln_tce%xi_fline_start(1:4,1:fln_tce%num_trace_buf) = 0.0d0
!$omp end workshare nowait
!$omp workshare
      fln_tce%trace_length(1:fln_tce%num_trace_buf) = 0.0d0
!$omp end workshare
!$omp end parallel
!
      end subroutine alloc_line_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_line_field_fline(viz_fields, fln_tce)
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: num
!
      num = viz_fields%ntot_color_comp
      allocate(fln_tce%c_fline_start(num,np_smp))
!$omp parallel workshare
      fln_tce%c_fline_start(1:num,1:np_smp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_line_field_fline
!
!  ---------------------------------------------------------------------
!
      subroutine resize_line_start_fline(num_each_field_line, fln_tce)
      integer(kind = kint), intent(in) :: num_each_field_line
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      if(num_each_field_line .le. fln_tce%num_trace_buf) return
!      write(*,*) 'change local number of lines for',num_each_field_line
      call dealloc_line_start_fline(fln_tce)
      call alloc_line_start_fline(num_each_field_line, fln_tce)
!
      end subroutine resize_line_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_global_start_fline(i_copied, i_org, fln_tce)
!
      use t_ctl_params_viz_fields
!
      integer(kind = kint), intent(in) :: i_copied, i_org
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      fln_tce%xx_fline_start(1:4,i_copied)                              &
     &          = fln_tce%xx_fline_start(1:4,i_org)
      fln_tce%xi_fline_start(1:4,i_copied)                              &
     &          = fln_tce%xi_fline_start(1:4,i_org)
!
      end subroutine copy_global_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_line_start_fline(fln_tce)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      deallocate(fln_tce%iline_original)
      deallocate(fln_tce%iflag_direction)
      deallocate(fln_tce%iflag_comm_start)
      deallocate(fln_tce%icount_fline)
      deallocate(fln_tce%isf_dbl_start)
      deallocate(fln_tce%xx_fline_start)
      deallocate(fln_tce%xi_fline_start)
      deallocate(fln_tce%trace_length)
!
      end subroutine dealloc_line_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_line_field_fline(fln_tce)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      deallocate(fln_tce%c_fline_start)
!
      end subroutine dealloc_line_field_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline(fln_tce)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      call dealloc_line_start_fline(fln_tce)
      call dealloc_line_field_fline(fln_tce)
!
      deallocate(fln_tce%istack_current_fline)
      deallocate(fln_tce%istack_smp_cur_fline)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_velocity_at_previous(numnod, fln_tce)
!
      integer(kind = kint), intent(in) :: numnod
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      fln_tce%n_points_prev = numnod
      allocate(fln_tce%v_prev(fln_tce%n_points_prev,3))
      if(fln_tce%n_points_prev .gt. 0) then
!$omp parallel workshare
        fln_tce%v_prev(1:fln_tce%n_points_prev,1:3) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_velocity_at_previous
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_velocity_at_previous(fln_tce)
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      deallocate(fln_tce%v_prev)
!
      end subroutine dealloc_velocity_at_previous
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_parallel_current_fline(num_fline, fln_tce)
!
      use calypso_mpi
      use calypso_mpi_int
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_fline
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, nmax
!
      fln_tce%num_current_fline = num_fline
      call count_number_4_smp(np_smp, ione, fln_tce%num_current_fline,  &
                              fln_tce%istack_smp_cur_fline, nmax)
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
      fln_tce%istack_current_fline(0) = 0
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &     = fln_tce%istack_current_fline(i-1)                          &
     &      + fln_tce%istack_current_fline(i)
      end do
!
      end subroutine count_parallel_current_fline
!
!  ---------------------------------------------------------------------
!
      end module t_tracing_data
