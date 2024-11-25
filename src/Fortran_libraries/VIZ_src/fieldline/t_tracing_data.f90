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
!!      subroutine alloc_line_start_fline(num_each_field_line,          &
!!     &                                  viz_fields, fln_tce)
!!      subroutine resize_line_start_fline(num_each_field_line,         &
!!     &                                   viz_fields, fln_tce)
!!        integer, intent(in) :: num_pe
!!        integer(kind = kint), intent(in) :: num_each_field_line
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine copy_global_start_fline(i_copied, i_org,             &
!!     &                                   viz_fields, fln_tce)
!!        integer(kind = kint), intent(in) :: i_copied, i_org
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine dealloc_line_start_fline(fln_tce)
!!      subroutine dealloc_num_gl_start_fline(fln_tce)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module t_tracing_data
!
      use m_precision
      use m_constants
      use t_control_params_4_fline
!
      implicit  none
!
      type each_fieldline_trace
        integer(kind = kint) :: num_current_fline
        integer(kind = kint), allocatable :: istack_current_fline(:)
!
        integer(kind = kint) :: num_trace_buf
        integer(kind= kint_gl), allocatable :: iline_original(:)
        integer(kind= kint), allocatable :: iflag_direction(:)
        integer(kind= kint), allocatable :: icount_fline(:)
        integer(kind= kint), allocatable :: iflag_comm_start(:)
        integer(kind= kint), allocatable :: isf_dbl_start(:,:)
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
        real(kind = kreal), allocatable ::  v_fline_start(:,:)
        real(kind = kreal), allocatable ::  c_fline_start(:,:)
        real(kind = kreal), allocatable ::  trace_length(:)
      end type each_fieldline_trace
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
      fln_tce%istack_current_fline = 0
      fln_tce%num_current_fline =    0
!
      call alloc_line_start_fline(ione, viz_fields, fln_tce)
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_line_start_fline(num_each_field_line,            &
     &                                  viz_fields, fln_tce)
!
      integer(kind = kint), intent(in) :: num_each_field_line
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: num, i
!
!
      fln_tce%num_trace_buf = 2 * num_each_field_line
      allocate(fln_tce%iline_original(fln_tce%num_trace_buf))
      allocate(fln_tce%iflag_direction(fln_tce%num_trace_buf))
      allocate(fln_tce%iflag_comm_start(fln_tce%num_trace_buf))
      allocate(fln_tce%icount_fline(fln_tce%num_trace_buf))
      allocate(fln_tce%isf_dbl_start(3,fln_tce%num_trace_buf))
!
      do i = 1, fln_tce%num_trace_buf
        fln_tce%iline_original(i) = i
      end do
!
      num = viz_fields%ntot_color_comp
      allocate(fln_tce%xx_fline_start(4,fln_tce%num_trace_buf))
      allocate(fln_tce%v_fline_start(4,fln_tce%num_trace_buf))
      allocate(fln_tce%c_fline_start(num, fln_tce%num_trace_buf))
      allocate(fln_tce%trace_length(fln_tce%num_trace_buf))
!
!$omp parallel workshare
      fln_tce%iflag_direction =  0
      fln_tce%iflag_comm_start =  0
      fln_tce%icount_fline = 0
      fln_tce%isf_dbl_start = 0
      fln_tce%v_fline_start =  0.0d0
      fln_tce%c_fline_start =  0.0d0
      fln_tce%xx_fline_start = 0.0d0
      fln_tce%trace_length = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_line_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine resize_line_start_fline(num_each_field_line,           &
     &                                   viz_fields, fln_tce)
      integer(kind = kint), intent(in) :: num_each_field_line
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      if(num_each_field_line .le. fln_tce%num_trace_buf) return
!      write(*,*) 'change local number of lines for',num_each_field_line
      call dealloc_line_start_fline(fln_tce)
      call alloc_line_start_fline(num_each_field_line,                  &
     &                            viz_fields, fln_tce)
!
      end subroutine resize_line_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_global_start_fline(i_copied, i_org,               &
     &                                   viz_fields, fln_tce)
!
      use t_ctl_params_viz_fields
!
      integer(kind = kint), intent(in) :: i_copied, i_org
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
          fln_tce%xx_fline_start(1:4,i_copied)                          &
     &          = fln_tce%xx_fline_start(1:4,i_org)
          fln_tce%v_fline_start(1:4,i_copied)                           &
     &          = fln_tce%v_fline_start(1:4,i_org)
          fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,i_copied)  &
     &      = fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,i_org)
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
      deallocate(fln_tce%v_fline_start)
      deallocate(fln_tce%c_fline_start)
      deallocate(fln_tce%trace_length)
!
      end subroutine dealloc_line_start_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline(fln_tce)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      call dealloc_line_start_fline(fln_tce)

      deallocate(fln_tce%istack_current_fline)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      end module t_tracing_data
