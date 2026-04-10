!>@file   add_tracer_fieldline_list.f90
!!@brief  module add_tracer_fieldline_list
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  local field line and tracer data structure
!!
!!@verbatim
!!      subroutine add_fline_start(iglobal_add, iele_add,               &
!!     &                           xx4_add, xi4_add, fline_lc)
!!      subroutine add_fline_list(iglobal_add, iele_add,                &
!!     &                          xx4_add, xi4_add, fline_lc)
!!        integer(kind = kint_gl), intent(in) :: iglobal_add
!!        integer(kind = kint), intent(in) :: iele_add
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: xx4_add(4), xi4_add(4)
!!        real(kind = kreal), intent(in) :: col_add(ntot_comp)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine add_traced_list(iglobal_tracer, isf_dbl_start,       &
!!     &                           xx4_add, fline_lc)
!!        integer(kind = kint_gl), intent(in) :: iglobal_tracer
!!        integer(kind = kint), intent(in) :: isf_dbl_start(3)
!!        real(kind = kreal), intent(in) :: xx4_add(4)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine return_to_trace_list(fline_lc, fln_tce)
!!        integer, intent(in) :: my_rank
!!        type(local_fieldline), intent(in) :: fline_lc(np_smp)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module add_tracer_fieldline_list
!
      use m_precision
      use m_constants
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_start(iglobal_add, iele_add,                 &
     &                           xx4_add, xi4_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_add
      integer(kind = kint), intent(in) :: iele_add
      real(kind = kreal), intent(in) :: xx4_add(4), xi4_add(4)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_add
      fline_lc%iele_fline(fline_lc%nnod_line_l) =    iele_add
      fline_lc%xx_line_l(1:4,fline_lc%nnod_line_l) = xx4_add(1:4)
      fline_lc%xi_line_l(1:4,fline_lc%nnod_line_l) = xi4_add(1:4)
!
      end subroutine add_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_list(iglobal_add, iele_add,                  &
     &                          xx4_add, xi4_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_add
      integer(kind = kint), intent(in) :: iele_add
      real(kind = kreal), intent(in) :: xx4_add(4), xi4_add(4)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l - 1
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_add
      fline_lc%iele_fline(fline_lc%nnod_line_l) =    iele_add
      fline_lc%xx_line_l(1:4,fline_lc%nnod_line_l) = xx4_add(1:4)
      fline_lc%xi_line_l(1:4,fline_lc%nnod_line_l) = xi4_add(1:4)
!
      end subroutine add_fline_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_traced_list(iglobal_tracer, isf_dbl_start,         &
     &                           xx4_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_tracer
      integer(kind = kint), intent(in) :: isf_dbl_start(3)
      real(kind = kreal), intent(in) :: xx4_add(4)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l) = isf_dbl_start(2)
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l) = isf_dbl_start(3)
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_tracer
      fline_lc%xx_line_l(1:4,fline_lc%nnod_line_l) =   xx4_add(1:4)
!
      end subroutine add_traced_list
!
!  ---------------------------------------------------------------------
!
      subroutine return_to_trace_list(my_rank, fline_lc, fln_tce)
!
      use t_tracing_data
!
      integer, intent(in) :: my_rank
      type(local_fieldline), intent(in) :: fline_lc(np_smp)
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint), allocatable :: istack_line_nod_smp(:)
      integer(kind = kint) :: i, ip, ist
!
!
      allocate(istack_line_nod_smp(0:np_smp))
      istack_line_nod_smp(0) =  0
      do ip = 1, np_smp
        istack_line_nod_smp(ip) =  istack_line_nod_smp(ip-1)            &
     &                            + fline_lc(ip)%nnod_line_l
      end do
!
      call count_parallel_current_fline(istack_line_nod_smp(np_smp),    &
     &                                  fln_tce)
      call resize_line_start_fline(fln_tce%num_current_fline, fln_tce)
!
!$omp parallel do private(ip,i,ist)
      do ip = 1, np_smp
        ist = istack_line_nod_smp(ip-1)
        do i = 1, fline_lc(ip)%nnod_line_l
          fln_tce%iline_original(i+ist)                                 &
     &       = fline_lc(ip)%iglobal_fline(i)
          fln_tce%xx_fline_start(1:4,i+ist)                             &
     &       = fline_lc(ip)%xx_line_l(1:4,i)
!
          fln_tce%isf_dbl_start(1,i+ist) =    my_rank
          fln_tce%isf_dbl_start(2:3,i+ist)                              &
     &       =  fline_lc(ip)%iedge_line_l(1:2,i)
        end do
      end do
!$omp end parallel do
      deallocate(istack_line_nod_smp)
!
      end subroutine return_to_trace_list
!
!  ---------------------------------------------------------------------
!
      end module add_tracer_fieldline_list
