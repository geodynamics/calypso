!t_flex_delta_t_data.f90
!     module t_flex_delta_t_data
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine alloc_check_delta_t_name(flex)
!!      subroutine alloc_check_delta_t_rms(flex)
!!      subroutine alloc_check_delta_t_data(flex)
!!      subroutine dealloc_check_delta_t_rms(flex)
!!      subroutine dealloc_check_delta_t_data(flex)
!!
!!      subroutine write_delta_t_check_head(id_file, flex)
!!      subroutine write_rms_delta_t_check(id_file, i_step, time, flex)
!!      subroutine write_max_delta_t_check(id_file, i_step, time, flex)
!!      subroutine write_min_delta_t_check(id_file, i_step, time, flex)
!!
!!      subroutine read_rms_delta_t_check(id_file, i_step, time, flex)
!!      subroutine read_max_delta_t_check(id_file, i_step, time, flex)
!!      subroutine read_min_delta_t_check(id_file, i_step, time, flex)
!
      module t_flex_delta_t_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>     Fixed time step flag
      integer(kind=kint), parameter :: iflag_fixed_step = 0
!>     Flexible time step flag
      integer(kind=kint), parameter :: iflag_flex_step =  1
!
!
      type flexible_stepping_parameter
!>        flag for time stepping
        integer(kind=kint) :: iflag_flexible_step = iflag_fixed_step
!>        Integer flag if time stepping is changed
        integer(kind= kint) :: iflag_flex_step_changed = id_turn_OFF
!
!>        End time
        real(kind=kreal) :: time_to_finish
!
!>        significand of @f$ \Delta t @f$
        real(kind=kreal) :: dt_fact
!>        exponent of @f$ \Delta t @f$
        integer(kind = kint) :: idt_digit
!
!>        Maximum length of time step
        integer(kind=kint) :: istep_max_dt
!
!>      Flexible time step number for maximum lenth of each step
        integer(kind=kint) :: istep_flex_to_max = 0
!>
        integer(kind= kint) :: interval_flex_2_max
!
!>        Maximum error to shrink time step
        real(kind=kreal) :: max_eps_to_shrink
!>        Minimum error to expand time step
        real(kind=kreal) :: min_eps_to_expand
!
!>        Maximum @f$ \Delta t @f$
        real(kind=kreal) :: dt_max
!>        Mimimum @f$ \Delta t @f$
        real(kind=kreal) :: dt_min
      end type flexible_stepping_parameter
!
      type flexible_stepping_data
        integer(kind = kint) :: num_fld
        integer(kind = kint) :: ntot_comp
        integer(kind = kint), allocatable :: num_comp(:)
        integer(kind = kint), allocatable :: istack_comp(:)
        character(len=kchara), allocatable :: fld_name(:)
        real(kind=kreal), allocatable :: d_ratio(:)
!
        real(kind=kreal), allocatable :: d_ratio_min_smp(:,:)
        real(kind=kreal), allocatable :: d_ratio_max_smp(:,:)
        real(kind=kreal), allocatable :: d_ratio_min_l(:)
        real(kind=kreal), allocatable :: d_ratio_max_l(:)
        real(kind=kreal), allocatable :: d_ratio_min(:)
        real(kind=kreal), allocatable :: d_ratio_max(:)
!
        real(kind=kreal), allocatable :: rms_dt_local(:)
        real(kind=kreal), allocatable :: ave_dt_local(:)
        real(kind=kreal), allocatable :: rms_dt_global(:)
        real(kind=kreal), allocatable :: ave_dt_global(:)
        real(kind=kreal), allocatable :: rms_dt_pre1(:)
        real(kind=kreal), allocatable :: rms_dt_pre2(:)
!
        integer(kind = kint), allocatable :: inod_min_dratio(:)
        integer(kind = kint), allocatable :: inod_max_dratio(:)
!
        real(kind=kreal) ::  d_ratio_allmax
        real(kind=kreal) ::  d_ratio_allmin
!
        integer(kind=kint) :: i_drmax_v = izero
        integer(kind=kint) :: i_drmax_p = izero
        integer(kind=kint) :: i_drmax_t = izero
        integer(kind=kint) :: i_drmax_b = izero
        integer(kind=kint) :: i_drmax_f = izero
        integer(kind=kint) :: i_drmax_d = izero
      end type flexible_stepping_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_check_delta_t_name(flex)
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      allocate(flex%fld_name(flex%num_fld) )
      allocate(flex%num_comp(flex%num_fld) )
      allocate(flex%istack_comp(0:flex%num_fld) )
!
      if(flex%num_fld .gt. izero) flex%num_comp = izero
      flex%istack_comp = izero
!
      end subroutine alloc_check_delta_t_name
!
! ----------------------------------------------------------------------
!
      subroutine alloc_check_delta_t_rms(flex)
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      allocate(flex%rms_dt_local(flex%ntot_comp))
      allocate(flex%ave_dt_local(flex%ntot_comp))
      allocate(flex%rms_dt_global(0:flex%ntot_comp))
      allocate(flex%ave_dt_global(0:flex%ntot_comp))
      allocate(flex%rms_dt_pre1(0:flex%ntot_comp))
      allocate(flex%rms_dt_pre2(0:flex%ntot_comp))
      allocate(flex%d_ratio(flex%ntot_comp))
!
      if(flex%ntot_comp .gt. 0) then
        flex%rms_dt_local =  zero
        flex%ave_dt_local =  zero
        flex%rms_dt_global = zero
        flex%ave_dt_global = zero
        flex%rms_dt_pre1 =   zero
        flex%rms_dt_pre2 =   zero
        flex%d_ratio =       zero
      end if
!
      end subroutine alloc_check_delta_t_rms
!
! ----------------------------------------------------------------------
!
      subroutine alloc_check_delta_t_data(flex)
!
      use m_machine_parameter
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      allocate(flex%d_ratio_min(flex%ntot_comp))
      allocate(flex%d_ratio_max(flex%ntot_comp))
      allocate(flex%d_ratio_min_l(flex%ntot_comp))
      allocate(flex%d_ratio_max_l(flex%ntot_comp))
      allocate(flex%d_ratio_min_smp(np_smp,flex%ntot_comp))
      allocate(flex%d_ratio_max_smp(np_smp,flex%ntot_comp))
!
      allocate(flex%inod_min_dratio(flex%ntot_comp))
      allocate(flex%inod_max_dratio(flex%ntot_comp))
!
      if(flex%ntot_comp .gt. 0) then
        flex%d_ratio_min =     zero
        flex%d_ratio_max =     zero
        flex%d_ratio_min_l =   zero
        flex%d_ratio_max_l =   zero
        flex%d_ratio_min_smp = zero
        flex%d_ratio_max_smp = zero
        flex%inod_max_dratio = izero
        flex%inod_min_dratio = izero
      end if
!
      end subroutine alloc_check_delta_t_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_check_delta_t_rms(flex)
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      deallocate(flex%rms_dt_local, flex%rms_dt_global)
      deallocate(flex%ave_dt_local, flex%ave_dt_global)
      deallocate(flex%rms_dt_pre1, flex%rms_dt_pre2)
      deallocate(flex%d_ratio)
!
      end subroutine dealloc_check_delta_t_rms
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_check_delta_t_data(flex)
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      deallocate(flex%d_ratio_min, flex%d_ratio_min_l)
      deallocate(flex%d_ratio_max, flex%d_ratio_max_l)
      deallocate(flex%d_ratio_min_smp, flex%d_ratio_max_smp)
      deallocate(flex%fld_name, flex%num_comp)
      deallocate(flex%istack_comp)
      deallocate(flex%inod_min_dratio, flex%inod_max_dratio)
!
      end subroutine dealloc_check_delta_t_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_delta_t_check_head(id_file, flex)
!
      integer(kind = kint), intent(in) :: id_file
      type(flexible_stepping_data), intent(inout) :: flex
!
      integer(kind = kint) :: i_fld
!
!
      write(id_file,'(a)') '! number of field and component to check'
      write(id_file,'(2i5)') flex%num_fld, flex%ntot_comp
!
      write(id_file,'(a)') 'step, '
      do i_fld = 1, flex%num_fld
        if(flex%num_comp(i_fld) .eq. ithree) then
          write(id_file,'(a,a4)') trim(flex%fld_name(i_fld)), '_x, '
          write(id_file,'(a,a4)') trim(flex%fld_name(i_fld)), '_y, '
          write(id_file,'(a,a4)') trim(flex%fld_name(i_fld)), '_z, '
        else if (flex%num_comp(i_fld) .eq. ione) then
          write(id_file,'(a,a2)') trim(flex%fld_name(i_fld)), ', '
        end if
      end do
!
      end subroutine write_delta_t_check_head
!
! ----------------------------------------------------------------------
!
      subroutine write_rms_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       flex%d_ratio(1:flex%ntot_comp)
!
      end subroutine write_rms_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine write_max_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       flex%d_ratio_max(1:flex%ntot_comp)
!
      end subroutine write_max_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine write_min_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       flex%d_ratio_min(1:flex%ntot_comp)
!
      end subroutine write_min_delta_t_check
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_rms_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
!
      type(flexible_stepping_data), intent(inout) :: flex
!
!
!
      read(id_file,*) i_step, time, flex%d_ratio(1:flex%ntot_comp)
!
      end subroutine read_rms_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine read_max_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      read(id_file,*) i_step, time,                                     &
     &                flex%d_ratio_max(1:flex%ntot_comp)
!
      end subroutine read_max_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine read_min_delta_t_check(id_file, i_step, time, flex)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
      type(flexible_stepping_data), intent(inout) :: flex
!
!
      write(id_file,*) i_step, time,                                    &
     &                flex%d_ratio_min(1:flex%ntot_comp)
!
      end subroutine read_min_delta_t_check
!
! ----------------------------------------------------------------------
!
      end module t_flex_delta_t_data
