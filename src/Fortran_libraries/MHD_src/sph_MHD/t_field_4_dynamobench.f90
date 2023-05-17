!>@file   t_field_4_dynamobench.f90
!!@brief  module t_field_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine init_circle_field_name_dbench(fld_ctl,               &
!!     &                                         d_circle, bench)
!!        type(ctl_array_c3), intent(in) :: fld_ctl
!!        type(phys_data), intent(inout) :: d_circle
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module t_field_4_dynamobench
!
      use m_precision
      use m_constants
      use t_time_data
      use t_phys_address
      use t_base_field_labels
      use t_boundary_data_sph_MHD
      use t_sph_trans_arrays_MHD
!
      implicit none
!
      type dynamobench_monitor
!>        Integer flag to get dynamo benchmark data
        integer(kind = kint) :: iflag_dynamobench =  0
!>        file prefix for benchmark output file
        character(len=kchara) :: benchmark_file_prefix
!
!>        file prefix for detailed benchmark output file
        character(len=kchara) :: detail_bench_file_prefix
!
!>        Address of volume monitor data for outer core
        integer(kind = kint) :: ipwr_ocore =  0
!>        Address of volume monitor data for inner core
        integer(kind = kint) :: ipwr_icore =  0
!
!>        average kinetic energy (poloidal, toroidal, total)
        real(kind = kreal) :: KE_bench(3)
!>        average magnetic energy (poloidal, toroidal, total)
        real(kind = kreal) :: ME_bench(3)
!
!>        time for previus monitoring of omega
        real(kind = kreal) :: t_prev = zero
!>        longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
        real(kind = kreal) :: phi_zero(4) = (/zero,zero,zero,zero/)
!>        longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
!!        at previous monitoring
        real(kind = kreal) :: phi_prev(4) = (/zero,zero,zero,zero/)
!>        drift phase velocity for @f$v_r = 0 @f$
        real(kind = kreal) :: phase_vr(4) = (/zero,zero,zero,zero/)
!>        drift phase velocity for @f$v_r = 0 @f$
        real(kind = kreal) :: ave_phase_vr = 0.0d0
!>        mangetic energy in inner core
        real(kind = kreal) :: mene_icore(3)
!>        rotation rate for inner core
        real(kind = kreal) :: rotate_icore(-1:1)
!>        magnetic torque for inner core
        real(kind = kreal) :: m_torque_icore(-1:1)
!
!>        phase of by @f$ V_{S4}^{4} @f$
        real(kind = kreal) :: phase_vm4(2)      = (/zero,zero/)
!>        phase of by @f$ V_{S4}^{4} @f$
!!        at previous monitoring
        real(kind = kreal) :: phase_vm4_prev(2) = (/zero,zero/)
!>        drift frequency obtained by @f$ V_{S4}^{4} @f$
        real(kind = kreal) :: omega_vm4(2)      = (/zero,zero/)
!
!>        local point data
        real(kind = kreal), allocatable :: d_zero(:,:)
!
!>        Address list for circle data
        type(base_field_address) :: iphys_dbench
      end type dynamobench_monitor
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_dynamobench_monitor(d_circle, bench)
!
      use t_phys_data
!
      type(phys_data), intent(inout) :: d_circle
      type(dynamobench_monitor), intent(inout) :: bench
!
      allocate(bench%d_zero(0:4,d_circle%ntot_phys))
      if(d_circle%ntot_phys .le. 0) return
      bench%d_zero(0:4,1:d_circle%ntot_phys) = 0.0d0
!
      end subroutine alloc_dynamobench_monitor
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_dynamobench_monitor(bench)
!
      type(dynamobench_monitor), intent(inout) :: bench
!
      if(allocated(bench%d_zero) .eqv. .FALSE.) return
      deallocate(bench%d_zero)
!
      end subroutine dealloc_dynamobench_monitor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_circle_field_name_dbench(ipol, d_circle, bench)
!
      use t_phys_data
      use m_base_field_labels
      use m_phys_constants
      use skip_comment_f
!
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: d_circle
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: ifld
!
!
      d_circle%num_phys = 0
      if(ipol%base%i_velo .gt. 0) then
        d_circle%num_phys = d_circle%num_phys + 1
      end if
      if(ipol%base%i_magne .gt. 0) then
        d_circle%num_phys = d_circle%num_phys + 1
      end if
      if(ipol%base%i_temp .gt. 0) then
        d_circle%num_phys = d_circle%num_phys + 1
      end if
      if(ipol%base%i_light .gt. 0) then
        d_circle%num_phys = d_circle%num_phys + 1
      end if
!
      call alloc_phys_name(d_circle)
!
      ifld = 0
      if(ipol%base%i_velo .gt. 0) then
        ifld = ifld + 1
        bench%iphys_dbench%i_velo                                       &
     &                    = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     velocity%name
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      if(ipol%base%i_magne .gt. 0) then
        ifld = ifld + 1
        bench%iphys_dbench%i_magne                                      &
     &                    = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     magnetic_field%name
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      if(ipol%base%i_temp .gt. 0) then
        ifld = ifld + 1
        bench%iphys_dbench%i_temp                                       &
     &                    = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     temperature%name
        d_circle%num_component(ifld) = n_scalar
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_scalar
      end if
      if(ipol%base%i_light .gt. 0) then
        ifld = ifld + 1
        bench%iphys_dbench%i_light                                      &
     &                    = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     composition%name
        d_circle%num_component(ifld) = n_scalar
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_scalar
      end if
      d_circle%flag_monitor = .TRUE.
      d_circle%ntot_phys =     d_circle%istack_component(ifld)
      d_circle%num_phys_viz =  d_circle%num_phys
      d_circle%ntot_phys_viz = d_circle%ntot_phys
!
      end subroutine init_circle_field_name_dbench
!
! -----------------------------------------------------------------------
!
      end module t_field_4_dynamobench
