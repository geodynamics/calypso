!>@file   t_field_on_circle.f90
!!@brief  module t_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine alloc_mul_fields_on_circle(num_circle, mul_circle)
!!      subroutine dealloc_mul_fields_on_circle(mul_circle)
!!        integer(kind = kint), intent(in) :: num_circle
!!        type(mul_fields_on_circle), intent(inout) :: mul_circle
!!      subroutine set_control_circles_def(smonitor_ctl, mul_circle)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(mul_fields_on_circle), intent(inout) :: mul_circle
!!
!!      subroutine init_circle_point_global(sph, comms_sph, trans_p,    &
!!     &                                    cdat, SR_sig, SR_r)
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine dealloc_circle_point_global(cdat)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_grids), intent(in) ::  sph
!!        type(circle_fld_maker), intent(inout) :: cdat
!!@endverbatim
!
      module t_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
      use t_circle_transform
      use t_sph_circle_parameters
      use t_FFT_selector
!
      implicit none
!
!
      type circle_fld_maker
!>        Legendre polynomials at specific latitude
        type(circle_transform_spectr) :: leg_circ
!>        Structure to make fields on circle
        type(circle_parameters) :: circle
!>         Structure of field data on circle
        type(phys_data) :: d_circle
!>        Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
        type(working_FFTs) :: WK_circle_fft
!
        integer(kind = kint), allocatable :: ipol_circle_trns(:)
!
!>         Integer of zonal indexing
        integer(kind = kint), allocatable :: mphi_list(:)
!>         longitude
        real(kind = kreal), allocatable :: phi_list(:)
      end type circle_fld_maker
!
      type mul_fields_on_circle
        integer(kind = kint) :: num_circles = 0
!>         Structure of field data on circle
        type(circle_fld_maker), allocatable :: cdat(:)
      end type mul_fields_on_circle
!
      private :: set_circle_point_global
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_mul_fields_on_circle(num_circle, mul_circle)
!
      integer(kind = kint), intent(in) :: num_circle
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      mul_circle%num_circles = max(num_circle, 0)
      allocate(mul_circle%cdat(mul_circle%num_circles))
!
      end subroutine alloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mul_fields_on_circle(mul_circle)
!
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      deallocate(mul_circle%cdat)
!
      end subroutine dealloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine set_control_circles_def(smonitor_ctl, mul_circle)
!
      use t_ctl_data_circles
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      integer(kind = kint) :: i
!
      call alloc_mul_fields_on_circle(smonitor_ctl%num_circ_ctl,        &
     &                                mul_circle)
!
      do i = 1, mul_circle%num_circles
        call set_control_circle_def(smonitor_ctl%meq_ctl(i),            &
     &                              mul_circle%cdat(i)%circle)
      end do
!
      end subroutine set_control_circles_def
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_circle_point_global(sph, comms_sph, trans_p,      &
     &                                    cdat, SR_sig, SR_r)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_solver_SR
!
      type(sph_grids), intent(in) ::  sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(circle_fld_maker), intent(inout) :: cdat
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: m
      real(kind = kreal) :: delta
!
!
      cdat%leg_circ%ltr_circle =  sph%sph_params%l_truncation
      call alloc_circle_field                                           &
     &   (sph%sph_rtp%nidx_rtp(3), cdat%circle, cdat%d_circle)
      if(my_rank.eq.0) then
        allocate(cdat%mphi_list(cdat%circle%mphi_circle))
        allocate(cdat%phi_list(cdat%circle%mphi_circle))
        delta = 8.0d0 * atan(one) / dble(cdat%circle%mphi_circle)
!$omp parallel do
        do m = 1, cdat%circle%mphi_circle
          cdat%mphi_list(m) = m
          cdat%phi_list(m) = dble(m-1) * delta
        end do
!$omp end parallel do
      end if
!
      call initialize_circle_transform(trans_p%iflag_FFT,               &
     &    cdat%circle, cdat%leg_circ, cdat%WK_circle_fft)
      call set_circle_point_global(sph%sph_rj, cdat%leg_circ,           &
     &                            cdat%circle)
!
      call alloc_work_circle_transform(cdat%d_circle, cdat%leg_circ)
      call init_legendre_on_circle(sph, comms_sph, trans_p,             &
     &                             cdat%leg_circ, SR_sig, SR_r)
!
      end subroutine init_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine set_circle_transfer_address(nod_fld, rj_fld, cdat)
!
      type(phys_data), intent(in) :: nod_fld, rj_fld
      type(circle_fld_maker), intent(inout) :: cdat
!
      logical, allocatable :: flag_use(:)
      integer(kind = kint) :: i_fld, j_fld
!
      allocate(cdat%ipol_circle_trns(1:rj_fld%num_phys))
!$omp parallel workshare
      cdat%ipol_circle_trns(1:rj_fld%num_phys) = 0
!$omp end parallel workshare
!
      allocate(flag_use(1:rj_fld%num_phys))
!$omp parallel workshare
      flag_use(1:rj_fld%num_phys) = .FALSE.
!$omp end parallel workshare
      do i_fld = 1, nod_fld%num_phys_viz
        do j_fld = 1, rj_fld%num_phys
          if(flag_use(j_fld)) cycle
          if(rj_fld%phys_name(j_fld)                                    &
     &         .eq. nod_fld%phys_name(i_fld)) then
            cdat%ipol_circle_trns(i_fld)                                &
     &                      = rj_fld%istack_component(j_fld-1) + 1 
            flag_use(j_fld) = .TRUE.
            exit
          end if
        end do
      end do
      deallocate(flag_use)
!
      end subroutine set_circle_transfer_address
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_point_global(cdat)
!
      type(circle_fld_maker), intent(inout) :: cdat
!
!
      call dealloc_circle_field(cdat%d_circle)
      call dealloc_circle_transform(cdat%leg_circ)
!
      end subroutine dealloc_circle_point_global
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_circle_point_global(sph_rj, leg_circ, circle)
!
      use t_spheric_rj_data
      use set_radial_interpolation
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(circle_transform_spectr), intent(in) :: leg_circ
!
      type(circle_parameters), intent(inout) :: circle
!
      integer(kind = kint) :: kr_st
!
!
      kr_st = 1
      call s_set_radial_interpolation                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    leg_circ%r_circle, kr_st,                                     &
     &    circle%kr_gl_rcirc_in, circle%kr_gl_rcirc_out,                &
     &    circle%coef_gl_rcirc_in)
      circle%coef_gl_rcirc_out = one - circle%coef_gl_rcirc_in
!
      end subroutine set_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine initialize_circle_transform                            &
     &          (iflag_FFT, circle, leg_circ, WK_circle_fft)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(circle_parameters), intent(in) :: circle
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
!
      call alloc_circle_transform(leg_circ)
!
      leg_circ%r_circle                                                &
     &      = sqrt(circle%s_circle**2 + circle%z_circle**2)
      leg_circ%theta_circle                                            &
     &      = acos(circle%z_circle / leg_circ%r_circle)
!
      leg_circ%ar_circle = one / leg_circ%r_circle
      leg_circ%ar2_circle = leg_circ%ar_circle**2
!
      if(my_rank .gt. 0) return
!
      if(i_debug .gt. 0) then
        write(*,*) 'np_smp', np_smp
        write(*,*) 'istack_circfft_smp', leg_circ%istack_circfft_smp
        write(*,*) 'mphi_circle', circle%mphi_circle
      end if
!
      call initialize_FFT_select                                        &
     &   (my_rank, iflag_FFT, np_smp, leg_circ%istack_circfft_smp,      &
     &    circle%mphi_circle, WK_circle_fft)
!
      end subroutine initialize_circle_transform
!
! ----------------------------------------------------------------------
!
      end module t_field_on_circle
