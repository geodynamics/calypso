!>@file   m_field_4_dynamobench.f90
!!@brief  module m_field_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine open_dynamobench_monitor_file
!!      subroutine output_field_4_dynamobench(i_step, time)
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module m_field_4_dynamobench
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>      file ID for benchmark output file
      integer(kind=kint), parameter :: id_dynamobench = 41
!>      file name for benchmark output file
      character(len=kchara), parameter                                  &
     &      :: dynamobench_field_name = 'dynamobench_field.dat'
!
!
!>      temperature address for spherical transform at equator
      integer(kind = kint) :: ibench_temp =  1
!>      velocity address for spherical transform at equator
      integer(kind = kint) :: ibench_velo =  2
!>      magnetic field address for spherical transform at equator
      integer(kind = kint) :: ibench_magne = 5
!
!>      average kinetic energy (poloidal, toroidal, total)
      real(kind = kreal) :: KE_bench(3)
!>      average magnetic energy (poloidal, toroidal, total)
      real(kind = kreal) :: ME_bench(3)
!
!>      time for previus monitoring of omega
      real(kind = kreal) :: t_prev = zero
!>      longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
      real(kind = kreal) :: phi_zero(4) = (/zero,zero,zero,zero/)
!>      longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
!!      at previous monitoring
      real(kind = kreal) :: phi_prev(4) = (/zero,zero,zero,zero/)
!>      drift frequency
      real(kind = kreal) :: drift(0:4)
!>      mangetic energy in inner core
      real(kind = kreal) :: mene_icore(3)
!>      rotation rate for inner core
      real(kind = kreal) :: rotate_icore(-1:1)
!>      magnetic torque for inner core
      real(kind = kreal) :: m_torque_icore(-1:1)
!
!>      phase of by @f$ V_{S4}^{4} @f$
      real(kind = kreal) :: phase_vm4(2)      = (/zero,zero/)
!>      phase of by @f$ V_{S4}^{4} @f$
!!      at previous monitoring
      real(kind = kreal) :: phase_vm4_prev(2) = (/zero,zero/)
!>      drift frequency obtained by @f$ V_{S4}^{4} @f$
      real(kind = kreal) :: omega_vm4(2)      = (/zero,zero/)
!
!>      local point data
      real(kind = kreal) :: d_zero(0:4,7)
!
      private :: id_dynamobench, dynamobench_field_name
      private :: open_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_dynamobench_monitor_file
!
      use m_control_params_sph_MHD
      use m_boundary_params_sph_MHD
      use m_sph_phys_address
!
!
      open(id_dynamobench, file=dynamobench_field_name,                 &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
  99  continue
      open(id_dynamobench, file=dynamobench_field_name)
!
      write(id_dynamobench,'(a)', advance='NO') 't_step    time    '
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'KE_pol    KE_tor    KE_total    '
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     'ME_pol    ME_tor    ME_total    '
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     'ME_pol_icore    ME_tor_icore    ME_total_icore    '
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') 'omega_ic_z    '
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &  .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') 'MAG_torque_ic_z    '
      end if
!
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'phi_1    phi_2    phi_3    phi_4    '
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'omega_vp44    omega_vt54    '
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO') 'B_theta    '
      end if
!
      write(id_dynamobench,'(a)')  'v_phi    temp'
!
      end subroutine open_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine output_field_4_dynamobench(i_step, time)
!
      use m_control_params_sph_MHD
      use m_boundary_params_sph_MHD
      use m_sph_phys_address
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      if(my_rank .ne. 0) return
!
      call open_dynamobench_monitor_file
!
      write(id_dynamobench,'(i10,1pE25.15e3)', advance='NO')            &
     &     i_step, time
      write(id_dynamobench,'(1p3E25.15e3)', advance='NO') KE_bench(1:3)
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     ME_bench(1:3)
      end if
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     mene_icore(1:3)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     rotate_icore(0)
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &   .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     m_torque_icore(0)
      end if
!
      write(id_dynamobench,'(1p4E25.15e3)', advance='NO') phi_zero(1:4)
      write(id_dynamobench,'(1p2E25.15e3)', advance='NO')               &
     &      omega_vm4(1:2)
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(1p2E25.15e3)', advance='NO')             &
     &      d_zero(0,ibench_magne+1)
      end if
!
      write(id_dynamobench,'(1p2E25.15e3)')                             &
     &     d_zero(0,ibench_velo+2), d_zero(0,ibench_temp)
!
      close(id_dynamobench)
!
      end subroutine output_field_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module m_field_4_dynamobench
