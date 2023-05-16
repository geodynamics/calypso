!>@file   global_field_4_dynamobench.f90
!!@brief  module global_field_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2012
!
!>@brief Evaluate global data for dynamo benchmark test
!!
!!@verbatim
!!      subroutine copy_kin_energy_4_dbench(pwr, KE_bench)
!!      subroutine copy_mag_energy_4_dbench(ipow, pwr, ME_bench)
!!        integer(kind = kint), intent(in) :: ipow
!!        type(sph_mean_squares), intent(in) :: pwr
!!        real(kind = kreal), intent(inout) :: KE_bench(3)
!!        real(kind = kreal), intent(inout) :: ME_bench(3)
!!
!!      subroutine pick_inner_core_rotation(idx_rj_degree_one, nidx_rj, &
!!     &          nlayer_ICB, ar_1d_rj, is_velo,                        &
!!     &          nnod_rj, ntot_phys_rj, d_rj, rotate_icore)
!!      subroutine pick_mag_torque_inner_core(idx_rj_degree_one,        &
!!     &          nidx_rj, nlayer_ICB, radius_1d_rj_r, is_lorentz,      &
!!     &          nnod_rj, ntot_phys_rj, d_rj, m_torque_icore)
!!@endverbatim
!
      module global_field_4_dynamobench
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_kin_energy_4_dbench(ipow, pwr, KE_bench)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: ipow
      type(sph_mean_squares), intent(in) :: pwr
      real(kind = kreal), intent(inout) :: KE_bench(3)
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, pwr%num_fld_sq
        if(pwr%pwr_name(i_fld) .eq. velocity%name) then
          i_comp = pwr%istack_comp_sq(i_fld-1) + 1
          KE_bench(1) = pwr%v_spectr(ipow)%v_sq(i_comp  )
          KE_bench(2) = pwr%v_spectr(ipow)%v_sq(i_comp+1)
          KE_bench(3) = pwr%v_spectr(ipow)%v_sq(i_comp+2)
          exit
        end if
      end do
!
      end subroutine copy_kin_energy_4_dbench
!
! ----------------------------------------------------------------------
!
      subroutine copy_mag_energy_4_dbench(ipow, pwr, ME_bench)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: ipow
      type(sph_mean_squares), intent(in) :: pwr
      real(kind = kreal), intent(inout) :: ME_bench(3)
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, pwr%num_fld_sq
        if(pwr%pwr_name(i_fld) .eq. magnetic_field%name) then
          i_comp = pwr%istack_comp_sq(i_fld-1) + 1
          ME_bench(1) = pwr%v_spectr(ipow)%v_sq(i_comp  )
          ME_bench(2) = pwr%v_spectr(ipow)%v_sq(i_comp+1)
          ME_bench(3) = pwr%v_spectr(ipow)%v_sq(i_comp+2)
          exit
        end if
      end do
!
      end subroutine copy_mag_energy_4_dbench
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine pick_inner_core_rotation(idx_rj_degree_one, nidx_rj,   &
     &          nlayer_ICB, ar_1d_rj, is_velo,                          &
     &          nnod_rj, ntot_phys_rj, d_rj, rotate_icore)
!
      use calypso_mpi_real
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: idx_rj_degree_one(-1:1)
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nlayer_ICB
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: rotate_icore(-1:1)
!
      integer(kind = kint) :: i, i10c_o
      real(kind = kreal) :: rotate_ic_local(-1:1)
!
!
      do i = -1, 1
        if(idx_rj_degree_one(i) .gt. 0) then
          i10c_o = idx_rj_degree_one(i) + (nlayer_ICB-1)*nidx_rj(2)
          rotate_ic_local(i) = d_rj(i10c_o,is_velo+2)                   &
     &                       * ar_1d_rj(nlayer_ICB,2)
        else
          rotate_ic_local(i) = zero
        end if
      end do
!
      call calypso_mpi_allreduce_real                                   &
     &   (rotate_ic_local, rotate_icore, cast_long(3),  MPI_SUM)
!
      end subroutine pick_inner_core_rotation
!
! ----------------------------------------------------------------------
!
      subroutine pick_mag_torque_inner_core(idx_rj_degree_one,          &
     &          nidx_rj, nlayer_ICB, radius_1d_rj_r, is_lorentz,        &
     &          nnod_rj, ntot_phys_rj, d_rj, m_torque_icore)
!
      use calypso_mpi_real
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: idx_rj_degree_one(-1:1)
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nlayer_ICB
      integer(kind = kint), intent(in) :: is_lorentz
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: m_torque_icore(-1:1)
!
!
      integer(kind = kint) :: i, i10c_o
      real(kind = kreal) :: m_torque_local(-1:1)
!
!
      do i = -1, 1
        if(idx_rj_degree_one(i) .gt. 0) then
          i10c_o = idx_rj_degree_one(i) + (nlayer_ICB-1)*nidx_rj(2)
          m_torque_local(i) = d_rj(i10c_o,is_lorentz+2)                 &
     &                       * (radius_1d_rj_r(nlayer_ICB)**3)          &
     &                       * eight * four*atan(one) / (five*three)
        else
          m_torque_local(i) = zero
        end if
      end do
!
      call calypso_mpi_allreduce_real                                   &
     &   (m_torque_local, m_torque_icore, cast_long(3), MPI_SUM)
!
      end subroutine pick_mag_torque_inner_core
!
! ----------------------------------------------------------------------
!
      end module global_field_4_dynamobench
