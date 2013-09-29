!>@file   global_field_4_dynamobench.f90
!!@brief  module global_field_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2012
!
!>@brief Evaluate global data for dynamo benchmark test
!!
!!@verbatim
!!      subroutine copy_energy_4_dynamobench
!!      subroutine copy_icore_energy_4_dbench
!!
!!      subroutine pick_inner_core_rotation
!!      subroutine pick_mag_torque_inner_core
!!@endverbatim
!
      module global_field_4_dynamobench
!
      use m_precision
!
      use m_constants
      use m_field_4_dynamobench
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_energy_4_dynamobench
!
      use m_phys_labels
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, num_rms_rj
        if(rms_name_rj(i_fld) .eq. fhd_velo) then
          i_comp = istack_rms_comp_rj(i_fld-1) + 1
          KE_bench(1) = rms_sph_vol(i_comp  )
          KE_bench(2) = rms_sph_vol(i_comp+1)
          KE_bench(3) = rms_sph_vol(i_comp+2)
          exit
        end if
      end do
!
      do i_fld = 1, num_rms_rj
        if(rms_name_rj(i_fld) .eq. fhd_magne) then
          i_comp = istack_rms_comp_rj(i_fld-1) + 1
          ME_bench(1) = rms_sph_vol(i_comp  )
          ME_bench(2) = rms_sph_vol(i_comp+1)
          ME_bench(3) = rms_sph_vol(i_comp+2)
          exit
        end if
      end do
!
      end subroutine copy_energy_4_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine copy_icore_energy_4_dbench
!
      use m_phys_labels
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, num_rms_rj
        if(rms_name_rj(i_fld) .eq. fhd_magne) then
          i_comp = istack_rms_comp_rj(i_fld-1) + 1
          mene_icore(1) = rms_sph_vol(i_comp  )
          mene_icore(2) = rms_sph_vol(i_comp+1)
          mene_icore(3) = rms_sph_vol(i_comp+2)
!
          exit
        end if
      end do
!
      end subroutine copy_icore_energy_4_dbench
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine pick_inner_core_rotation
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: i, i10c_o
      real(kind = kreal) :: rotate_ic_local(-1:1)
!
!
      do i = -1, 1
        if(idx_rj_degree_one(i) .gt. 0) then
          i10c_o = idx_rj_degree_one(i) + (nlayer_ICB-1)*nidx_rj(2)
          rotate_ic_local(i) = d_rj(i10c_o,itor%i_velo)                 &
     &                       * ar_1d_rj(nlayer_ICB,2)
        else
          rotate_ic_local(i) = zero
        end if
      end do
!
      call MPI_allREDUCE (rotate_ic_local, rotate_icore, ithree,        &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine pick_inner_core_rotation
!
! ----------------------------------------------------------------------
!
      subroutine pick_mag_torque_inner_core
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: i, i10c_o
      real(kind = kreal) :: m_torque_local(-1:1)
!
!
      do i = -1, 1
        if(idx_rj_degree_one(i) .gt. 0) then
          i10c_o = idx_rj_degree_one(i) + (nlayer_ICB-1)*nidx_rj(2)
          m_torque_local(i) = d_rj(i10c_o,itor%i_lorentz)               &
     &                       * (radius_1d_rj_r(nlayer_ICB)**3)          &
     &                       * eight * four*atan(one) / (five*three)
        else
          m_torque_local(i) = zero
        end if
      end do
!
      call MPI_allREDUCE (m_torque_local, m_torque_icore, ithree,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine pick_mag_torque_inner_core
!
! ----------------------------------------------------------------------
!
      end module global_field_4_dynamobench
