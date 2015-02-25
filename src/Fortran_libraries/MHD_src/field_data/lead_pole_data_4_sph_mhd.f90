!> @file  lead_pole_data_4_sph_mhd.f90
!!      module lead_pole_data_4_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Spherical transform at poles
!!
!!@verbatim
!!      subroutine lead_pole_fields_4_sph_mhd
!!@endverbatim
!
      module lead_pole_data_4_sph_mhd
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine lead_pole_fields_4_sph_mhd
!
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
      use pole_energy_flux_sph
      use copy_MHD_4_pole_trans
!
!
      if(iflag_shell_mode .eq. iflag_MESH_same) return
!
      if (iflag_debug.eq.1) write(*,*) 'copy_snap_vec_from_pole_trans'
      call copy_snap_vec_from_pole_trans
!
      if (iflag_debug.eq.1) write(*,*) 'pole_nonlinear_sph_MHD'
      call pole_nonlinear_sph_MHD
      if (iflag_debug.eq.1) write(*,*) 'pole_energy_flux_rtp'
      call pole_energy_flux_rtp
!
      end subroutine lead_pole_fields_4_sph_mhd
!
!-----------------------------------------------------------------------
!
      end module lead_pole_data_4_sph_mhd
