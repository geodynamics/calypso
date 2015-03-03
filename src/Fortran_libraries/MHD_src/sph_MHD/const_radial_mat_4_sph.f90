!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine const_radial_mat_sph_mhd
!!      subroutine const_radial_mat_sph_snap
!!@endverbatim
!
      module const_radial_mat_4_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: const_radial_matrices_sph
      private :: const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_mhd
!
      use m_spheric_parameter
!
!
      call const_radial_matrices_sph
!
      if(inod_rj_center .eq. 0) return
      call const_radial_mat_sph_w_center
!
      end subroutine const_radial_mat_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_snap
!
      use m_control_parameter
      use m_spheric_parameter
      use m_radial_matrices_sph
      use m_radial_mat_sph_w_center
      use const_r_mat_4_scalar_sph
      use const_r_mat_w_center_sph
!
!
      if (iflag_t_evo_4_velo .lt. id_Crank_nicolson) return
      call allocate_press_mat_sph
!
      if(iflag_debug .gt. 0)                                            &
     &          write(*,*) 'const_radial_mat_4_press_sph'
      call const_radial_mat_4_press_sph
!
      if(inod_rj_center .eq. 0) return
      call allocate_press00_mat_sph
!
      if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
      call const_radial_mat_press00_sph
!
      end subroutine const_radial_mat_sph_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_matrices_sph
!
      use m_control_parameter
      use m_radial_matrices_sph
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call allocate_velo_mat_sph
        call allocate_press_mat_sph
!
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step
        call const_radial_mat_4_press_sph
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        call allocate_temp_mat_sph
        call const_radial_mat_4_temp_sph
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call allocate_magne_mat_sph
        call const_radial_mat_4_magne_sph
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        call allocate_composit_mat_sph
        call const_radial_mat_4_composit_sph
      end if
!
      end subroutine const_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_w_center
!
      use m_control_parameter
      use m_radial_mat_sph_w_center
      use const_r_mat_w_center_sph
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call allocate_press00_mat_sph
!
        if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
        call const_radial_mat_press00_sph
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_temp00_sph'
        call allocate_temp00_mat_sph
        call const_radial_mat_temp00_sph
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_comp00_sph'
        call allocate_comp00_mat_sph
        call const_radial_mat_comp00_sph
      end if
!
      end subroutine const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph
