!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine s_const_radial_mat_4_sph
!!@endverbatim
!
      module const_radial_mat_4_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_radial_mat_4_sph
!
      use m_control_parameter
      use m_control_params_sph_MHD
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call allocate_velo_mat_sph
!
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        call allocate_temp_mat_sph
        call const_radial_mat_4_temp_sph
        if(i_debug .eq. iflag_full_msg)                                 &
     &     call check_temp_matrices_sph(my_rank)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call allocate_magne_mat_sph
        call const_radial_mat_4_magne_sph
        if(i_debug .eq. iflag_full_msg)                                 &
     &     call check_magne_matrices_sph(my_rank)
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        call allocate_composit_mat_sph
        call const_radial_mat_4_composit_sph
        if(i_debug .eq. iflag_full_msg)                                 &
     &       call check_composit_matrix_sph(my_rank)
      end if
!
      end subroutine s_const_radial_mat_4_sph
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph
