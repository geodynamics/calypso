!>@file   t_ctl_data_mhd_evo_scheme.f90
!!@brief  module t_ctl_data_mhd_evo_scheme
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine reset_time_loop_ctl(mevo_ctl)
!!        type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
!!
!! !!!   method for time evolution  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   iflag_supg_ctl:      0...no SUPG 1...SUPG
!!   iflag_supg_v_ctl           Off
!!   iflag_supg_t_ctl           Off
!!   iflag_supg_b_ctl           Off
!!   iflag_supg_c_ctl           Off
!!
!!   num_multi_pass_ctl:  iteration counts for multi pass
!!   maxiter_ctl:         maximum iteration number for correction
!!   eps_4_velo_ctl:      ||div v||_{n} / ||div v||_{n-1}
!!   eps_4_magne_ctl:     ||div B||_{n} / ||div B||_{n-1}
!!   scheme_ctl:          Scheme for time evolution
!!                 explicit_Euler...explicit_Euler
!!                 2nd_Adams_Bashforth...2nd_Adams_Bashforth
!!                 Crank_Nicolson...Crank_Nicolson with 2nd_Adams_Bashforth
!!                 Crank_Nicolson_consist...Crank_Nicolson
!!                                         with consistent mass matrix
!!   eps_crank_ctl:        
!!   method_4_velo_ctl:    method for Crank Nicolson Scheme
!!   precond_4_crank_ctl:  preconditioning method for Crank Nicolson Scheme
!!
!!   Legendre_trans_loop_ctl: Legendre_transform loop type
!!                   ('inner_radial_loop' 'outer_radial_loop' 'long_loop')
!!   FFT_library_ctl:  Selection of FFT librarry  ('FFTW3' or 'FFTPACK')
!!   import_table_mode_ctl:   Selection of import mode
!!                     ('regular_table' or 'reversed_table')
!!   send_recv_routine_ctl:    Selection of send_recv_routines
!!                     ('SEND_RECV', 'AllToAllv', or 'AllToAll')
!!   Legendre_vector_length_ctl:  vector length for Legendre transform
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin time_loop_ctl
!!      iflag_supg_ctl           0
!!      num_multi_pass_ctl       1
!!      maxiter_ctl              1
!!      eps_4_velo_ctl           5.0e-1
!!      eps_4_magne_ctl          5.0e-1
!!      scheme_ctl              Crank_Nicolson
!!      diffuse_correct_ctl     On
!!      coef_implicit_ctl       5.0e-1
!!      coef_imp_v_ctl          5.0e-1
!!      coef_imp_t_ctl          5.0e-1
!!      coef_imp_b_ctl          5.0e-1
!!      coef_imp_c_ctl          5.0e-1
!!
!!      eps_crank_ctl           1.0e-6
!!      eps_B_solver_ctl        1.0e-6
!!      method_4_velo_ctl      CG 
!!      precond_4_crank_ctl     SSOR   
!!
!!      Legendre_trans_loop_ctl   'inner_radial_loop'
!!      FFT_library_ctl           'FFTW'
!!
!!      Legendre_vector_length_ctl    2
!!    end time_loop_ctl
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_evo_scheme
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
      type mhd_evo_scheme_control
!>        Block name
        character(len=kchara) :: block_name = 'time_loop_ctl'
!
        type(read_real_item) :: coef_implicit_ctl
        type(read_real_item) :: coef_imp_v_ctl
        type(read_real_item) :: coef_imp_t_ctl
        type(read_real_item) :: coef_imp_b_ctl
        type(read_real_item) :: coef_imp_c_ctl
!
!
        type(read_character_item) :: iflag_supg_ctl
        type(read_character_item) :: iflag_supg_v_ctl
        type(read_character_item) :: iflag_supg_t_ctl
        type(read_character_item) :: iflag_supg_b_ctl
        type(read_character_item) :: iflag_supg_c_ctl
!
        type(read_integer_item) :: num_multi_pass_ctl
        type(read_integer_item) :: maxiter_ctl
! 
        type(read_real_item) :: eps_4_velo_ctl
        type(read_real_item) :: eps_4_magne_ctl
!
        type(read_real_item) :: eps_crank_ctl
        type(read_real_item) :: eps_B_crank_ctl
! 
        type(read_character_item) :: scheme_ctl
        type(read_character_item) :: diffuse_correct
! 
        type(read_character_item) :: method_4_CN
        type(read_character_item) :: precond_4_CN
! 
        type(read_character_item) :: Legendre_trans_type
        type(read_character_item) :: FFT_library
        type(read_character_item) :: import_mode
!
        type(read_integer_item) :: leg_vector_len
!
        integer (kind=kint) :: i_time_loop =      0
      end type mhd_evo_scheme_control
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine reset_time_loop_ctl(mevo_ctl)
!
      type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
!
!
      mevo_ctl%scheme_ctl%iflag =          0
      mevo_ctl%diffuse_correct%iflag =     0
      mevo_ctl%method_4_CN%iflag =         0
      mevo_ctl%precond_4_CN%iflag =        0
      mevo_ctl%Legendre_trans_type%iflag = 0
      mevo_ctl%FFT_library%iflag =         0
      mevo_ctl%import_mode%iflag =         0
!
      mevo_ctl%eps_4_velo_ctl%iflag =  0
      mevo_ctl%eps_4_magne_ctl%iflag = 0
      mevo_ctl%coef_implicit_ctl%iflag =  0
      mevo_ctl%coef_imp_v_ctl%iflag =  0
      mevo_ctl%coef_imp_t_ctl%iflag =  0
      mevo_ctl%coef_imp_b_ctl%iflag =  0
      mevo_ctl%coef_imp_c_ctl%iflag =  0
      mevo_ctl%eps_crank_ctl%iflag =   0
      mevo_ctl%eps_B_crank_ctl%iflag = 0
!
      mevo_ctl%iflag_supg_ctl%iflag =   0
      mevo_ctl%iflag_supg_v_ctl%iflag = 0
      mevo_ctl%iflag_supg_t_ctl%iflag = 0
      mevo_ctl%iflag_supg_b_ctl%iflag = 0
      mevo_ctl%iflag_supg_c_ctl%iflag = 0
!
      mevo_ctl%num_multi_pass_ctl%iflag = 0
      mevo_ctl%maxiter_ctl%iflag =        0
      mevo_ctl%leg_vector_len%iflag =     0
!
      mevo_ctl%i_time_loop = 0
!
      end subroutine reset_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_evo_scheme
