!>@file   m_ctl_data_mhd_evo_scheme.f90
!!@brief  module m_ctl_data_mhd_evo_scheme
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine read_restart_ctl
!!      subroutine read_time_loop_ctl
!!
!! !!!!  control for initial and restart data  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!   no_data:             No initial values
!!   start_from_rst_file: Read restart data as initial values
!!
!!   dynamo_benchmark_0: Initial values for dynamo benchmark Case 0
!!   dynamo_benchmark_1: Initial values for dynamo benchmark Case 1
!!   dynamo_benchmark_2: Initial values for dynamo benchmark Case 1
!!
!!   pseudo_vacuum_benchmark: Initial values for pseudo vacuum benchmark
!!
!!   rotate_x: rotate around x-axis
!!   rotate_y: rotate around y-axis
!!   rotate_z: rotate around z-axis
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin restart_file_ctl
!!     rst_ctl                start_from_rst_file
!!    end restart_file_ctl
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
      module m_ctl_data_mhd_evo_scheme
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara) :: restart_flag_ctl
!   control flage for restart data
!
      real(kind=kreal) :: coef_imp_v_ctl
      real(kind=kreal) :: coef_imp_t_ctl
      real(kind=kreal) :: coef_imp_b_ctl
      real(kind=kreal) :: coef_imp_c_ctl
!
!
      character(len=kchara) :: iflag_supg_ctl
      character(len=kchara) :: iflag_supg_v_ctl
      character(len=kchara) :: iflag_supg_t_ctl
      character(len=kchara) :: iflag_supg_b_ctl
      character(len=kchara) :: iflag_supg_c_ctl
!
      integer(kind=kint) :: num_multi_pass_ctl
      integer(kind=kint) :: maxiter_ctl
! 
      real(kind=kreal)   :: eps_4_velo_ctl
      real(kind=kreal)   :: eps_4_magne_ctl
!
      real(kind=kreal)   :: eps_crank_ctl
      real(kind=kreal)   :: eps_B_crank_ctl
! 
      character (len=kchara)   :: scheme_ctl
      character (len=kchara)   :: diffuse_correct_ctl
! 
      character (len=kchara)   :: method_4_velo_ctl
      character (len=kchara)   :: precond_4_crank_ctl
! 
      character(len = kchara) :: Legendre_trans_loop_ctl
      character(len = kchara) :: FFT_library_ctl
      character(len = kchara) :: import_mode_ctl
      character(len = kchara) :: SR_routine_ctl
!
      integer(kind = kint) :: legendre_vector_len_ctl
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &      :: hd_restart_file =   'restart_file_ctl'
      integer (kind=kint) :: i_restart_file =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_loop =      'time_loop_ctl'
      integer (kind=kint) :: i_time_loop =      0
!
!    4th level for restart
!
      character(len=kchara), parameter :: hd_rst_flag = 'rst_ctl'
      integer (kind=kint) :: i_rst_flag = 0
!
!    4th level for time_loop_ctl
!
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_supg =     'iflag_supg_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_v_supg =   'iflag_supg_v_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_t_supg =   'iflag_supg_t_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_b_supg =   'iflag_supg_b_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_c_supg =   'iflag_supg_c_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_num_multi_pass = 'num_multi_pass_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_maxiter =        'maxiter_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_4_velo =     'eps_4_velo_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_4_magne =    'eps_4_magne_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_scheme =         'scheme_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_diff_correct =   'diffuse_correct_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_v =     'coef_imp_v_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_t =     'coef_imp_t_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_b =     'coef_imp_b_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_c =     'coef_imp_c_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_crank =      'eps_crank_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_B_crank =    'eps_B_solver_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_method_4_velo =  'method_4_velo_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_precond_4_crank = 'precond_4_crank_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_sph_transform_mode =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_FFT_package =  'FFT_library_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_import_mode =  'import_table_mode_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_SR_routine =   'send_recv_routine_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_legendre_vect_len = 'Legendre_vector_length_ctl'
!
      integer (kind=kint) :: i_iflag_supg =         0
      integer (kind=kint) :: i_iflag_v_supg =       0
      integer (kind=kint) :: i_iflag_t_supg =       0
      integer (kind=kint) :: i_iflag_b_supg =       0
      integer (kind=kint) :: i_iflag_c_supg =       0
!
      integer (kind=kint) :: i_num_multi_pass =     0
      integer (kind=kint) :: i_maxiter =            0
      integer (kind=kint) :: i_eps_4_velo =         0
      integer (kind=kint) :: i_eps_4_magne =        0
      integer (kind=kint) :: i_scheme =             0
      integer (kind=kint) :: i_diff_correct =       0
      integer (kind=kint) :: i_coef_imp_v =         0
      integer (kind=kint) :: i_coef_imp_t =         0
      integer (kind=kint) :: i_coef_imp_b =         0
      integer (kind=kint) :: i_coef_imp_c =         0
      integer (kind=kint) :: i_eps_crank =          0
      integer (kind=kint) :: i_eps_B_crank =        0
      integer (kind=kint) :: i_method_4_velo =      0
      integer (kind=kint) :: i_precond_4_crank =    0
      integer (kind=kint) :: i_sph_transform_mode = 0
      integer (kind=kint) :: i_FFT_package =        0
      integer (kind=kint) :: i_import_mode =        0
      integer (kind=kint) :: i_SR_routine =         0
      integer (kind=kint) :: i_legendre_vect_len  = 0
!
      private :: hd_restart_file, hd_rst_flag, i_restart_file
      private :: hd_time_loop, i_time_loop
      private :: hd_iflag_supg, hd_num_multi_pass, hd_maxiter
      private :: hd_iflag_v_supg, hd_iflag_t_supg, hd_iflag_b_supg
      private :: hd_iflag_c_supg, hd_eps_B_crank
      private :: hd_eps_4_velo, hd_eps_4_magne, hd_scheme
      private :: hd_diff_correct, hd_coef_imp_v, hd_coef_imp_t
      private :: hd_coef_imp_b, hd_coef_imp_c, hd_eps_crank
      private :: hd_method_4_velo, hd_precond_4_crank
      private :: hd_sph_transform_mode, hd_legendre_vect_len
      private :: hd_FFT_package, hd_import_mode, hd_SR_routine
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_restart_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_restart_file) .eq. 0) return
      if (i_restart_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_restart_file, i_restart_file)
        if(i_restart_file .gt. 0) exit
!
        call read_character_ctl_item(hd_rst_flag,                       &
     &          i_rst_flag, restart_flag_ctl)
      end do
!
      end subroutine read_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_time_loop_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_time_loop) .eq. 0) return
      if (i_time_loop .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_time_loop, i_time_loop)
        if(i_time_loop .gt. 0) exit
!
        call read_character_ctl_item(hd_scheme, i_scheme, scheme_ctl)
        call read_character_ctl_item(hd_diff_correct,                   &
     &        i_diff_correct, diffuse_correct_ctl)
        call read_character_ctl_item(hd_method_4_velo,                  &
     &        i_method_4_velo, method_4_velo_ctl)
        call read_character_ctl_item(hd_precond_4_crank,                &
     &        i_precond_4_crank, precond_4_crank_ctl)
        call read_character_ctl_item(hd_sph_transform_mode,             &
     &          i_sph_transform_mode, Legendre_trans_loop_ctl)
        call read_character_ctl_item(hd_FFT_package,                    &
     &          i_FFT_package, FFT_library_ctl)
        call read_character_ctl_item(hd_import_mode,                    &
     &          i_import_mode, import_mode_ctl)
        call read_character_ctl_item(hd_SR_routine,                     &
     &          i_SR_routine, SR_routine_ctl)
!
        call read_real_ctl_item(hd_eps_4_velo, i_eps_4_velo,            &
     &        eps_4_velo_ctl)
        call read_real_ctl_item(hd_eps_4_magne, i_eps_4_magne,          &
     &        eps_4_magne_ctl)
        call read_real_ctl_item(hd_coef_imp_v, i_coef_imp_v,            &
     &        coef_imp_v_ctl)
        call read_real_ctl_item(hd_coef_imp_t, i_coef_imp_t,            &
     &        coef_imp_t_ctl)
        call read_real_ctl_item(hd_coef_imp_b, i_coef_imp_b,            &
     &        coef_imp_b_ctl)
        call read_real_ctl_item(hd_coef_imp_c, i_coef_imp_c,            &
     &        coef_imp_c_ctl)
        call read_real_ctl_item(hd_eps_crank, i_eps_crank,              &
     &        eps_crank_ctl)
        call read_real_ctl_item(hd_eps_B_crank, i_eps_B_crank,          &
     &        eps_B_crank_ctl)
!
        call read_character_ctl_item(hd_iflag_supg, i_iflag_supg,       &
     &        iflag_supg_ctl)
        call read_character_ctl_item(hd_iflag_v_supg, i_iflag_v_supg,   &
     &        iflag_supg_v_ctl)
        call read_character_ctl_item(hd_iflag_t_supg, i_iflag_t_supg,   &
     &        iflag_supg_t_ctl)
        call read_character_ctl_item(hd_iflag_b_supg, i_iflag_b_supg,   &
     &        iflag_supg_b_ctl)
        call read_character_ctl_item(hd_iflag_c_supg, i_iflag_c_supg,   &
     &        iflag_supg_c_ctl)
!
        call read_integer_ctl_item(hd_num_multi_pass, i_num_multi_pass, &
     &        num_multi_pass_ctl)
        call read_integer_ctl_item(hd_maxiter, i_maxiter, maxiter_ctl)
        call read_integer_ctl_item(hd_legendre_vect_len,                &
     &        i_legendre_vect_len, legendre_vector_len_ctl)
      end do
!
      end subroutine read_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evo_scheme
