!>@file   ctl_data_mhd_evo_scheme_IO.f90
!!@brief  module ctl_data_mhd_evo_scheme_IO
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine read_time_loop_ctl                                   &
!!     &         (id_control, hd_block, mevo_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_time_loop_ctl                                  &
!!     &         (id_control, hd_block, mevo_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        integer(kind = kint), intent(inout) :: level
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
      module ctl_data_mhd_evo_scheme_IO
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_ctl_data_mhd_evo_scheme
!
      implicit  none
!
!    4th level for time_loop_ctl
!
      character(len=kchara), parameter, private                         &
     &      :: hd_iflag_supg =     'iflag_supg_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_iflag_v_supg =   'iflag_supg_v_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_iflag_t_supg =   'iflag_supg_t_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_iflag_b_supg =   'iflag_supg_b_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_iflag_c_supg =   'iflag_supg_c_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_num_multi_pass = 'num_multi_pass_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_maxiter =        'maxiter_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_eps_4_velo =     'eps_4_velo_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_eps_4_magne =    'eps_4_magne_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_scheme =         'scheme_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_diff_correct =   'diffuse_correct_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_imp_v =     'coef_imp_v_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_imp_t =     'coef_imp_t_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_imp_b =     'coef_imp_b_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_imp_c =     'coef_imp_c_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_eps_crank =      'eps_crank_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_eps_B_crank =    'eps_B_solver_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_method_4_velo =  'method_4_velo_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_precond_4_crank = 'precond_4_crank_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_import_mode =  'import_table_mode_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_package =  'FFT_library_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_sph_transform_mode =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_legendre_vect_len = 'Legendre_vector_length_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_time_loop_ctl                                     &
     &         (id_control, hd_block, mevo_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mevo_ctl%i_time_loop .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_scheme, mevo_ctl%scheme_ctl)
        call read_chara_ctl_type(c_buf, hd_diff_correct,                &
     &      mevo_ctl%diffuse_correct)
        call read_chara_ctl_type(c_buf, hd_method_4_velo,               &
     &      mevo_ctl%method_4_CN)
        call read_chara_ctl_type(c_buf, hd_precond_4_crank,             &
     &      mevo_ctl%precond_4_CN)
        call read_chara_ctl_type(c_buf, hd_sph_transform_mode,          &
     &      mevo_ctl%Legendre_trans_type)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_FFT_package, mevo_ctl%FFT_library)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_import_mode, mevo_ctl%import_mode)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps_4_velo,  mevo_ctl%eps_4_velo_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps_4_magne, mevo_ctl%eps_4_magne_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_coef_imp_v,  mevo_ctl%coef_imp_v_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_coef_imp_t,  mevo_ctl%coef_imp_t_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_coef_imp_b,  mevo_ctl%coef_imp_b_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_coef_imp_c,  mevo_ctl%coef_imp_c_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps_crank,   mevo_ctl%eps_crank_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps_B_crank, mevo_ctl%eps_B_crank_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iflag_supg,   mevo_ctl%iflag_supg_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iflag_v_supg, mevo_ctl%iflag_supg_v_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iflag_t_supg, mevo_ctl%iflag_supg_t_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iflag_b_supg, mevo_ctl%iflag_supg_b_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iflag_c_supg, mevo_ctl%iflag_supg_c_ctl)
!
        call read_integer_ctl_type(c_buf, hd_num_multi_pass,            &
     &      mevo_ctl%num_multi_pass_ctl)
        call read_integer_ctl_type(c_buf, hd_maxiter,                   &
     &      mevo_ctl%maxiter_ctl)
        call read_integer_ctl_type(c_buf, hd_legendre_vect_len,         &
     &      mevo_ctl%leg_vector_len)
      end do
      mevo_ctl%i_time_loop = 1
!
      end subroutine read_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_time_loop_ctl                                    &
     &         (id_control, hd_block, mevo_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(mevo_ctl%i_time_loop .le. 0) return
!
      maxlen = len_trim(hd_iflag_supg)
      maxlen = max(maxlen, len_trim(hd_iflag_v_supg))
      maxlen = max(maxlen, len_trim(hd_iflag_t_supg))
      maxlen = max(maxlen, len_trim(hd_iflag_b_supg))
      maxlen = max(maxlen, len_trim(hd_iflag_c_supg))
      maxlen = max(maxlen, len_trim(hd_num_multi_pass))
      maxlen = max(maxlen, len_trim(hd_maxiter))
      maxlen = max(maxlen, len_trim(hd_eps_4_velo))
      maxlen = max(maxlen, len_trim(hd_eps_4_magne))
      maxlen = max(maxlen, len_trim(hd_scheme))
      maxlen = max(maxlen, len_trim(hd_diff_correct))
      maxlen = max(maxlen, len_trim(hd_coef_imp_v))
      maxlen = max(maxlen, len_trim(hd_coef_imp_t))
      maxlen = max(maxlen, len_trim(hd_coef_imp_b))
      maxlen = max(maxlen, len_trim(hd_coef_imp_c))
      maxlen = max(maxlen, len_trim(hd_eps_crank))
      maxlen = max(maxlen, len_trim(hd_eps_B_crank))
      maxlen = max(maxlen, len_trim(hd_method_4_velo))
      maxlen = max(maxlen, len_trim(hd_precond_4_crank))
      maxlen = max(maxlen, len_trim(hd_import_mode))
      maxlen = max(maxlen, len_trim(hd_FFT_package))
      maxlen = max(maxlen, len_trim(hd_sph_transform_mode))
      maxlen = max(maxlen, len_trim(hd_legendre_vect_len))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iflag_supg,   mevo_ctl%iflag_supg_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iflag_v_supg, mevo_ctl%iflag_supg_v_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iflag_t_supg, mevo_ctl%iflag_supg_t_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iflag_b_supg, mevo_ctl%iflag_supg_b_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iflag_c_supg, mevo_ctl%iflag_supg_c_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_num_multi_pass, mevo_ctl%num_multi_pass_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_maxiter, mevo_ctl%maxiter_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps_4_velo,  mevo_ctl%eps_4_velo_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps_4_magne, mevo_ctl%eps_4_magne_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_scheme, mevo_ctl%scheme_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_diff_correct, mevo_ctl%diffuse_correct)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_coef_imp_v,  mevo_ctl%coef_imp_v_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_coef_imp_t,  mevo_ctl%coef_imp_t_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_coef_imp_b,  mevo_ctl%coef_imp_b_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_coef_imp_c,  mevo_ctl%coef_imp_c_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps_crank, mevo_ctl%eps_crank_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps_B_crank, mevo_ctl%eps_B_crank_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_method_4_velo, mevo_ctl%method_4_CN)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_precond_4_crank, mevo_ctl%precond_4_CN)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_import_mode, mevo_ctl%import_mode)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_FFT_package, mevo_ctl%FFT_library)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_sph_transform_mode, mevo_ctl%Legendre_trans_type)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_legendre_vect_len, mevo_ctl%leg_vector_len)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_mhd_evo_scheme_IO
