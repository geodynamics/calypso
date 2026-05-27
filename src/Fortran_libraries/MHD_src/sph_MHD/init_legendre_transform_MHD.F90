!>@file   init_legendre_transform_MHD.F90
!!@brief  module init_legendre_transform_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_leg_trans_sph_MHD                               &
!!     &         (MHD_prop, sph_MHD_bc, sph, comms_sph, omega_sph,      &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,&
!!     &          trns_MHD, WK_leg, WK_FFTs_MHD, trans_p, gt_cor,       &
!!     &          cor_rlm, rj_fld, SR_sig, SR_r)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        integer(kind = kint), intent(in) :: ncomp_max_trans
!!        integer(kind = kint), intent(in) :: nvector_max_trans
!!        integer(kind = kint), intent(in) :: nscalar_max_trans
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!!
      module init_legendre_transform_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_const_wz_coriolis_rtp
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
      use t_solver_SR
!
      implicit  none
!
      integer(kind = kint), parameter, private :: num_test =      5
      integer(kind = kint), parameter, private                          &
     &            :: list_test(num_test) = (/iflag_leg_symmetry,        &
     &                                       iflag_leg_sym_spin_loop,   &
     &                                       iflag_leg_sym_matmul,      &
     &                                       iflag_leg_sym_matmul_big,  &
     &                                       iflag_on_the_fly_matprod/)
!
      integer(kind = kint), parameter, private :: ntest_blas =    1
      integer(kind = kint), parameter, private                          &
     &      :: list_blas_test(ntest_blas) = (/iflag_leg_sym_dgemm_big/)
!
      private :: find_fastest_leg_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_trans_sph_MHD                                 &
     &         (MHD_prop, sph_MHD_bc, sph, comms_sph, omega_sph,        &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          trns_MHD, WK_leg, WK_FFTs_MHD, trans_p, gt_cor,         &
     &          cor_rlm, rj_fld, SR_sig, SR_r)
!
      use m_legendre_transform_list
      use skip_comment_f
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      character(len=kchara) :: tmpchara
!
!
      call find_fastest_leg_trans(sph, comms_sph, MHD_prop%fl_prop,     &
     &    sph_MHD_bc%sph_bc_U, omega_sph, trans_p, gt_cor,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r)
!
      call sel_init_legendre_trans                                      &
     &   (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    sph%sph_params, sph%sph_rtm, sph%sph_rlm,                     &
     &    trans_p%leg, trans_p%idx_trns, WK_leg)
!
      if(my_rank .ne. 0) return
      tmpchara = chosen_legendre_name(WK_leg%id_legendre)
      call change_2_upper_case(tmpchara)
      write(*,'(a,i4)', advance='no')                                   &
     &       'Selected Legendre transform type: ', WK_leg%id_legendre
      write(*,'(a,a,a)') ' (', trim(tmpchara), ') '
!
      end subroutine init_leg_trans_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine find_fastest_leg_trans(sph, comms_sph,                 &
     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,          &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD,                  &
     &          cor_rlm, SR_sig, SR_r)
!
      use test_legendre_transforms
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal) :: etime_shortest
      integer(kind = kint) :: id_shortest
!
!
      if(WK_leg%id_legendre .ne. iflag_leg_compare) return
!
      etime_shortest =  1.0d30
      call test_legendre_trans(num_test, list_test,                     &
     &   sph, comms_sph, fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor, &
     &   ncomp_max_trans, nvector_max_trans, nscalar_max_trans,         &
     &   rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r,  &
     &   etime_shortest, id_shortest)
!
#ifdef BLAS
      call test_legendre_trans(ntest_blas, list_blas_test,              &
     &   sph, comms_sph, fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor, &
     &   ncomp_max_trans, nvector_max_trans, nscalar_max_trans,         &
     &   rj_fld, trns_MHD, WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r,  &
     &   etime_shortest, id_shortest)
#endif
!
      WK_leg%id_legendre = id_shortest
!
      end subroutine find_fastest_leg_trans
!
!-----------------------------------------------------------------------
!
      end module init_legendre_transform_MHD
