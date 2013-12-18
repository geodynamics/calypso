!>@file   m_addresses_trans_sph_snap.f90
!!@brief  module m_addresses_trans_sph_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_snapshot_trans
!!      subroutine check_addresses_snapshot_trans
!!@endverbatim
!
      module m_addresses_trans_sph_snap
!
      use m_precision
!
      use m_phys_constants
      use t_phys_address
!
      implicit none
!
!>      number of vectors for backward spherical harmonics transform
      integer(kind = kint) :: nvector_snap_rj_2_rtp = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_snap_rj_2_rtp = 0
!>      number of tensors for backward spherical harmonics transform
      integer(kind = kint) :: ntensor_snap_rj_2_rtp = 0
!
!>      number of vectors for forward spherical harmonics transform
      integer(kind = kint) :: nvector_snap_rtp_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_snap_rtp_2_rj = 0
!>      number of tensors for forward spherical harmonics transform
      integer(kind = kint) :: ntensor_snap_rtp_2_rj = 0
!
!>      number of components for backward spherical harmonics transform
      integer(kind = kint) :: ncomp_snap_rj_2_rtp = 0
!>      number of components for forward spherical harmonics transform
      integer(kind = kint) :: ncomp_snap_rtp_2_rj = 0
!
!>    addresses for fields to backward transform
      type(phys_address), save :: bsnap_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: fsnap_trns
!
      private :: add_snap_transform_flag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      ncomp_snap_rj_2_rtp = 0
      ncomp_snap_rtp_2_rj = 0
      call vector_address_sph_snap_trans
      call scalar_address_sph_snap_trans
!
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_snap_rj_2_rtp)
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_snap_rtp_2_rj)
!
      nb_sph_trans = max(nb_sph_trans,nscalar_snap_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nvector_snap_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nscalar_snap_rj_2_rtp)
      nb_sph_trans = max(nb_sph_trans,nvector_snap_rj_2_rtp)
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine vector_address_sph_snap_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      nvector_snap_rtp_2_rj = 0
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_coriolis, irtp%i_coriolis,                   &
     &    nvector_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_coriolis)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_electric, irtp%i_electric,                   &
     &    nvector_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_electric)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_poynting, irtp%i_poynting,                   &
     &    nvector_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_poynting)
!
!
      nvector_snap_rj_2_rtp = 0
      if(b_trns%i_velo .eq. 0 .or. ipol%i_coriolis .gt. 0) then
        call add_snap_transform_flag                                    &
     &     (n_vector, ipol%i_velo, irtp%i_velo,                         &
     &      nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_velo)
      end if
      if(b_trns%i_vort .eq. 0) then
        call add_snap_transform_flag                                    &
     &     (n_vector, ipol%i_vort, irtp%i_vort,                         &
     &      nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_vort)
      end if
      if(b_trns%i_magne .eq. 0) then
        call add_snap_transform_flag                                    &
     &     (n_vector, ipol%i_magne, irtp%i_magne,                       &
     &      nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_magne)
      end if
      if(b_trns%i_current .eq. 0) then
        call add_snap_transform_flag                                    &
     &     (n_vector, ipol%i_current, irtp%i_current,                   &
     &      nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_current)
      end if
!
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_v_diffuse, irtp%i_v_diffuse,                 &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_v_diffuse)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_w_diffuse, irtp%i_w_diffuse,                 &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_w_diffuse)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_vp_diffuse, irtp%i_vp_diffuse,               &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_vp_diffuse)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_b_diffuse, irtp%i_b_diffuse,                 &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_b_diffuse)
!
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_rot_inertia, irtp%i_rot_inertia,             &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_rot_inertia)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_rot_Coriolis, irtp%i_rot_Coriolis,           &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_rot_Coriolis)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_rot_Lorentz, irtp%i_rot_Lorentz,             &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_rot_Lorentz)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_rot_buoyancy, irtp%i_rot_buoyancy,           &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_rot_buoyancy)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_rot_comp_buo, irtp%i_rot_comp_buo,           &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_rot_comp_buo)
!
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_press_grad, irtp%i_press_grad,               &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_press_grad)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_induction, irtp%i_induction,                 &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_induction)
!
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_grad_t, irtp%i_grad_t,                       &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_grad_t)
      call add_snap_transform_flag                                      &
     &   (n_vector, ipol%i_grad_composit, irtp%i_grad_composit,         &
     &    nvector_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_grad_composit)
!
      end subroutine vector_address_sph_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine scalar_address_sph_snap_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      nscalar_snap_rtp_2_rj = 0
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_me_gen, irtp%i_me_gen,                       &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_me_gen)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_ujb, irtp%i_ujb,                             &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_ujb)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_nega_ujb, irtp%i_nega_ujb,                   &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_nega_ujb)
!
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_buo_gen, irtp%i_buo_gen,                     &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_buo_gen)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_c_buo_gen, irtp%i_c_buo_gen,                 &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_c_buo_gen)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_f_buo_gen, irtp%i_f_buo_gen,                 &
     &    nscalar_snap_rtp_2_rj, ncomp_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_f_buo_gen)
!
!
!
      nscalar_snap_rj_2_rtp = 0
      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_snap_transform_flag                                    &
     &     (n_scalar, ipol%i_temp, irtp%i_temp,                         &
     &      nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_temp)
      end if
      if(b_trns%i_light .eq. 0) then
        call add_snap_transform_flag                                    &
     &     (n_scalar, ipol%i_light, irtp%i_light,                       &
     &      nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                 &
     &      bsnap_trns%i_light)
      end if
!
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_press, irtp%i_press,                         &
     &    nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_press)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_par_temp, irtp%i_par_temp,                   &
     &    nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_par_temp)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_t_diffuse, irtp%i_t_diffuse,                 &
     &    nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_t_diffuse)
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_c_diffuse, irtp%i_c_diffuse,                 &
     &    nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_c_diffuse)
!
      call add_snap_transform_flag                                      &
     &   (n_scalar, ipol%i_div_Coriolis, irtp%i_div_Coriolis,           &
     &    nscalar_snap_rj_2_rtp, ncomp_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_div_Coriolis)
!
      end subroutine scalar_address_sph_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_snap_transform_flag(ncomp_field, is_fld, irtp_fld, &
     &          num_trans, ncomp_trans, itrans)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld, ncomp_field
      integer(kind = kint), intent(inout) :: num_trans, ncomp_trans
      integer(kind = kint), intent(inout) :: itrans
!
      integer(kind = kint) :: iflag
!
!
      iflag = is_fld * irtp_fld
      call add_vector_transform_flag                                    &
     &   (ncomp_field, iflag, num_trans, ncomp_trans, itrans)
!
      end subroutine add_snap_transform_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      write(*,*) 'nvector_snap_rj_2_rtp', nvector_snap_rj_2_rtp
      if(bsnap_trns%i_velo .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_velo', bsnap_trns%i_velo,               &
     &            ipol%i_velo, irtp%i_velo
      if(bsnap_trns%i_vort .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_vort', bsnap_trns%i_vort,               &
     &            ipol%i_vort, irtp%i_vort
      if(bsnap_trns%i_magne .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_magne', bsnap_trns%i_magne,             &
     &            ipol%i_magne, irtp%i_magne
      if(bsnap_trns%i_current .gt. 0) write(*,*)                        &
     &            'bsnap_trns%i_current', bsnap_trns%i_current,         &
     &            ipol%i_current, irtp%i_current
!
      if(bsnap_trns%i_v_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_v_diffuse', bsnap_trns%i_v_diffuse,     &
     &            ipol%i_v_diffuse, irtp%i_v_diffuse
      if(bsnap_trns%i_w_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_w_diffuse', bsnap_trns%i_w_diffuse,     &
     &            ipol%i_w_diffuse, irtp%i_w_diffuse
      if(bsnap_trns%i_vp_diffuse .gt. 0) write(*,*)                     &
     &            'bsnap_trns%i_vp_diffuse', bsnap_trns%i_vp_diffuse,   &
     &            ipol%i_vp_diffuse, irtp%i_vp_diffuse
      if(bsnap_trns%i_b_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_b_diffuse', bsnap_trns%i_b_diffuse,     &
     &            ipol%i_b_diffuse, irtp%i_b_diffuse
!
      if(bsnap_trns%i_rot_Coriolis .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_Coriolis', bsnap_trns%i_rot_Coriolis,  &
     &          ipol%i_rot_Coriolis, irtp%i_rot_Coriolis
      if(bsnap_trns%i_rot_Lorentz .gt. 0) write(*,*)                    &
     &         'bsnap_trns%i_rot_Lorentz',  bsnap_trns%i_rot_Lorentz,   &
     &          ipol%i_rot_Lorentz, irtp%i_rot_Lorentz
      if(bsnap_trns%i_rot_buoyancy .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_buoyancy',  bsnap_trns%i_rot_buoyancy, &
     &          ipol%i_rot_buoyancy, irtp%i_rot_buoyancy
      if(bsnap_trns%i_rot_comp_buo .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_comp_buo',  bsnap_trns%i_rot_comp_buo, &
     &          ipol%i_rot_comp_buo, irtp%i_rot_comp_buo
!
      if(bsnap_trns%i_press_grad .gt. 0) write(*,*)                     &
     &            'bsnap_trns%i_press_grad', bsnap_trns%i_press_grad,   &
     &             ipol%i_press_grad, irtp%i_press_grad
      if(bsnap_trns%i_induction .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_induction', bsnap_trns%i_induction,     &
     &            ipol%i_induction, irtp%i_induction
!
      if(bsnap_trns%i_grad_t .gt. 0) write(*,*)                         &
     &            'bsnap_trns%i_grad_t',    bsnap_trns%i_grad_t,        &
     &            ipol%i_grad_t, irtp%i_grad_t
      if(bsnap_trns%i_grad_composit .gt. 0) write(*,*)                  &
     &            'bsnap_trns%i_grad_composit',                         &
     &            bsnap_trns%i_grad_composit,                           &
     &            ipol%i_grad_composit, irtp%i_grad_composit
      write(*,*)
!
      write(*,*) 'nscalar_snap_rj_2_rtp', nscalar_snap_rj_2_rtp
      if(bsnap_trns%i_temp .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_temp', bsnap_trns%i_temp,               &
     &            ipol%i_temp, irtp%i_temp
      if(bsnap_trns%i_light .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_light', bsnap_trns%i_light,             &
     &            ipol%i_light, irtp%i_light
      if(bsnap_trns%i_press .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_press', bsnap_trns%i_press,             &
     &            ipol%i_press, irtp%i_press
      if(bsnap_trns%i_par_temp .gt. 0) write(*,*)                       &
     &            'bsnap_trns%i_par_temp', bsnap_trns%i_par_temp,       &
     &            ipol%i_par_temp, irtp%i_par_temp
!
      if(bsnap_trns%i_t_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_t_diffuse', bsnap_trns%i_t_diffuse,     &
     &            ipol%i_t_diffuse, irtp%i_t_diffuse
      if(bsnap_trns%i_c_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_c_diffuse', bsnap_trns%i_c_diffuse,     &
     &            ipol%i_c_diffuse, irtp%i_c_diffuse
      write(*,*)
!
!
      write(*,*) 'nvector_snap_rtp_2_rj', nvector_snap_rtp_2_rj
      if(fsnap_trns%i_coriolis .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_coriolis',  fsnap_trns%i_coriolis,      &
     &            ipol%i_coriolis, irtp%i_coriolis
!
      if(fsnap_trns%i_electric .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_electric',  fsnap_trns%i_electric,      &
     &            ipol%i_electric, irtp%i_electric
      if(fsnap_trns%i_poynting .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_poynting',  fsnap_trns%i_poynting,      &
     &            ipol%i_poynting, irtp%i_poynting
      write(*,*)
!
      write(*,*) 'nscalar_snap_rtp_2_rj', nscalar_snap_rtp_2_rj
      if(fsnap_trns%i_me_gen .gt. 0) write(*,*)                         &
     &            'fsnap_trns%i_me_gen', fsnap_trns%i_me_gen,           &
     &            ipol%i_me_gen, irtp%i_me_gen
      if(fsnap_trns%i_ujb .gt. 0) write(*,*)                            &
     &            'fsnap_trns%i_ujb', fsnap_trns%i_ujb,                 &
     &            ipol%i_ujb, irtp%i_ujb
      if(fsnap_trns%i_nega_ujb .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_nega_ujb',  fsnap_trns%i_nega_ujb,      &
     &            ipol%i_nega_ujb, irtp%i_nega_ujb
!
      if(fsnap_trns%i_buo_gen .gt. 0) write(*,*)                        &
     &            'fsnap_trns%i_buo_gen',   fsnap_trns%i_buo_gen,       &
     &            ipol%i_buo_gen, irtp%i_buo_gen
      if(fsnap_trns%i_c_buo_gen .gt. 0) write(*,*)                      &
     &            'fsnap_trns%i_c_buo_gen', fsnap_trns%i_c_buo_gen,     &
     &            ipol%i_c_buo_gen, irtp%i_c_buo_gen
      if(fsnap_trns%i_f_buo_gen .gt. 0) write(*,*)                      &
     &            'fsnap_trns%i_f_buo_gen', fsnap_trns%i_f_buo_gen,     &
     &            ipol%i_f_buo_gen, irtp%i_f_buo_gen
        write(*,*)
!
      end subroutine check_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_snap
