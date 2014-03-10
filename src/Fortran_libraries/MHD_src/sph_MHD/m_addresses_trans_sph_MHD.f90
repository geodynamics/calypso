!>@file   m_addresses_trans_sph_MHD.f90
!!@brief  module m_addresses_trans_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_MHD
!!      subroutine check_add_trans_sph_MHD
!!@endverbatim
!
      module m_addresses_trans_sph_MHD
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      number of components
!!      for backward vector spherical harmonics transform
      integer(kind = kint) :: nvector_rj_2_rtp = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_rj_2_rtp = 0
!>      number of components
!!      for forward vector spherical harmonics transform
      integer(kind = kint) :: nvector_rtp_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_rtp_2_rj = 0
!
!>    addresses of fields for backward transform
      type(phys_address), save :: b_trns
!
!>    addresses of forces for forward transform
      type(phys_address), save :: f_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_MHD
!
      use m_control_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint) :: ncomp_fwd, ncomp_bwd
!
!
      nvector_rj_2_rtp = 0
      nscalar_rj_2_rtp = 0
      nvector_rtp_2_rj = 0
      nscalar_rtp_2_rj = 0
!
!   velocity flag
      if(iflag_t_evo_4_velo .gt. id_no_evolution                        &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 3
        b_trns%i_velo = nvector_rj_2_rtp - 2
      end if
!   vorticity flag
      if(iflag_t_evo_4_velo .gt. id_no_evolution) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 3
        b_trns%i_vort = nvector_rj_2_rtp - 2
      end if
!   magnetic field flag
      if(iflag_t_evo_4_magne .gt. id_no_evolution                       &
     &      .or. iflag_4_lorentz .gt. id_turn_OFF) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 3
        b_trns%i_magne = nvector_rj_2_rtp - 2
      end if
!   current density flag
      if(iflag_4_lorentz .gt. id_turn_OFF) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 3
        b_trns%i_current = nvector_rj_2_rtp - 2
      end if
!
!
!   temperature flag
      if(iflag_t_evo_4_temp .gt. id_no_evolution) then
        nscalar_rj_2_rtp = nscalar_rj_2_rtp + 1
        b_trns%i_temp = nscalar_rj_2_rtp
      end if
!
!   composition flag
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        nscalar_rj_2_rtp = nscalar_rj_2_rtp + 1
        b_trns%i_light = nscalar_rj_2_rtp
      end if
!
!
!   advection flag
      if(iflag_t_evo_4_velo .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 3
        f_trns%i_m_advect = nvector_rtp_2_rj - 2
!   Coriolis flag
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          nvector_rtp_2_rj = nvector_rtp_2_rj + 3
          f_trns%i_coriolis = nvector_rtp_2_rj - 2
        end if
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          nvector_rtp_2_rj =      nvector_rtp_2_rj + 3
          f_trns%i_rot_Coriolis = nvector_rtp_2_rj - 2
        end if
!   Lorentz flag
        if(iflag_4_lorentz .gt. id_turn_OFF) then
          nvector_rtp_2_rj = nvector_rtp_2_rj + 3
          f_trns%i_lorentz = nvector_rtp_2_rj - 2
        end if
      end if
!
!   induction flag
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 3
        f_trns%i_vp_induct =  nvector_rtp_2_rj - 2
      end if
!
!   heat flux flag
      if(iflag_t_evo_4_temp .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 3
        f_trns%i_h_flux = nvector_rtp_2_rj - 2
      end if
!
!   composition flux flag
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 3
        f_trns%i_c_flux = nvector_rtp_2_rj - 2
      end if
!
!   divergence of Coriolis flux flag
      if(iflag_4_coriolis .gt. id_turn_OFF) then
        nscalar_rtp_2_rj = nscalar_rtp_2_rj + 1
        f_trns%i_div_Coriolis = nscalar_rtp_2_rj
      end if
!
      nb_sph_trans = nvector_rj_2_rtp
      nb_sph_trans = max(nb_sph_trans,nscalar_rj_2_rtp)
      nb_sph_trans = max(nb_sph_trans,nvector_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nscalar_rtp_2_rj)
!
      ncomp_bwd = 3*nvector_rj_2_rtp + nscalar_rj_2_rtp
      ncomp_fwd = 3*nvector_rtp_2_rj + nscalar_rtp_2_rj
      ncomp_sph_trans = max(ncomp_bwd,ncomp_fwd)
!
      end subroutine set_addresses_trans_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine check_add_trans_sph_MHD
!
      use m_sph_phys_address
      use m_work_4_sph_trans
!
!
      write(*,*) 'nb_sph_trans    ', nb_sph_trans
      write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
!
      write(*,*) 'nvector_rj_2_rtp  ', nvector_rj_2_rtp
      if(b_trns%i_velo .gt. 0) write(*,*) 'b_trns%i_velo  ',            &
     &        b_trns%i_velo, irtp%i_velo,                               &
     &        ipol%i_velo, itor%i_velo, idpdr%i_velo
      if(b_trns%i_vort .gt. 0) write(*,*) 'b_trns%i_vort  ',            &
     &         b_trns%i_vort, irtp%i_vort,                              &
     &        ipol%i_vort, itor%i_vort, idpdr%i_vort
      if(b_trns%i_magne .gt. 0) write(*,*) 'b_trns%i_magne ',           &
     &        b_trns%i_magne, irtp%i_magne,                             &
     &        ipol%i_magne, itor%i_magne, idpdr%i_magne
      if(b_trns%i_current .gt. 0) write(*,*) 'b_trns%i_current ',       &
     &        b_trns%i_current, irtp%i_current, ipol%i_current,         &
     &        itor%i_current, idpdr%i_current
      write(*,*)
!
      write(*,*) 'nscalar_rj_2_rtp  ', nscalar_rj_2_rtp
      if(b_trns%i_temp .gt. 0) write(*,*) 'b_trns%i_temp   ',           &
     &         b_trns%i_temp, irtp%i_temp, ipol%i_temp
      if(b_trns%i_light .gt. 0) write(*,*) 'b_trns%i_light  ',          &
     &         b_trns%i_light, irtp%i_light, ipol%i_light
      write(*,*)
!
      write(*,*) 'nvector_rtp_2_rj  ', nvector_rtp_2_rj
      if(f_trns%i_m_advect .gt. 0) write(*,*) 'f_trns%i_m_advect ',     &
     &        f_trns%i_m_advect, irtp%i_m_advect, ipol%i_m_advect,      &
     &        itor%i_m_advect, idpdr%i_m_advect
      if(f_trns%i_coriolis .gt. 0) write(*,*) 'f_trns%i_coriolis  ',    &
     &        f_trns%i_coriolis, irtp%i_coriolis, ipol%i_coriolis,      &
     &        itor%i_coriolis, idpdr%i_coriolis
      if(f_trns%i_rot_Coriolis .gt. 0) write(*,*)                       &
     &       'f_trns%i_rot_Coriolis  ', f_trns%i_rot_Coriolis,          &
     &        irtp%i_rot_Coriolis, ipol%i_rot_Coriolis,                 &
     &        itor%i_rot_Coriolis, idpdr%i_rot_Coriolis
      if(f_trns%i_lorentz .gt. 0) write(*,*) 'f_trns%i_lorentz  ',      &
     &        f_trns%i_lorentz, irtp%i_lorentz, ipol%i_lorentz,         &
     &        itor%i_lorentz, idpdr%i_lorentz
      if(f_trns%i_vp_induct .gt. 0) write(*,*) 'f_trns%i_vp_induct ',   &
     &        f_trns%i_vp_induct, irtp%i_vp_induct, ipol%i_vp_induct,   &
     &        itor%i_vp_induct, idpdr%i_vp_induct
      if(f_trns%i_h_flux .gt. 0) write(*,*) 'f_trns%i_h_flux',          &
     &        f_trns%i_h_flux, irtp%i_h_flux, ipol%i_h_flux,            &
     &        itor%i_h_flux, idpdr%i_h_flux
      if(f_trns%i_c_flux .gt. 0) write(*,*) 'f_trns%i_c_flux',          &
     &        f_trns%i_c_flux, irtp%i_c_flux,                           &
     &        ipol%i_c_flux, itor%i_c_flux, idpdr%i_c_flux
!
      write(*,*) 'nscalar_rtp_2_rj  ', nscalar_rtp_2_rj
      if(f_trns%i_div_Coriolis .gt. 0) write(*,*)                       &
     &       'f_trns%i_div_Coriolis  ', f_trns%i_div_Coriolis,          &
     &        irtp%i_div_Coriolis, ipol%i_div_Coriolis
      write(*,*)
!
      end subroutine check_add_trans_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_MHD
