!>@file   legendre_transform_select.f90
!!@brief  module legendre_transform_select
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!
!!@verbatim
!!      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!!
!!      subroutine sel_alloc_legendre_trans(ncomp)
!!      subroutine sel_dealloc_legendre_trans
!!
!!    Backward transforms
!!      subroutine sel_backward_legendre_trans(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine sel_forward_legendre_trans(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_select
!
      use m_precision
!
      use m_work_4_sph_trans_spin
!
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_lgloop
      use legendre_transform_fdout
!
      implicit none
!
!>      Character flag to perform Legendre transform 
!@n     using original array order
      character(len = kchara), parameter                                &
     &           :: leg_orginal_loop = 'original_loop'
!>      Character flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      character(len = kchara), parameter                                &
     &           :: leg_krloop_inner = 'inner_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_krloop_outer = 'outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with longest loop
      character(len = kchara), parameter                                &
     &           :: leg_long_loop =    'long_loop'
!>      Character flag to perform Legendre transform 
!@n     with outmost field loop
      character(len = kchara), parameter                                &
     &           :: leg_fdout_loop =   'outer_field_loop'
!
!
!>      integer flag to run elpse time check for legendre transform
      integer(kind = kint), parameter :: iflag_leg_undefined = -1
!>      integer flag to perform Legendre transform 
!@n     using original array order
      integer(kind = kint), parameter :: iflag_leg_orginal_loop = 1
!>      integer flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      integer(kind = kint), parameter :: iflag_leg_krloop_inner = 2
!>      integer flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_krloop_outer = 3
!>      integer flag to perform Legendre transform 
!@n     with longest loop
      integer(kind = kint), parameter :: iflag_leg_long_loop =    4
!>      integer flag to perform Legendre transform 
!@n     with outmost field loop
      integer(kind = kint), parameter :: iflag_leg_fdout_loop =   5
!
!>      Integer flag for Legendre transform
      integer(kind = kint)                                              &
     &              :: id_legendre_transfer = iflag_leg_undefined
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: tranx_loop_ctl
!
!
      if(     cmp_no_case(tranx_loop_ctl, leg_krloop_inner).gt.0) then
        id_legendre_transfer = iflag_leg_krloop_inner
      else if(cmp_no_case(tranx_loop_ctl, leg_krloop_outer).gt.0) then
        id_legendre_transfer = iflag_leg_krloop_outer
      else if(cmp_no_case(tranx_loop_ctl, leg_long_loop).gt.0) then
        id_legendre_transfer = iflag_leg_long_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_fdout_loop).gt.0) then
        id_legendre_transfer = iflag_leg_fdout_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_orginal_loop).gt.0) then
        id_legendre_transfer = iflag_leg_orginal_loop
      end if
!
      end subroutine set_legendre_trans_mode_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_alloc_legendre_trans(ncomp)
!
      integer(kind = kint), intent(in) :: ncomp
!
!
      if    (id_legendre_transfer .eq. iflag_leg_krloop_outer           &
     &  .or. id_legendre_transfer .eq. iflag_leg_krloop_inner           &
     &  .or. id_legendre_transfer .eq. iflag_leg_fdout_loop) then
        call allocate_work_sph_trans(ncomp)
      end if
!
      end subroutine sel_alloc_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_dealloc_legendre_trans
!
!
      if    (id_legendre_transfer .eq. iflag_leg_krloop_outer           &
     &  .or. id_legendre_transfer .eq. iflag_leg_krloop_inner           &
     &  .or. id_legendre_transfer .eq. iflag_leg_fdout_loop) then
        call deallocate_work_sph_trans
      end if
!
      end subroutine sel_dealloc_legendre_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_backward_legendre_trans(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_backward_trans_spin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_bwd_trans_fields_krin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_long_loop) then
        call leg_backward_trans_long(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_fdout_loop) then
        call leg_backward_trans_fdout(ncomp, nvector, nscalar)
      else
        call leg_backward_trans_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine sel_backward_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_forward_legendre_trans(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(ncomp .le. 0) return
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_forward_trans_spin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_fwd_trans_fields_krin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_long_loop) then
        call leg_forward_trans_long(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_fdout_loop) then
        call leg_forward_trans_fdout(ncomp, nvector, nscalar)
      else
        call leg_forwawd_trans_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine sel_forward_legendre_trans
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_select
