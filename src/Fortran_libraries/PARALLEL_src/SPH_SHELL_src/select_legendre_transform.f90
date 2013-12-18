!>@file   select_legendre_transform.f90
!!@brief  module select_legendre_transform
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Select Legendre transform loops
!!
!!@verbatim
!!      subroutine sel_legendre_trans_init
!!      subroutine sel_legendre_trans_finalize
!!
!!      subroutine sel_bwd_leg_trans_vector(ncomp, nvector)
!!      subroutine sel_bwd_leg_trans_scalar(ncomp, nvector, nscalar)
!!
!!      subroutine sel_fwd_leg_trans_vector(ncomp, nvector)
!!      subroutine sel_fwd_leg_trans_scalar(ncomp, nvector, nscalar)
!!
!!   input /outpt arrays for single field
!!
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(3*i_rtp  )
!!
!!     forward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!     backward transform: 
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!   input /outpt arrays for single field
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(3*i_rtp  )
!!      Scalar spectr:         sp_rj(i_rj)
!!@endverbatim
!!
!!@n @param  nvector  number of fields to be transformed
!
      module select_legendre_transform
!
      use m_precision
!
      use calypso_mpi
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_legendre_trans_init
!
      use m_work_4_sph_trans_krin
      use m_work_4_sph_trans_spin
      use m_work_4_sph_trans_lgloop
      use m_work_4_sph_trans_fldout
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call allocate_work_sph_trans_spin(nb_sph_trans)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call allocate_work_sph_trans_krin(nb_sph_trans)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        call allocate_work_sph_trans_long(ncomp_sph_trans)
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        call allocate_work_sph_trans_fout(ncomp_sph_trans)
      end if
!
      end subroutine sel_legendre_trans_init
!
! -----------------------------------------------------------------------
!
      subroutine sel_legendre_trans_finalize
!
      use m_work_4_sph_trans_krin
      use m_work_4_sph_trans_spin
      use m_work_4_sph_trans_lgloop
      use m_work_4_sph_trans_fldout
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call deallocate_work_sph_trans_spin
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call deallocate_work_sph_trans_krin
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        call deallocate_work_sph_trans_long
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        call deallocate_work_sph_trans_fout
      end if
!
      end subroutine sel_legendre_trans_finalize
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bwd_leg_trans_vector(ncomp, nvector)
!
      use m_machine_parameter
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_lgloop
      use legendre_transform_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_bwd_trans_vector_spin'
        call leg_bwd_trans_vector_spin(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_bwd_trans_vector_krin'
        call leg_bwd_trans_vector_krin(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_bwd_trans_vector_long'
        call leg_bwd_trans_vector_long(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_bwd_trans_vector_fdout'
        call leg_bwd_trans_vector_fdout(ncomp, nvector)
      else
        if(iflag_debug .gt. 0) write(*,*) 'leg_bwd_trans_vector_org'
        call leg_bwd_trans_vector_org(ncomp, nvector)
      end if
!
      end subroutine sel_bwd_leg_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine sel_bwd_leg_trans_scalar(ncomp, nvector, nscalar)
!
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_lgloop
      use legendre_transform_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_bwd_trans_scalar_spin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_bwd_trans_scalar_krin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        call leg_bwd_trans_scalar_long(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        call leg_bwd_trans_scalar_fdout(ncomp, nvector, nscalar)
      else
        call leg_bwd_trans_scalar_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine sel_bwd_leg_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_fwd_leg_trans_vector(ncomp, nvector)
!
      use m_machine_parameter
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_lgloop
      use legendre_transform_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_fwd_trans_vector_spin'
        call leg_fwd_trans_vector_spin(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_fwd_trans_vector_krin'
        call leg_fwd_trans_vector_krin(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_fwd_trans_vector_long'
        call leg_fwd_trans_vector_long(ncomp, nvector)
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'leg_fwd_trans_vector_fdout'
        call leg_fwd_trans_vector_fdout(ncomp, nvector)
      else
        if(iflag_debug .gt. 0) write(*,*) 'leg_fwd_trans_vector_org'
        call leg_fwd_trans_vector_org(ncomp, nvector)
      end if
!
      end subroutine sel_fwd_leg_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine sel_fwd_leg_trans_scalar(ncomp, nvector, nscalar)
!
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_lgloop
      use legendre_transform_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_fwd_trans_scalar_spin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_fwd_trans_scalar_krin(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_long) then
        call leg_fwd_trans_scalar_long(ncomp, nvector, nscalar)
      else if(id_legendre_transfer .eq. iflag_leg_fldloop_outer) then
        call leg_fwd_trans_scalar_fdout(ncomp, nvector, nscalar)
      else
        call leg_fwd_trans_scalar_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine sel_fwd_leg_trans_scalar
!
! -----------------------------------------------------------------------
!
      end module select_legendre_transform
