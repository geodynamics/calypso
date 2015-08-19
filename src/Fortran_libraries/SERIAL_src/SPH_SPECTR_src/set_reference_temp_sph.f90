!>@file   set_reference_temp_sph.f90
!!@brief  module set_reference_temp_sph
!!
!!@author H. Matsui
!!@date Programmed June., 1994
!!@date Modified Apr., 2009
!
!> @brief Set diffusive temperature profile
!!@n      with fixed temperature boundary
!!
!!@verbatim
!!      subroutine set_reftemp_4_sph
!!***********************************************************************
!!*
!!*     ref_temp(k,0) : reference of temperature  (output)
!!*     ref_temp(k,1) : dT_0 / dr
!!*
!!*                          c2
!!*      ref_temp(k) = c1 + ------
!!*                          r(k)
!!*
!!*                      dto(k)
!!*     dref_temp(k) = ---------
!!*                        dr
!!*                         c2
!!*                  = - --------
!!*                        rs(k)
!!*
!!***********************************************************************
!!@endverbatim
!
      module set_reference_temp_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_reftemp_4_sph(r_ICB, r_CMB, temp_ICB, temp_CMB)
!
      use m_sph_spectr_data
      use m_sph_phys_address
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: temp_ICB, temp_CMB
!
      integer(kind = kint) :: j, k, kk, inod
      real(kind = kreal) :: c1, c2
!
!
      c1 = ( r_CMB*temp_CMB - r_ICB*temp_ICB ) /   ( r_CMB - r_ICB )
      c2 = r_CMB * r_ICB * (temp_ICB - temp_CMB) / ( r_CMB - r_ICB )
!
      d_rj(1:nnod_rj,ipol%i_ref_t) =  zero
      d_rj(1:nnod_rj,ipol%i_gref_t) = zero
!
      do j = 1, nidx_rj(2)
!
        if ( idx_gl_1d_rj_j(j,1) .eq. izero) then
          do kk = nlayer_ICB, nlayer_CMB
            k = kk - nlayer_ICB
            inod = j + (kk-1) * nidx_rj(2)
            d_rj(inod,ipol%i_ref_t) =    c1 + c2 * ar_1d_rj(kk,1)
            d_rj(inod,ipol%i_gref_t) = - c2 * ar_1d_rj(kk,2)
          end do
!
          do kk = 1 ,nlayer_ICB-1
            k = nlayer_ICB - kk
            inod = j + (kk-1) * nidx_rj(2)
            d_rj(inod,ipol%i_ref_t) =  temp_ICB
          end do
!
          exit
        end if
      end do
!
      end subroutine set_reftemp_4_sph
!
!  -------------------------------------------------------------------
!
      end module set_reference_temp_sph
