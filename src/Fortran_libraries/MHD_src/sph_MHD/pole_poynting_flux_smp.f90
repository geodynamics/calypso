!> @file  pole_poynting_flux_smp.f90
!!      module pole_poynting_flux_smp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate products at poles
!!
!!@verbatim
!!      subroutine cal_pole_electric_field_smp(numnod, internal_node,   &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef_d, ncomp_nod,          &
!!     &          i_curent, i_uxb, i_electric, d_nod)
!!      subroutine cal_pole_poynting_flux_smp(numnod, internal_node,    &
!!     &          xx, nnod_rtp, nidx_rtp_r, coef_d, ncomp_nod,          &
!!     &          i_curent, i_uxb, i_magne, i_poynting, d_nod)
!!@endverbatim
!!
!!@n @param numnod               number of nodes
!!@n @param internal_node        number of internal nodes
!!@n @param xx                   position
!!@n @param nnod_rtp             number of grid points
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param nidx_rtp_r           number of radial points 
!!                               for @f$ f(r,\theta,\phi) @f$
!!@n @param coef_d        Coefficient for magnetic diffusion
!!
!!@n @param  i_magne   Magnetic field @f$ B_{i} @f$
!!@n @param  i_curent  Current density
!!                                 @f$e_{ijk) \partial_{j} B_{k})@f$
!!@n @param  i_uxb      Induction @f$(e_{ijk) u{j} B_{k})@f$
!!@n @param  i_electric  Electric field
!!              @f$ E_{i} = \sigma^{-1} J_{i} - (e_{ijk) u{j} B_{k})@f$
!!@n @param  i_poynting          Poynting flux @f$(e_{ijk) E{j} B_{k})@f$
!
      module pole_poynting_flux_smp
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_pole_electric_field_smp(numnod, internal_node,     &
     &          xx, nnod_rtp, nidx_rtp_r, coef_d, ncomp_nod,            &
     &          i_curent, i_uxb, i_electric, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: ncomp_nod, i_electric
      integer(kind = kint), intent(in) :: i_uxb, i_curent
      real(kind = kreal), intent(inout) :: d_nod(numnod, ncomp_nod)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_nod(inod,i_electric  ) = coef_d * d_nod(inod,i_curent  )    &
     &                              - d_nod(inod,i_uxb  )
          d_nod(inod,i_electric+1) = coef_d * d_nod(inod,i_curent+1)    &
     &                              - d_nod(inod,i_uxb+1)
          d_nod(inod,i_electric+2) = coef_d * d_nod(inod,i_curent+2)    &
     &                              - d_nod(inod,i_uxb+2)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_nod(inod,i_electric  ) = coef_d * d_nod(inod,i_curent  )    &
     &                              - d_nod(inod,i_uxb  )
          d_nod(inod,i_electric+1) = coef_d * d_nod(inod,i_curent+1)    &
     &                              - d_nod(inod,i_uxb+1)
          d_nod(inod,i_electric+2) = coef_d * d_nod(inod,i_curent+2)    &
     &                              - d_nod(inod,i_uxb+2)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_nod(inod,i_electric  ) = coef_d * d_nod(inod,i_curent  )        &
     &                          - d_nod(inod,i_uxb  )
      d_nod(inod,i_electric+1) = coef_d * d_nod(inod,i_curent+1)        &
     &                          - d_nod(inod,i_uxb+1)
      d_nod(inod,i_electric+2) = coef_d * d_nod(inod,i_curent+2)        &
     &                          - d_nod(inod,i_uxb+2)
!
      end subroutine cal_pole_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pole_poynting_flux_smp(numnod, internal_node,      &
     &          xx, nnod_rtp, nidx_rtp_r, coef_d, ncomp_nod,            &
     &          i_curent, i_uxb, i_magne, i_poynting, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef_d
      integer(kind = kint), intent(in) :: ncomp_nod, i_poynting
      integer(kind = kint), intent(in) :: i_uxb, i_magne, i_curent
      real(kind = kreal), intent(inout) :: d_nod(numnod, ncomp_nod)
!
      integer(kind = kint) :: inod, kr
      real (kind=kreal) :: e_fld(3)
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_fld(1) = coef_d * d_nod(inod,i_curent  )                    &
     &              - d_nod(inod,i_uxb  )
          e_fld(2) = coef_d * d_nod(inod,i_curent+1)                    &
     &              - d_nod(inod,i_uxb+1)
          e_fld(3) = coef_d * d_nod(inod,i_curent+2)                    &
     &              - d_nod(inod,i_uxb+2)
!
          d_nod(inod,i_poynting  ) = e_fld(2)*d_nod(inod,i_magne+2)     &
     &                              - e_fld(3)*d_nod(inod,i_magne+1)
          d_nod(inod,i_poynting+1) = e_fld(3)*d_nod(inod,i_magne  )     &
     &                              - e_fld(1)*d_nod(inod,i_magne+2)
          d_nod(inod,i_poynting+2) = e_fld(1)*d_nod(inod,i_magne+1)     &
     &                              - e_fld(2)*d_nod(inod,i_magne  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          e_fld(1) = coef_d * d_nod(inod,i_curent  )                    &
     &              - d_nod(inod,i_uxb  )
          e_fld(2) = coef_d * d_nod(inod,i_curent+1)                    &
     &              - d_nod(inod,i_uxb+1)
          e_fld(3) = coef_d * d_nod(inod,i_curent+2)                    &
     &              - d_nod(inod,i_uxb+2)
!
          d_nod(inod,i_poynting  ) = e_fld(2)*d_nod(inod,i_magne+2)     &
     &                              - e_fld(3)*d_nod(inod,i_magne+1)
          d_nod(inod,i_poynting+1) = e_fld(3)*d_nod(inod,i_magne  )     &
     &                              - e_fld(1)*d_nod(inod,i_magne+2)
          d_nod(inod,i_poynting+2) = e_fld(1)*d_nod(inod,i_magne+1)     &
     &                              - e_fld(2)*d_nod(inod,i_magne  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      e_fld(1) = coef_d * d_nod(inod,i_curent  ) - d_nod(inod,i_uxb  )
      e_fld(2) = coef_d * d_nod(inod,i_curent+1) - d_nod(inod,i_uxb+1)
      e_fld(3) = coef_d * d_nod(inod,i_curent+2) - d_nod(inod,i_uxb+2)
!
      d_nod(inod,i_poynting  ) = e_fld(2)*d_nod(inod,i_magne+2)         &
     &                          - e_fld(3)*d_nod(inod,i_magne+1)
      d_nod(inod,i_poynting+1) = e_fld(3)*d_nod(inod,i_magne  )         &
     &                          - e_fld(1)*d_nod(inod,i_magne+2)
      d_nod(inod,i_poynting+2) = e_fld(1)*d_nod(inod,i_magne+1)         &
     &                          - e_fld(2)*d_nod(inod,i_magne  )
!
      end subroutine cal_pole_poynting_flux_smp
!
! -----------------------------------------------------------------------
!
      end module pole_poynting_flux_smp
