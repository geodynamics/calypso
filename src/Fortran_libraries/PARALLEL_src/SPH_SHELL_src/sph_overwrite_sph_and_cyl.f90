!>@file   sph_overwrite_sph_and_cyl.f90
!!@brief  module sph_overwrite_sph_and_cyl
!!
!!@author H. Matsui
!!@date Programmed in July, 2013
!
!>@brief Transfer vector and tensor between spherical and cylindrial
!!       coordinates
!!
!!@verbatim
!!      subroutine sph_overwrte_sph_vect_to_cyl(ntot_comp, i_fld, d_rtp)
!!      subroutine sph_overwrte_cyl_vect_to_sph(ntot_comp, i_fld, d_rtp)
!!
!!      subroutine sph_overwrte_sph_tsr_to_cyl(ntot_comp, i_fld, d_rtp)
!!      subroutine sph_overwrte_cyl_tsr_to_sph(ntot_comp, i_fld, d_rtp)
!!@endverbatim
!!
!!@n @param  numdir     Number of component of field
!!@n @param  irtp_fld   Start address for field @f$ f(\r,\theta\phi) @f$
!
      module sph_overwrite_sph_and_cyl
!
      use m_precision
      use m_constants
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine sph_overwrte_sph_vect_to_cyl(ntot_comp, i_fld, d_rtp)
!
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,ntot_comp)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: vr, vt, vp
!
!
!$omp parallel do private(kr,lt,inod,vr,vt,vp)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1) * nidx_rtp(1)                            &
     &              + (mp-1)  * nidx_rtp(1)*nidx_rtp(2)
            vr = d_rtp(inod,i_fld  )
            vt = d_rtp(inod,i_fld+1)
            vp = d_rtp(inod,i_fld+2)
!
            d_rtp(inod,i_fld  ) =  vr * sin_theta_1d_rtp(lt)            &
     &                           + vt * cos_theta_1d_rtp(lt)
            d_rtp(inod,i_fld+1) =  vp
            d_rtp(inod,i_fld+2) =  vr * cos_theta_1d_rtp(lt)            &
     &                           - vt * sin_theta_1d_rtp(lt)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_overwrte_sph_vect_to_cyl
!
! -------------------------------------------------------------------
!
      subroutine sph_overwrte_cyl_vect_to_sph(ntot_comp, i_fld, d_rtp)
!
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,ntot_comp)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: vs, vp, vz
!
!
!$omp parallel do private(kr,lt,inod,vs,vp,vz)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1) * nidx_rtp(1)                            &
     &                + (mp-1)  * nidx_rtp(1)*nidx_rtp(2)
            vs = d_rtp(inod,i_fld  )
            vp = d_rtp(inod,i_fld+1)
            vz = d_rtp(inod,i_fld+2)
!
            d_rtp(inod,i_fld  ) =  vs * sin_theta_1d_rtp(lt)            &
     &                           + vz * cos_theta_1d_rtp(lt)
            d_rtp(inod,i_fld+1) =  vs * cos_theta_1d_rtp(lt)            &
     &                           - vz * sin_theta_1d_rtp(lt)
            d_rtp(inod,i_fld+2) =  vp
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_overwrte_cyl_vect_to_sph
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine sph_overwrte_sph_tsr_to_cyl(ntot_comp, i_fld, d_rtp)
!
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,ntot_comp)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(kr,lt,inod,trr,trt,trp,ttt,ttp,tpp)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1) * nidx_rtp(1)                            &
     &              + (mp-1)  * nidx_rtp(1)*nidx_rtp(2)
             trr = d_rtp(inod,i_fld  )
             trt = d_rtp(inod,i_fld+1)
             trp = d_rtp(inod,i_fld+2)
             ttt = d_rtp(inod,i_fld+3)
             ttp = d_rtp(inod,i_fld+4)
             tpp = d_rtp(inod,i_fld+5)
!
             d_rtp(inod,i_fld  )                                        &
     &          =    trr * sin_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         + two*trt * cos_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         +     ttt * cos_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+2)                                        &
     &           =   trp * sin_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         +     ttp * cos_theta_1d_rtp(lt) 
!
             d_rtp(inod,i_fld+1)                                        &
     &           =   trr * cos_theta_1d_rtp(lt) *sin_theta_1d_rtp(lt)   &
     &         +     trt * (cos_theta_1d_rtp(lt)*cos_theta_1d_rtp(lt)   &
     &                    - sin_theta_1d_rtp(lt)*sin_theta_1d_rtp(lt))  &
     &         -     ttt * sin_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+3) = tpp
!
             d_rtp(inod,i_fld+4)                                        &
     &           =   trp * cos_theta_1d_rtp(lt)                         &
     &         -     ttp * sin_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+5)                                        &
     &           =   trr * cos_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)  &
     &         - two*trt * cos_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         +     ttt * sin_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_overwrte_sph_tsr_to_cyl
!
! -------------------------------------------------------------------
!
      subroutine sph_overwrte_cyl_tsr_to_sph(ntot_comp, i_fld, d_rtp)
!
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real(kind = kreal), intent(inout) :: d_rtp(nnod_rtp,ntot_comp)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: tss, tsp, tsz, tpp, tpz, tzz
!
!
!$omp parallel do private(kr,lt,inod,tss,tsp,tsz,tpp,tpz,tzz)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1) * nidx_rtp(1)                            &
     &              + (mp-1)  * nidx_rtp(1)*nidx_rtp(2)
             tss = d_rtp(inod,i_fld  )
             tsp = d_rtp(inod,i_fld+1)
             tsz = d_rtp(inod,i_fld+2)
             tpp = d_rtp(inod,i_fld+3)
             tpz = d_rtp(inod,i_fld+4)
             tzz = d_rtp(inod,i_fld+5)
!
             d_rtp(inod,i_fld  )                                        &
     &          =    tss * sin_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         + two*tsz * sin_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)  &
     &         +     tzz * cos_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+1)                                        &
     &          =    tss * sin_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)  &
     &         +     tsz * ( cos_theta_1d_rtp(lt)*cos_theta_1d_rtp(lt)  &
     &                     - sin_theta_1d_rtp(lt)*sin_theta_1d_rtp(lt)) &
     &         -     tzz * cos_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+2)                                        &
     &          =    tsp * sin_theta_1d_rtp(lt)                         &
     &         +     tpz * cos_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+3)                                        &
     &          =    tss * cos_theta_1d_rtp(lt) * cos_theta_1d_rtp(lt)  &
     &         - two*tsz * cos_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)  &
     &         +     tzz * sin_theta_1d_rtp(lt) * sin_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+4)                                        &
     &          =    tsp * cos_theta_1d_rtp(lt)                         &
     &         -     tpz * sin_theta_1d_rtp(lt)
!
             d_rtp(inod,i_fld+5) = tpp
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_overwrte_cyl_tsr_to_sph
!
! -------------------------------------------------------------------
!
      end module sph_overwrite_sph_and_cyl
