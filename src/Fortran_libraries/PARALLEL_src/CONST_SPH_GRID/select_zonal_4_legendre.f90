!>@file   select_zonal_4_legendre.f90
!!@brief  module select_zonal_4_legendre
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set order of spheherical harmonics modes
!!
!!@verbatim
!!      subroutine sel_zonal_ordering_4_leg_trns                        &
!!     &         (iflag_rlm_distribute, ndomain_m,                      &
!!     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!!@endverbatim
!
      module select_zonal_4_legendre
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: id_cyclic_eq_transform =   0
      integer(kind = kint), parameter :: id_cyclic_eq_mode =        1
!
      integer(kind = kint), parameter :: id_cyclic_eq_trans_neib =  2
      integer(kind = kint), parameter :: id_cyclic_eq_mode_neib =   3
!
      integer(kind = kint), parameter :: id_simple_rlm_distribute = 10
!
      integer(kind = kint), parameter :: id_V1_rlm_distribute =  100
      integer(kind = kint), parameter :: id_test_distribute = 999
!
      private :: zonal_wavenum_list_test
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_zonal_ordering_4_leg_trns                          &
     &         (iflag_rlm_distribute, ndomain_m,                        &
     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!
      use zonal_wavenumber_4_legendre
!
      integer(kind = kint), intent(in) :: iflag_rlm_distribute
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint), allocatable :: ip_tmp(:)
!
!
      allocate( ip_tmp(0:ltr) )
      ip_tmp = 0
!
      if(iflag_rlm_distribute .eq. id_cyclic_eq_mode) then
        call set_domain_by_eq_leg_modes                                 &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_back_order(ndomain_m, ltr, m_folding,        &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
      else if(iflag_rlm_distribute .eq. id_cyclic_eq_mode_neib) then
        call set_domain_by_eq_leg_modes                                 &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_neib_order(ndomain_m, ltr, m_folding,        &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      else if(iflag_rlm_distribute .eq. id_cyclic_eq_trans_neib) then
        call set_domain_by_eq_leg_trns                                  &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_neib_order(ndomain_m, ltr, m_folding,        &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      else if(iflag_rlm_distribute .eq. id_simple_rlm_distribute) then
        call set_domain_by_eq_leg_trns                                  &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_back_order(ndomain_m, ltr, m_folding,        &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      else if(iflag_rlm_distribute .eq. id_V1_rlm_distribute) then
        call set_domain_by_eq_leg_modes                                 &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_fwd_order(ndomain_m, ltr, m_folding,         &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      else if(iflag_rlm_distribute .eq. id_test_distribute) then
        call zonal_wavenum_list_test(ndomain_m, ltr, m_folding,         &
     &      nth, nph, jdx_fsph, mdx_4_lgd, ip_tmp)
!
      else
        call set_domain_by_eq_leg_trns                                  &
     &     (ndomain_m, ltr, m_folding, ip_tmp)
        call set_local_sph_back_order(ndomain_m, ltr, m_folding,        &
     &      nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
      end if
!
      deallocate( ip_tmp )
!
      end subroutine sel_zonal_ordering_4_leg_trns
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine zonal_wavenum_list_test(ndomain_m,                     &
     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd, ip_tmp)
!
      use zonal_wavenumber_4_legendre
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
      integer(kind = kint), intent(inout) :: ip_tmp(0:ltr)
!
!      integer(kind = kint) :: m, mm
!
!
      write(*,*) 'test test'
!
      call set_domain_by_eq_leg_modes                                   &
     &   (ndomain_m, ltr, m_folding, ip_tmp)
!
      call set_local_sph_fwd_order(ndomain_m, ltr, m_folding,           &
     &    nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
!        write(8,*) 'm, ip_tmp(m)'
!      do m = -ltr, ltr
!        write(8,*) m, ip_tmp(m)
!      end do
!
!        write(*,*) 'm, jdx_fsph(m)'
!      do m = -ltr, ltr
!        write(*,*) m, jdx_fsph(m)
!      end do
!        write(*,*) 'mm, mdx_4_lgd(mm)'
!      do mm = 0,nph
!        write(*,*) mm, mdx_4_lgd(mm)
!      end do
!
      end subroutine zonal_wavenum_list_test
!
! -----------------------------------------------------------------------
!
      end module select_zonal_4_legendre
