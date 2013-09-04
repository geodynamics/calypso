!>@file   set_coriolis_tri_sph.f90
!!@brief  module set_coriolis_tri_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Apr., 2010
!
!>@brief Evaluate coeffeicients for rotation of Coriolis term
!!       using Gaunt integrals
!!
!!@verbatim
!!      subroutine s_set_coriolis_tri_sph
!!
!!      subroutine interact_rot_coriolis_z
!!      subroutine interact_rot_coriolis_x
!!      subroutine interact_rot_coriolis_y
!!@endverbatim
!
      module set_coriolis_tri_sph
!
      use m_precision
!
      use m_constants
      use m_spherical_harmonics
      use m_integrals_4_sph_coriolis
      use m_coriolis_coefs_tri_sph
!
      implicit none
!
      private :: interact_rot_coriolis_z
      private :: interact_rot_coriolis_x, interact_rot_coriolis_y
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_coriolis_tri_sph
!
      use m_machine_parameter
      use m_sph_spectr_data
!
!
      if (iflag_debug.eq.1) write(*,*) 'interact_coriolis'
      call alloc_g0
      call interact_rot_coriolis_z
!
      if( omega_rj(1,2,1).ne.zero .or. omega_rj(1,2,3).ne.zero) then
        call alloc_g0_xy
        if (iflag_debug.eq.1) write(*,*) 'interact_rot_coriolis_x'
        call interact_rot_coriolis_x
        call interact_rot_coriolis_y
      end if
!
      end subroutine s_set_coriolis_tri_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interact_rot_coriolis_z
!
      integer(kind = kint) :: j3
!
!
!$omp parallel do private(j3)
      do j3 = 1 ,jmax_tri_sph
        sw(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                            &
     &               * gk_cor(j3,1,2) * g(j3,17)
        sw(2,1,j3) = ( two-g(j3,5)-g(j3,3) )                            &
     &               * gk_cor(j3,2,2) * g(j3,17) 
!*
        sw(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                    &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        sw(2,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                   &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        sw(1,3,j3) =  two * el_cor(j3,1,2) * g(j3,17)
        sw(2,3,j3) =  zero
!*
        tw(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )                  &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        tw(2,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )                  &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,2) * g(j3,17) * half 
!*
        tw(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                           &
     &               * gk_cor(j3,1,2) * g(j3,17)
        tw(2,4,j3) =-( -two+g(j3,5)+g(j3,3) )                           &
     &               * gk_cor(j3,2,2) * g(j3,17)
!*
        tw(1,1,j3) =-two * el_cor(j3,1,2) * g(j3,17)
        tw(2,1,j3) = zero
        tw(1,2,j3) =-g(j3,3) * el_cor(j3,1,2) * g(j3,17)
        tw(2,2,j3) = zero
!
!
        sd(1,1,j3) = g(j3,4) * gk_cor(j3,1,2) * g(j3,17)
        sd(2,1,j3) = g(j3,5) * gk_cor(j3,2,2) * g(j3,17)
!*
        sd(1,2,j3) = ( two+g(j3,4)-g(j3,3) )                            &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        sd(2,2,j3) = ( two-g(j3,5)+g(j3,3) )                            &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        td(1,j3) = el_cor(j3,1,2) * g(j3,17)
        td(2,j3) = zero
!*
!
        tr(1,j3) = -( two+g(j3,4)-g(j3,3) )                             &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        tr(2,j3) = -( two-g(j3,5)+g(j3,3) )                             &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        sr(1,j3) = el_cor(j3,1,2) * g(j3,17)
        sr(2,j3) = zero
      end do
!$omp end parallel do
!*
      end subroutine interact_rot_coriolis_z
!
! -----------------------------------------------------------------------
!
      subroutine interact_rot_coriolis_x
!
      integer(kind = kint) :: j3
!
!*  -------  lead non-liner coefs ---------
!*
!cdir select(concur)
!$omp parallel do private(j3)
      do j3 = 1 ,jmax_tri_sph
        sw1(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                           &
     &               * gk_cor(j3,1,1) * g(j3,17)
        sw1(2,1,j3) = ( two-g(j3,4)-g(j3,3) )                           &
     &               * gk_cor(j3,2,1) * g(j3,17)
        sw1(3,1,j3) = ( two-g(j3,5)-g(j3,3) )                           &
     &               * gk_cor(j3,3,1) * g(j3,17)
        sw1(4,1,j3) = ( two-g(j3,5)-g(j3,3) )                           &
     &               * gk_cor(j3,4,1) * g(j3,17)
!*
        sw1(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                   &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sw1(2,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                   &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sw1(3,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                  &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sw1(4,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                  &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        sw1(1,3,j3) =  two * el_cor(j3,1,1) * g(j3,17)
        sw1(2,3,j3) =  two * el_cor(j3,2,1) * g(j3,17)
        sw1(3,3,j3) = zero
        sw1(4,3,j3) = zero
!*
        tw1(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )                 &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        tw1(2,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )                 &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        tw1(3,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )                 &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        tw1(4,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )                 &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        tw1(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,1,1) * g(j3,17)
        tw1(2,4,j3) =-( -two+g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,2,1) * g(j3,17)
        tw1(3,4,j3) =-( -two+g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,3,1) * g(j3,17)
        tw1(4,4,j3) =-( -two+g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,4,1) * g(j3,17)
!*
        tw1(1,1,j3) =-two * el_cor(j3,1,1) * g(j3,17)
        tw1(2,1,j3) =-two * el_cor(j3,2,1) * g(j3,17)
        tw1(3,1,j3) = zero
        tw1(4,1,j3) = zero
!
        tw1(1,2,j3) =-g(j3,3) * el_cor(j3,1,1) * g(j3,17)
        tw1(2,2,j3) =-g(j3,3) * el_cor(j3,2,1) * g(j3,17)
        tw1(3,2,j3) = zero
        tw1(4,2,j3) = zero
!
!
        sd1(1,1,j3) = g(j3,4) * gk_cor(j3,1,1) * g(j3,17)
        sd1(2,1,j3) = g(j3,4) * gk_cor(j3,2,1) * g(j3,17)
        sd1(3,1,j3) = g(j3,5) * gk_cor(j3,3,1) * g(j3,17)
        sd1(4,1,j3) = g(j3,5) * gk_cor(j3,4,1) * g(j3,17)
!*
        sd1(1,2,j3) = ( two-g(j3,4)+g(j3,3) )                           &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sd1(2,2,j3) = ( two-g(j3,4)+g(j3,3) )                           &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sd1(3,2,j3) = ( two-g(j3,5)+g(j3,3) )                           &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sd1(4,2,j3) = ( two-g(j3,5)+g(j3,3) )                           &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        td1(1,j3) = el_cor(j3,1,1) * g(j3,17)
        td1(2,j3) = el_cor(j3,2,1) * g(j3,17)
        td1(3,j3) = zero
        td1(4,j3) = zero
!
!
        sr1(1,j3) = -( two-g(j3,4)+g(j3,3) )                            &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sr1(2,j3) = -( two-g(j3,4)+g(j3,3) )                            &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sr1(3,j3) = -( two-g(j3,5)+g(j3,3) )                            &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sr1(4,j3) = -( two-g(j3,5)+g(j3,3) )                            &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        tr1(1,j3) = el_cor(j3,1,1) * g(j3,17)
        tr1(2,j3) = el_cor(j3,2,1) * g(j3,17)
        tr1(3,j3) = zero
        tr1(4,j3) = zero
      end do
!$omp end parallel do
!
      end subroutine interact_rot_coriolis_x
!
! -----------------------------------------------------------------------
!
      subroutine interact_rot_coriolis_y
!
      integer(kind = kint) :: j3
!
!cdir select(concur)
!$omp parallel do private(j3)
      do j3 = 1 ,jmax_tri_sph
        sw3(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                           &
     &               * gk_cor(j3,1,3) * g(j3,17)
        sw3(2,1,j3) = ( two-g(j3,4)-g(j3,3) )                           &
     &               * gk_cor(j3,2,3) * g(j3,17)
        sw3(3,1,j3) = ( two-g(j3,5)-g(j3,3) )                           &
     &               * gk_cor(j3,3,3) * g(j3,17) 
        sw3(4,1,j3) = ( two-g(j3,5)-g(j3,3) )                           &
     &               * gk_cor(j3,4,3) * g(j3,17) 
!*
        sw3(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                   &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sw3(2,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                   &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sw3(3,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                  &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sw3(4,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                  &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        sw3(1,3,j3) =  two * el_cor(j3,1,3) * g(j3,17)
        sw3(2,3,j3) =  two * el_cor(j3,2,3) * g(j3,17)
        sw3(3,3,j3) = zero
        sw3(4,3,j3) = zero
!*
        tw3(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )                 &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        tw3(2,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )                 &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        tw3(3,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )                 &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,3,3) * g(j3,17) * half 
        tw3(4,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )                 &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,4,3) * g(j3,17) * half 
!*
        tw3(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,1,3) * g(j3,17)
        tw3(2,4,j3) =-( -two+g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,2,3) * g(j3,17)
        tw3(3,4,j3) =-( -two+g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,3,3) * g(j3,17)
        tw3(4,4,j3) =-( -two+g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,4,3) * g(j3,17)
!*
        tw3(1,1,j3) =-two * el_cor(j3,1,3) * g(j3,17)
        tw3(2,1,j3) =-two * el_cor(j3,2,3) * g(j3,17)
        tw3(3,1,j3) = zero
        tw3(4,1,j3) = zero
!
        tw3(1,2,j3) =-g(j3,4) * el_cor(j3,1,3) * g(j3,17)
        tw3(2,2,j3) =-g(j3,5) * el_cor(j3,2,3) * g(j3,17)
        tw3(3,2,j3) = zero
        tw3(4,2,j3) = zero
!
!
        sd3(1,1,j3) = g(j3,4) * gk_cor(j3,1,3) * g(j3,17)
        sd3(2,1,j3) = g(j3,4) * gk_cor(j3,2,3) * g(j3,17)
        sd3(3,1,j3) = g(j3,5) * gk_cor(j3,3,3) * g(j3,17)
        sd3(4,1,j3) = g(j3,5) * gk_cor(j3,4,3) * g(j3,17)
!*
        sd3(1,2,j3) =  ( two-g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sd3(2,2,j3) =  ( two-g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sd3(3,2,j3) =  ( two-g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sd3(4,2,j3) =  ( two-g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        td3(1,j3) = el_cor(j3,1,3) * g(j3,17)
        td3(2,j3) = el_cor(j3,2,3) * g(j3,17)
        td3(3,j3) = zero
        td3(4,j3) = zero
!
!
        sr3(1,j3) = -( two-g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sr3(2,j3) = -( two-g(j3,4)+g(j3,3) )                          &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sr3(3,j3) = -( two-g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sr3(4,j3) = -( two-g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        tr3(1,j3) = el_cor(j3,1,3) * g(j3,17)
        tr3(2,j3) = el_cor(j3,2,3) * g(j3,17)
        tr3(3,j3) = zero
        tr3(4,j3) = zero
      end do
!$omp end parallel do
!
      end subroutine interact_rot_coriolis_y
!
! -----------------------------------------------------------------------
!
      end module set_coriolis_tri_sph
