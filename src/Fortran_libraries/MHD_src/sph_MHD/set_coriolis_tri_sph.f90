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
        sw_rj(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                         &
     &               * gk_cor(j3,1,2) * g(j3,17)
        sw_rj(2,1,j3) = ( two-g(j3,5)-g(j3,3) )                         &
     &               * gk_cor(j3,2,2) * g(j3,17) 
!*
        sw_rj(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                 &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        sw_rj(2,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )                &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        sw_rj(1,3,j3) =  two * el_cor(j3,1,2) * g(j3,17)
        sw_rj(2,3,j3) =  zero
!*
        tw_rj(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )               &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        tw_rj(2,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )               &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,2) * g(j3,17) * half 
!*
        tw_rj(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                        &
     &               * gk_cor(j3,1,2) * g(j3,17)
        tw_rj(2,4,j3) =-( -two+g(j3,5)+g(j3,3) )                        &
     &               * gk_cor(j3,2,2) * g(j3,17)
!*
        tw_rj(1,1,j3) =-two * el_cor(j3,1,2) * g(j3,17)
        tw_rj(2,1,j3) = zero
        tw_rj(1,2,j3) =-g(j3,3) * el_cor(j3,1,2) * g(j3,17)
        tw_rj(2,2,j3) = zero
!
!
        sd_rj(1,1,j3) = g(j3,4) * gk_cor(j3,1,2) * g(j3,17)
        sd_rj(2,1,j3) = g(j3,5) * gk_cor(j3,2,2) * g(j3,17)
!*
        sd_rj(1,2,j3) = ( two+g(j3,4)-g(j3,3) )                         &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        sd_rj(2,2,j3) = ( two-g(j3,5)+g(j3,3) )                         &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        td_rj(1,j3) = el_cor(j3,1,2) * g(j3,17)
        td_rj(2,j3) = zero
!*
!
        tr_rj(1,j3) = -( two+g(j3,4)-g(j3,3) )                          &
     &               * gk_cor(j3,1,2) * g(j3,17) * half
        tr_rj(2,j3) = -( two-g(j3,5)+g(j3,3) )                          &
     &               * gk_cor(j3,2,2) * g(j3,17) * half
!*
        sr_rj(1,j3) = el_cor(j3,1,2) * g(j3,17)
        sr_rj(2,j3) = zero
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
!$omp parallel do private(j3)
      do j3 = 1 ,jmax_tri_sph
        sw1_rj(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                        &
     &               * gk_cor(j3,1,1) * g(j3,17)
        sw1_rj(2,1,j3) = ( two-g(j3,4)-g(j3,3) )                        &
     &               * gk_cor(j3,2,1) * g(j3,17)
        sw1_rj(3,1,j3) = ( two-g(j3,5)-g(j3,3) )                        &
     &               * gk_cor(j3,3,1) * g(j3,17)
        sw1_rj(4,1,j3) = ( two-g(j3,5)-g(j3,3) )                        &
     &               * gk_cor(j3,4,1) * g(j3,17)
!*
        sw1_rj(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sw1_rj(2,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sw1_rj(3,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )               &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sw1_rj(4,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )               &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        sw1_rj(1,3,j3) =  two * el_cor(j3,1,1) * g(j3,17)
        sw1_rj(2,3,j3) =  two * el_cor(j3,2,1) * g(j3,17)
        sw1_rj(3,3,j3) = zero
        sw1_rj(4,3,j3) = zero
!*
        tw1_rj(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )              &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        tw1_rj(2,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )              &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        tw1_rj(3,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )              &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        tw1_rj(4,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )              &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        tw1_rj(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,1,1) * g(j3,17)
        tw1_rj(2,4,j3) =-( -two+g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,2,1) * g(j3,17)
        tw1_rj(3,4,j3) =-( -two+g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,3,1) * g(j3,17)
        tw1_rj(4,4,j3) =-( -two+g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,4,1) * g(j3,17)
!*
        tw1_rj(1,1,j3) =-two * el_cor(j3,1,1) * g(j3,17)
        tw1_rj(2,1,j3) =-two * el_cor(j3,2,1) * g(j3,17)
        tw1_rj(3,1,j3) = zero
        tw1_rj(4,1,j3) = zero
!
        tw1_rj(1,2,j3) =-g(j3,3) * el_cor(j3,1,1) * g(j3,17)
        tw1_rj(2,2,j3) =-g(j3,3) * el_cor(j3,2,1) * g(j3,17)
        tw1_rj(3,2,j3) = zero
        tw1_rj(4,2,j3) = zero
!
!
        sd1_rj(1,1,j3) = g(j3,4) * gk_cor(j3,1,1) * g(j3,17)
        sd1_rj(2,1,j3) = g(j3,4) * gk_cor(j3,2,1) * g(j3,17)
        sd1_rj(3,1,j3) = g(j3,5) * gk_cor(j3,3,1) * g(j3,17)
        sd1_rj(4,1,j3) = g(j3,5) * gk_cor(j3,4,1) * g(j3,17)
!*
        sd1_rj(1,2,j3) = ( two-g(j3,4)+g(j3,3) )                        &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sd1_rj(2,2,j3) = ( two-g(j3,4)+g(j3,3) )                        &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sd1_rj(3,2,j3) = ( two-g(j3,5)+g(j3,3) )                        &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sd1_rj(4,2,j3) = ( two-g(j3,5)+g(j3,3) )                        &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        td1_rj(1,j3) = el_cor(j3,1,1) * g(j3,17)
        td1_rj(2,j3) = el_cor(j3,2,1) * g(j3,17)
        td1_rj(3,j3) = zero
        td1_rj(4,j3) = zero
!
!
        sr1_rj(1,j3) = -( two-g(j3,4)+g(j3,3) )                         &
     &               * gk_cor(j3,1,1) * g(j3,17) * half
        sr1_rj(2,j3) = -( two-g(j3,4)+g(j3,3) )                         &
     &               * gk_cor(j3,2,1) * g(j3,17) * half
        sr1_rj(3,j3) = -( two-g(j3,5)+g(j3,3) )                         &
     &               * gk_cor(j3,3,1) * g(j3,17) * half
        sr1_rj(4,j3) = -( two-g(j3,5)+g(j3,3) )                         &
     &               * gk_cor(j3,4,1) * g(j3,17) * half
!*
        tr1_rj(1,j3) = el_cor(j3,1,1) * g(j3,17)
        tr1_rj(2,j3) = el_cor(j3,2,1) * g(j3,17)
        tr1_rj(3,j3) = zero
        tr1_rj(4,j3) = zero
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
!$omp parallel do private(j3)
      do j3 = 1 ,jmax_tri_sph
        sw3_rj(1,1,j3) = ( two-g(j3,4)-g(j3,3) )                        &
     &               * gk_cor(j3,1,3) * g(j3,17)
        sw3_rj(2,1,j3) = ( two-g(j3,4)-g(j3,3) )                        &
     &               * gk_cor(j3,2,3) * g(j3,17)
        sw3_rj(3,1,j3) = ( two-g(j3,5)-g(j3,3) )                        &
     &               * gk_cor(j3,3,3) * g(j3,17) 
        sw3_rj(4,1,j3) = ( two-g(j3,5)-g(j3,3) )                        &
     &               * gk_cor(j3,4,3) * g(j3,17) 
!*
        sw3_rj(1,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sw3_rj(2,2,j3) = g(j3,4)*( two-g(j3,4)+g(j3,3) )                &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sw3_rj(3,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )               &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sw3_rj(4,2,j3) =  g(j3,5)*( two-g(j3,5)+g(j3,3) )               &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        sw3_rj(1,3,j3) =  two * el_cor(j3,1,3) * g(j3,17)
        sw3_rj(2,3,j3) =  two * el_cor(j3,2,3) * g(j3,17)
        sw3_rj(3,3,j3) = zero
        sw3_rj(4,3,j3) = zero
!*
        tw3_rj(1,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )              &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        tw3_rj(2,3,j3) =-( g(j3,3)*( two+g(j3,4)-g(j3,3) )              &
     &               + two*( -two+g(j3,4)+g(j3,3) ) )                   &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        tw3_rj(3,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )              &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,3,3) * g(j3,17) * half 
        tw3_rj(4,3,j3) =-( g(j3,3)*( two+g(j3,5)-g(j3,3) )              &
     &               + two*( -two+g(j3,5)+g(j3,3) ) )                   &
     &               * gk_cor(j3,4,3) * g(j3,17) * half 
!*
        tw3_rj(1,4,j3) =-( -two+g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,1,3) * g(j3,17)
        tw3_rj(2,4,j3) =-( -two+g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,2,3) * g(j3,17)
        tw3_rj(3,4,j3) =-( -two+g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,3,3) * g(j3,17)
        tw3_rj(4,4,j3) =-( -two+g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,4,3) * g(j3,17)
!*
        tw3_rj(1,1,j3) =-two * el_cor(j3,1,3) * g(j3,17)
        tw3_rj(2,1,j3) =-two * el_cor(j3,2,3) * g(j3,17)
        tw3_rj(3,1,j3) = zero
        tw3_rj(4,1,j3) = zero
!
        tw3_rj(1,2,j3) =-g(j3,4) * el_cor(j3,1,3) * g(j3,17)
        tw3_rj(2,2,j3) =-g(j3,5) * el_cor(j3,2,3) * g(j3,17)
        tw3_rj(3,2,j3) = zero
        tw3_rj(4,2,j3) = zero
!
!
        sd3_rj(1,1,j3) = g(j3,4) * gk_cor(j3,1,3) * g(j3,17)
        sd3_rj(2,1,j3) = g(j3,4) * gk_cor(j3,2,3) * g(j3,17)
        sd3_rj(3,1,j3) = g(j3,5) * gk_cor(j3,3,3) * g(j3,17)
        sd3_rj(4,1,j3) = g(j3,5) * gk_cor(j3,4,3) * g(j3,17)
!*
        sd3_rj(1,2,j3) =  ( two-g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sd3_rj(2,2,j3) =  ( two-g(j3,4)+g(j3,3) )                       &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sd3_rj(3,2,j3) =  ( two-g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sd3_rj(4,2,j3) =  ( two-g(j3,5)+g(j3,3) )                       &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        td3_rj(1,j3) = el_cor(j3,1,3) * g(j3,17)
        td3_rj(2,j3) = el_cor(j3,2,3) * g(j3,17)
        td3_rj(3,j3) = zero
        td3_rj(4,j3) = zero
!
!
        sr3_rj(1,j3) = -( two-g(j3,4)+g(j3,3) )                         &
     &               * gk_cor(j3,1,3) * g(j3,17) * half
        sr3_rj(2,j3) = -( two-g(j3,4)+g(j3,3) )                         &
     &               * gk_cor(j3,2,3) * g(j3,17) * half
        sr3_rj(3,j3) = -( two-g(j3,5)+g(j3,3) )                         &
     &               * gk_cor(j3,3,3) * g(j3,17) * half
        sr3_rj(4,j3) = -( two-g(j3,5)+g(j3,3) )                         &
     &               * gk_cor(j3,4,3) * g(j3,17) * half
!*
        tr3_rj(1,j3) = el_cor(j3,1,3) * g(j3,17)
        tr3_rj(2,j3) = el_cor(j3,2,3) * g(j3,17)
        tr3_rj(3,j3) = zero
        tr3_rj(4,j3) = zero
      end do
!$omp end parallel do
!
      end subroutine interact_rot_coriolis_y
!
! -----------------------------------------------------------------------
!
      end module set_coriolis_tri_sph
