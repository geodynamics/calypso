!>@file  phong_reflection.f90
!!       module phong_reflection
!!
!!@author H. Matsui
!!@date   Programmed in July. 2006
!
!> @brief Evaluate phong reflection and alpha blending
!!
!!@verbatim
!!      subroutine cal_phong_reflection(view_point_d,                   &
!!     &          num_of_lights, light_point, norm_v, k_ads,            &
!!     &          in_point, out_point, color, rgb)
!!        real(kind = kreal), intent(in) :: view_point_d(3)
!!        real(kind = kreal), intent(in) :: k_ads(3)
!!        real(kind = kreal), intent(in) :: norm_v(3)
!!        integer(kind = kint), intent(in) :: num_of_lights
!!        real(kind = kreal), intent(in) :: light_point(3,num_of_lights)
!!        real(kind = kreal), intent(in) :: in_point4(4)
!!        real(kind = kreal), intent(in) :: out_point4(4)
!!        real(kind = kreal), intent(in) :: color(3)
!!        real(kind = kreal), intent(inout)  :: rgb(3)
!!
!!      subroutine compute_opacity(transfer_function_style, opa_value,  &
!!     &          num_of_features, fea_point, value, opacity_local)
!!        integer(kind = kint), intent(in) :: transfer_function_style
!!        real(kind = kreal), intent(in) :: opa_value
!!        integer(kind = kint), intent(in) :: num_of_features
!!        real(kind = kreal), intent(in) :: fea_point(3,num_of_features)
!!        real(kind = kreal), intent(in) :: value
!!        real(kind = kreal), intent(out) :: opacity_local
!!
!!      subroutine composite_alpha_blending(rgba_src, rgba_tgt)
!!        real(kind = kreal), intent(in) :: rgba_src(4)
!!        real(kind = kreal), intent(inout) :: rgba_tgt(4)
!!      subroutine alpha_blending(rgba_src, rgba_tgt)
!!        real(kind = kreal), intent(in) :: rgba_src(4)
!!        real(kind = kreal), intent(inout) :: rgba_tgt(4)
!!@endverbatim
!
      module phong_reflection
!
      use m_precision
      use m_constants
!
      implicit  none
!
      real(kind = kreal), parameter :: EPSILON = 1.0d-9
      private :: EPSILON
!
      character(len = kchara), parameter                                &
     &                        :: hd_intensity =   'intense_chenge'
      character(len = kchara), parameter                                &
     &                        :: hd_pointlinear = 'point_linear'
      integer(kind = kint), parameter :: iflag_anbient =     1
      integer(kind = kint), parameter :: iflag_intense =     2
      integer(kind = kint), parameter :: iflag_pointlinear = 5
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_phong_reflection(view_point_d,                     &
     &          num_of_lights, light_point, norm_v, k_ads,              &
     &          in_point4, out_point4, color, rgb)
!
      real(kind = kreal), intent(in) :: view_point_d(3)
      real(kind = kreal), intent(in) :: k_ads(3)
      real(kind = kreal), intent(in) :: norm_v(3)
      integer(kind = kint), intent(in) :: num_of_lights
      real(kind = kreal), intent(in) :: light_point(3,num_of_lights)
!
      real(kind = kreal), intent(in) :: in_point4(4)
      real(kind = kreal), intent(in) :: out_point4(4)
!
      real(kind = kreal), intent(in) :: color(3)
!
      real(kind = kreal), intent(inout)  :: rgb(3)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: cosalpha, costheta
      real(kind = kreal) :: lp_norm, vp_norm, hp_norm, norm
      real(kind = kreal) :: prodLN, prodVN, prodHN
      real(kind = kreal) :: coef
      real(kind = kreal), allocatable :: lp(:), vp(:), hp(:), vo(:)
      real(kind = kreal) :: x4_mid(4)
!
      allocate(vo(4))
      allocate(lp(4), vp(4), hp(4))
!
      rgb(1:3) = zero
      x4_mid(1:4) = half*(out_point4(1:4) +  in_point4(1:4))
      vo(1:3) = view_point_d(1:3) - norm_v(1:3)
      vo(4) = 0.0d0
!
      do j = 1, num_of_lights
        lp(1:3) = light_point(1:3,j) - x4_mid(1:3)
        lp(4) = 0.0d0
        vp(1:3) = - x4_mid(1:3)
        vp(4) = 0.0d0
        hp(1:4) = (lp(1:4) + vp(1:4)) / two
!
        lp_norm = sqrt(lp(1)*lp(1) + lp(2)*lp(2)                        &
     &               + lp(3)*lp(3) + lp(4)*lp(4))
        vp_norm = sqrt(vp(1)*vp(1) + vp(2)*vp(2)                        &
     &               + vp(3)*vp(3) + vp(4)*vp(4))
        hp_norm = sqrt(hp(1)*hp(1) + hp(2)*hp(2)                        &
     &               + hp(3)*hp(3) + hp(4)*hp(4))
        norm =    sqrt(vo(1)*vo(1) + vo(2)*vo(2)                        &
     &               + vo(3)*vo(3) + vo(4)*vo(4))
!
        if(abs(lp_norm) .gt. EPSILON) lp(1:4) = lp(1:4) / lp_norm
        if(abs(vp_norm) .gt. EPSILON) vp(1:4) = vp(1:4) / vp_norm
        if(abs(hp_norm) .gt. EPSILON) hp(1:4) = hp(1:4) / hp_norm
        if(abs(norm) .gt.    EPSILON) vo(1:4) = vo(1:4) / norm
!
        prodLN = vo(1)*lp(1) + vo(2)*lp(2) + vo(3)*lp(3) + vo(4)*lp(4)
        prodVN = vo(1)*vp(1) + vo(2)*vp(2) + vo(3)*vp(3) + vo(4)*vp(4)
        prodHN = vo(1)*hp(1) + vo(2)*hp(2) + vo(3)*hp(3) + vo(4)*hp(4)
!
        cosalpha = prodLN
        costheta = prodLN*prodVN                                        &
     &            - sqrt(one-prodLN*prodLN) * sqrt(one-prodVN*prodVN)
        cosalpha = abs(cosalpha)
!
        if(cosalpha .gt. zero) then
          coef = k_ads(1) + k_ads(2)*cosalpha + k_ads(3)*costheta**6
        else
          coef = k_ads(1)
        end if
        rgb(1:3) = rgb(1:3) + color(1:3) * coef
      end do
      deallocate(vo, lp, vp, hp)
!
      end subroutine cal_phong_reflection
!
! ----------------------------------------------------------------------
!
      subroutine compute_opacity(transfer_function_style, opa_value,    &
     &          num_of_features, fea_point, value, opacity_local)
!
      integer(kind = kint), intent(in) :: transfer_function_style
      real(kind = kreal), intent(in) :: opa_value
      integer(kind = kint), intent(in) :: num_of_features
      real(kind = kreal), intent(in) :: fea_point(3,num_of_features)
      real(kind = kreal), intent(in) :: value
!
      real(kind = kreal), intent(out) :: opacity_local
!
      integer(kind = kint) :: i, min_type
      real(kind = kreal) ::  mint, t
!
!
      mint = 1.0d-17
      min_type = 1
      opacity_local = zero
      if     (transfer_function_style .eq. iflag_anbient) then
        opacity_local = opa_value
      else if(transfer_function_style .eq. iflag_pointlinear) then
        opacity_local = opa_value
        do i = 1, num_of_features-1
          if(value .le. fea_point(1,1)) then
            opacity_local = fea_point(3,1)
            exit
          else if(value .ge. fea_point(1,num_of_features)) then
            opacity_local = fea_point(3,num_of_features)
            exit
          else if(value.ge.fea_point(1,i)                               &
     &         .and. value.le.fea_point(1,i+1)) then
            opacity_local = fea_point(3,i)                              &
     &                     + (fea_point(3,i+1) - fea_point(3,i))        &
     &                     * (value - fea_point(1,i))                   &
     &                      / (fea_point(1,i+1) - fea_point(1,i))
            exit
          end if
        end do
      end if
!
      end subroutine compute_opacity
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine composite_alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
!   This us is backward casting!!
!
      rgba_tgt(4) = rgba_src(4) + rgba_tgt(4) * (one - rgba_src(4))
      rgba_tgt(1:3) =  rgba_src(1:3)                                    &
     &               + rgba_tgt(1:3) * (one - rgba_src(4))
!
      end subroutine composite_alpha_blending
!
! ----------------------------------------------------------------------
!
      subroutine alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
      real(kind = kreal) :: rgba_bck(4), a_rgba
!
!
      rgba_bck(1:4) = rgba_tgt(1:4)
!
      rgba_tgt(4) = rgba_src(4) + rgba_bck(4) * (one - rgba_src(4))
      if(rgba_tgt(4) .eq. zero) then
        rgba_tgt(1:3) = zero
      else
        a_rgba = one / rgba_tgt(4)
        rgba_tgt(1:3) =  rgba_src(1:3) * (rgba_src(4)*a_rgba)           &
     &                 + rgba_bck(1:3) * (rgba_bck(4)*a_rgba)           &
     &                  * (one - rgba_src(4))
      end if
!
      end subroutine alpha_blending
!
! ----------------------------------------------------------------------
!
      end module phong_reflection
