!>@file   transform_mat_operations.f90
!!        module transform_mat_operations
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on ????
!!
!>@brief Matrix operations for Affine transforms
!!
!!@verbatim
!!      subroutine Kemo_viewmatrix(mat, scale, shift, rotation, lookat)
!!      subroutine Kemo_rotaion_viewmatrix(mat, scale, shift, rotation, &
!!     &          lookat, rot_movie, iaxis_rot)
!!
!!      subroutine Kemo_Unit(mat)
!!      subroutine Kemo_Scale(mat, scale)
!!      subroutine Kemo_Translate(mat, trans)
!!      subroutine Kemo_Rotate(mat, angle_deg, axis)
!!
!!      subroutine Kemo_Translate_mat(trans_mat, trans)
!!      subroutine Kemo_Scale_mat(scale_mat, scale)
!!      subroutine Kemo_Rotate_mat(rot_mat, angle_deg, axis)
!!
!!      subroutine  cal_matmat44(S, A, B)
!!
!!      subroutine  cal_matmat44_multi_smp(N, S, A, B)
!!           N(i,j,N) = A(i,j,N)B(i,j,N)
!!      subroutine  cal_matcmat44_multi_smp(N, S, A, B)
!!           N(i,j,N) = A(i,j,N)B(i,j)
!!      subroutine  cal_cmatmat44_multi_smp(N, S, A, B)
!!           N(i,j,N) = A(i,j)B(i,j,N)
!!         (!$omp parallel need befor using)
!!@endverbatim
!
      module transform_mat_operations
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: Kemo_Unit_mat
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_viewmatrix(mat, scale, shift, rotation, lookat)
!
      real(kind = kreal), intent(in) :: scale(3), shift(3)
      real(kind = kreal), intent(in) :: lookat(3), rotation(4)
!
      real(kind = kreal), intent(inout) :: mat(4,4)
!
      real(kind = kreal) :: opp_lookat(3)
!
      opp_lookat(1:3) = -lookat(1:3)
!
      call Kemo_Unit(mat)
      call Kemo_Translate(mat, shift(1))
      call Kemo_Rotate(mat, rotation(1), rotation(2))
      call Kemo_Scale(mat, scale(1))
      call Kemo_Translate(mat, opp_lookat(1))
!
      end subroutine Kemo_viewmatrix
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_rotaion_viewmatrix(mat, scale, shift, rotation,   &
     &          lookat, rot_movie, iaxis_rot)
!
      real(kind = kreal), intent(in) :: scale(3), shift(3)
      real(kind = kreal), intent(in) :: lookat(3), rotation(4)
      real(kind = kreal), intent(in) :: rot_movie
      integer(kind = kint), intent(in) :: iaxis_rot
!
      real(kind = kreal), intent(inout) :: mat(4,4)
!
      real(kind = kreal) :: opp_lookat(3), rotation_axis(3)
!
      opp_lookat(1:3) = -lookat(1:3)
      rotation_axis(1:3) = zero
      rotation_axis(iaxis_rot) = one
!
      call Kemo_Unit(mat)
      call Kemo_Translate(mat, shift(1))
      call Kemo_Rotate(mat, rotation(1),  rotation(2))
      call Kemo_Rotate(mat, rot_movie, rotation_axis(1))
      call Kemo_Scale(mat, scale(1))
      call Kemo_Translate(mat, opp_lookat(1))
!
      end subroutine Kemo_rotaion_viewmatrix
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Unit(mat)
!
      real(kind = kreal), intent(inout) :: mat(4,4)
!
!
      call Kemo_Unit_mat(mat)
!
      end subroutine Kemo_Unit
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Scale(mat, scale)
!
      real(kind = kreal), intent(in) :: scale(3)
      real(kind = kreal), intent(inout) :: mat(4,4)
!
      real(kind = kreal) :: scale_mat(4,4), mat_tmp(4,4)
!
!
      mat_tmp(1:4,1:4) = mat(1:4,1:4)
!
      call Kemo_Scale_mat(scale_mat, scale)
      call cal_matmat44(mat(1,1), scale_mat(1,1), mat_tmp(1,1))
!
      end subroutine Kemo_Scale
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Translate(mat, trans)
!
      real(kind = kreal), intent(in) :: trans(3)
      real(kind = kreal), intent(inout) :: mat(4,4)
!
      real(kind = kreal) :: trans_mat(4,4), mat_tmp(4,4)
!
!
      mat_tmp(1:4,1:4) = mat(1:4,1:4)
!
      call Kemo_Translate_mat(trans_mat, trans)
      call cal_matmat44(mat(1,1), trans_mat(1,1), mat_tmp(1,1))
!
      end subroutine Kemo_Translate
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Rotate(mat, angle_deg, axis)
!
      real(kind = kreal), intent(in) :: angle_deg
      real(kind = kreal), intent(in) :: axis(3)
      real(kind = kreal), intent(inout) :: mat(4,4)
!
      real(kind = kreal) :: rot_mat(4,4), mat_tmp(4,4)
!
!
      mat_tmp(1:4,1:4) = mat(1:4,1:4)
!
      call Kemo_Rotate_mat(rot_mat, angle_deg, axis)
      call cal_matmat44(mat(1,1), rot_mat(1,1), mat_tmp(1,1))
!
      end subroutine Kemo_Rotate
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Rotate_mat(rot_mat, angle_deg, axis)
!
      real(kind = kreal), intent(in) :: angle_deg
      real(kind = kreal), intent(in) :: axis(3)
      real(kind = kreal), intent(inout) :: rot_mat(4,4)
!
      real(kind = kreal) :: c_agl, s_agl, angle, pi
      real(kind = kreal) :: axs1(3), saxis
!
!
      pi = four * atan(one)
      angle = angle_deg * pi / 180.0
      c_agl = cos(angle)
      s_agl = sin(angle)
      saxis = sqrt(axis(1)*axis(1) + axis(2)*axis(2) + axis(3)*axis(3))
      if(saxis .eq. 0.0d0) then
        axs1(1:2) = 0.0d0
        axs1(3) = 1.0d0
      else
        axs1(1:3) = axis(1:3) / saxis
      end if
!
      rot_mat(1,1) = axs1(1)*axs1(1) *  (one - c_agl) +           c_agl
      rot_mat(1,2) = axs1(1)*axs1(2) * (one - c_agl) - axs1(3) * s_agl
      rot_mat(1,3) = axs1(1)*axs1(3) * (one - c_agl) + axs1(2) * s_agl
      rot_mat(1,4) = zero
!
      rot_mat(2,1) = axs1(2)*axs1(1) * (one - c_agl) + axs1(3) * s_agl
      rot_mat(2,2) = axs1(2)*axs1(2) *  (one - c_agl) +           c_agl
      rot_mat(2,3) = axs1(2)*axs1(3) * (one - c_agl) - axs1(1) * s_agl
      rot_mat(2,4) = zero
!
      rot_mat(3,1) = axs1(3)*axs1(1) * (one - c_agl) - axs1(2) * s_agl
      rot_mat(3,2) = axs1(3)*axs1(2) * (one - c_agl) + axs1(1) * s_agl
      rot_mat(3,3) = axs1(3)*axs1(3) * (one - c_agl) +           c_agl
      rot_mat(3,4) = zero
!
      rot_mat(4,1) = zero
      rot_mat(4,2) = zero
      rot_mat(4,3) = zero
      rot_mat(4,4) = one
!
      end subroutine Kemo_Rotate_mat
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Translate_mat(trans_mat, trans)
!
      real(kind = kreal), intent(in) :: trans(3)
      real(kind = kreal), intent(inout) :: trans_mat(4,4)
!
!
      trans_mat(1,1) = one
      trans_mat(1,2) = zero
      trans_mat(1,3) = zero
      trans_mat(1,4) = trans(1)
!
      trans_mat(2,1) = zero
      trans_mat(2,2) = one
      trans_mat(2,3) = zero
      trans_mat(2,4) = trans(2)
!
      trans_mat(3,1) = zero
      trans_mat(3,2) = zero
      trans_mat(3,3) = one
      trans_mat(3,4) = trans(3)
!
      trans_mat(4,1) = zero
      trans_mat(4,2) = zero
      trans_mat(4,3) = zero
      trans_mat(4,4) = one
!
      end subroutine Kemo_Translate_mat
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Scale_mat(scale_mat, scale)
!
      real(kind = kreal), intent(in) :: scale(3)
      real(kind = kreal), intent(inout) :: scale_mat(4,4)
!
!
      scale_mat(1,1) = scale(1)
      scale_mat(1,2) = zero
      scale_mat(1,3) = zero
      scale_mat(1,4) = zero
!
      scale_mat(2,1) = zero
      scale_mat(2,2) = scale(2)
      scale_mat(2,3) = zero
      scale_mat(2,4) = zero
!
      scale_mat(3,1) = zero
      scale_mat(3,2) = zero
      scale_mat(3,3) = scale(3)
      scale_mat(3,4) = zero
!
      scale_mat(4,1) = zero
      scale_mat(4,2) = zero
      scale_mat(4,3) = zero
      scale_mat(4,4) = one
!
      end subroutine Kemo_Scale_mat
!
!  ---------------------------------------------------------------------
!
      subroutine Kemo_Unit_mat(unit_mat)
!
      real(kind = kreal), intent(inout) :: unit_mat(4,4)
!
!
      unit_mat(1,1) = one
      unit_mat(1,2) = zero
      unit_mat(1,3) = zero
      unit_mat(1,4) = zero
!
      unit_mat(2,1) = zero
      unit_mat(2,2) = one
      unit_mat(2,3) = zero
      unit_mat(2,4) = zero
!
      unit_mat(3,1) = zero
      unit_mat(3,2) = zero
      unit_mat(3,3) = one
      unit_mat(3,4) = zero
!
      unit_mat(4,1) = zero
      unit_mat(4,2) = zero
      unit_mat(4,3) = zero
      unit_mat(4,4) = one
!
      end subroutine Kemo_Unit_mat
!
!  ---------------------------------------------------------------------
!
      subroutine  cal_matmat44(S, A, B)
!
      real(kind = kreal), intent(in) :: B(4,4), A(4,4)
      real(kind = kreal), intent(inout) :: S(4,4)
!
!
      S(1,1) = A(1,1) * B(1,1) + A(1,2) * B(2,1)                        &
     &        + A(1,3) * B(3,1) + A(1,4) * B(4,1)
      S(2,1) = A(2,1) * B(1,1) + A(2,2) * B(2,1)                        &
     &        + A(2,3) * B(3,1) + A(2,4) * B(4,1)
      S(3,1) = A(3,1) * B(1,1) + A(3,2) * B(2,1)                        &
     &        + A(3,3) * B(3,1) + A(3,4) * B(4,1)
      S(4,1) = A(4,1) * B(1,1) + A(4,2) * B(2,1)                        &
     &        + A(4,3) * B(3,1) + A(4,4) * B(4,1)

      S(1,2) = A(1,1) * B(1,2) + A(1,2) * B(2,2)                        &
     &        + A(1,3) * B(3,2) + A(1,4) * B(4,2)
      S(2,2) = A(2,1) * B(1,2) + A(2,2) * B(2,2)                        &
     &        + A(2,3) * B(3,2) + A(2,4) * B(4,2)
      S(3,2) = A(3,1) * B(1,2) + A(3,2) * B(2,2)                        &
     &        + A(3,3) * B(3,2) + A(3,4) * B(4,2)
      S(4,2) = A(4,1) * B(1,2) + A(4,2) * B(2,2)                        &
     &        + A(4,3) * B(3,2) + A(4,4) * B(4,2)

      S(1,3) = A(1,1) * B(1,3) + A(1,2) * B(2,3)                        &
     &        + A(1,3) * B(3,3) + A(1,4) * B(4,3)
      S(2,3) = A(2,1) * B(1,3) + A(2,2) * B(2,3)                        &
     &        + A(2,3) * B(3,3) + A(2,4) * B(4,3)
      S(3,3) = A(3,1) * B(1,3) + A(3,2) * B(2,3)                        &
     &        + A(3,3) * B(3,3) + A(3,4) * B(4,3)
      S(4,3) = A(4,1) * B(1,3) + A(4,2) * B(2,3)                        &
     &        + A(4,3) * B(3,3) + A(4,4) * B(4,3)

      S(1,4) = A(1,1) * B(1,4) + A(1,2) * B(2,4)                        &
     &        + A(1,3) * B(3,4) + A(1,4) * B(4,4)
      S(2,4) = A(2,1) * B(1,4) + A(2,2) * B(2,4)                        &
     &        + A(2,3) * B(3,4) + A(2,4) * B(4,4)
      S(3,4) = A(3,1) * B(1,4) + A(3,2) * B(2,4)                        &
     &        + A(3,3) * B(3,4) + A(3,4) * B(4,4)
      S(4,4) = A(4,1) * B(1,4) + A(4,2) * B(2,4)                        &
     &        + A(4,3) * B(3,4) + A(4,4) * B(4,4)
!
      end subroutine  cal_matmat44
!
!  ---------------------------------------------------------------------
!
      subroutine  cal_matmat44_multi_smp(N, S, A, B)
!
      integer(kind = kint), intent(in) :: N
      real(kind = kreal), intent(in) :: B(4,4,N), A(4,4,N)
      real(kind = kreal), intent(inout) :: S(4,4,N)
      integer(kind = kint) :: i
!
!
!$omp do
      do i = 1, N
        S(1,1,i) = A(1,1,i) * B(1,1,i) + A(1,2,i) * B(2,1,i)            &
     &            + A(1,3,i) * B(3,1,i) + A(1,4,i) * B(4,1,i)
        S(2,1,i) = A(2,1,i) * B(1,1,i) + A(2,2,i) * B(2,1,i)            &
     &            + A(2,3,i) * B(3,1,i) + A(2,4,i) * B(4,1,i)
        S(3,1,i) = A(3,1,i) * B(1,1,i) + A(3,2,i) * B(2,1,i)            &
     &            + A(3,3,i) * B(3,1,i) + A(3,4,i) * B(4,1,i)
        S(4,1,i) = A(4,1,i) * B(1,1,i) + A(4,2,i) * B(2,1,i)            &
     &            + A(4,3,i) * B(3,1,i) + A(4,4,i) * B(4,1,i)

        S(1,2,i) = A(1,1,i) * B(1,2,i) + A(1,2,i) * B(2,2,i)            &
     &            + A(1,3,i) * B(3,2,i) + A(1,4,i) * B(4,2,i)
        S(2,2,i) = A(2,1,i) * B(1,2,i) + A(2,2,i) * B(2,2,i)            &
     &            + A(2,3,i) * B(3,2,i) + A(2,4,i) * B(4,2,i)
        S(3,2,i) = A(3,1,i) * B(1,2,i) + A(3,2,i) * B(2,2,i)            &
     &            + A(3,3,i) * B(3,2,i) + A(3,4,i) * B(4,2,i)
        S(4,2,i) = A(4,1,i) * B(1,2,i) + A(4,2,i) * B(2,2,i)            &
     &            + A(4,3,i) * B(3,2,i) + A(4,4,i) * B(4,2,i)

        S(1,3,i) = A(1,1,i) * B(1,3,i) + A(1,2,i) * B(2,3,i)            &
     &            + A(1,3,i) * B(3,3,i) + A(1,4,i) * B(4,3,i)
        S(2,3,i) = A(2,1,i) * B(1,3,i) + A(2,2,i) * B(2,3,i)            &
     &            + A(2,3,i) * B(3,3,i) + A(2,4,i) * B(4,3,i)
        S(3,3,i) = A(3,1,i) * B(1,3,i) + A(3,2,i) * B(2,3,i)            &
     &            + A(3,3,i) * B(3,3,i) + A(3,4,i) * B(4,3,i)
        S(4,3,i) = A(4,1,i) * B(1,3,i) + A(4,2,i) * B(2,3,i)            &
     &            + A(4,3,i) * B(3,3,i) + A(4,4,i) * B(4,3,i)

        S(1,4,i) = A(1,1,i) * B(1,4,i) + A(1,2,i) * B(2,4,i)            &
     &            + A(1,3,i) * B(3,4,i) + A(1,4,i) * B(4,4,i)
        S(2,4,i) = A(2,1,i) * B(1,4,i) + A(2,2,i) * B(2,4,i)            &
     &            + A(2,3,i) * B(3,4,i) + A(2,4,i) * B(4,4,i)
        S(3,4,i) = A(3,1,i) * B(1,4,i) + A(3,2,i) * B(2,4,i)            &
     &            + A(3,3,i) * B(3,4,i) + A(3,4,i) * B(4,4,i)
        S(4,4,i) = A(4,1,i) * B(1,4,i) + A(4,2,i) * B(2,4,i)            &
     &            + A(4,3,i) * B(3,4,i) + A(4,4,i) * B(4,4,i)
      end do
!$omp end do nowait
!
      end subroutine  cal_matmat44_multi_smp
!
!  ---------------------------------------------------------------------
!
      subroutine  cal_matcmat44_multi_smp(N, S, A, B)
!
      integer(kind = kint), intent(in) :: N
      real(kind = kreal), intent(in) :: B(4,4), A(4,4,N)
      real(kind = kreal), intent(inout) :: S(4,4,N)
      integer(kind = kint) :: i
!
!
!$omp do
      do i = 1, N
        S(1,1,i) = A(1,1,i) * B(1,1) + A(1,2,i) * B(2,1)                &
     &            + A(1,3,i) * B(3,1) + A(1,4,i) * B(4,1)
        S(2,1,i) = A(2,1,i) * B(1,1) + A(2,2,i) * B(2,1)                &
     &            + A(2,3,i) * B(3,1) + A(2,4,i) * B(4,1)
        S(3,1,i) = A(3,1,i) * B(1,1) + A(3,2,i) * B(2,1)                &
     &            + A(3,3,i) * B(3,1) + A(3,4,i) * B(4,1)
        S(4,1,i) = A(4,1,i) * B(1,1) + A(4,2,i) * B(2,1)                &
     &            + A(4,3,i) * B(3,1) + A(4,4,i) * B(4,1)

        S(1,2,i) = A(1,1,i) * B(1,2) + A(1,2,i) * B(2,2)                &
     &            + A(1,3,i) * B(3,2) + A(1,4,i) * B(4,2)
        S(2,2,i) = A(2,1,i) * B(1,2) + A(2,2,i) * B(2,2)                &
     &            + A(2,3,i) * B(3,2) + A(2,4,i) * B(4,2)
        S(3,2,i) = A(3,1,i) * B(1,2) + A(3,2,i) * B(2,2)                &
     &            + A(3,3,i) * B(3,2) + A(3,4,i) * B(4,2)
        S(4,2,i) = A(4,1,i) * B(1,2) + A(4,2,i) * B(2,2)                &
     &            + A(4,3,i) * B(3,2) + A(4,4,i) * B(4,2)

        S(1,3,i) = A(1,1,i) * B(1,3) + A(1,2,i) * B(2,3)                &
     &            + A(1,3,i) * B(3,3) + A(1,4,i) * B(4,3)
        S(2,3,i) = A(2,1,i) * B(1,3) + A(2,2,i) * B(2,3)                &
     &            + A(2,3,i) * B(3,3) + A(2,4,i) * B(4,3)
        S(3,3,i) = A(3,1,i) * B(1,3) + A(3,2,i) * B(2,3)                &
     &            + A(3,3,i) * B(3,3) + A(3,4,i) * B(4,3)
        S(4,3,i) = A(4,1,i) * B(1,3) + A(4,2,i) * B(2,3)                &
     &            + A(4,3,i) * B(3,3) + A(4,4,i) * B(4,3)

        S(1,4,i) = A(1,1,i) * B(1,4) + A(1,2,i) * B(2,4)                &
     &            + A(1,3,i) * B(3,4) + A(1,4,i) * B(4,4)
        S(2,4,i) = A(2,1,i) * B(1,4) + A(2,2,i) * B(2,4)                &
     &            + A(2,3,i) * B(3,4) + A(2,4,i) * B(4,4)
        S(3,4,i) = A(3,1,i) * B(1,4) + A(3,2,i) * B(2,4)                &
     &            + A(3,3,i) * B(3,4) + A(3,4,i) * B(4,4)
        S(4,4,i) = A(4,1,i) * B(1,4) + A(4,2,i) * B(2,4)                &
     &            + A(4,3,i) * B(3,4) + A(4,4,i) * B(4,4)
      end do
!$omp end do nowait
!
      end subroutine  cal_matcmat44_multi_smp
!
!  ---------------------------------------------------------------------
!
      subroutine  cal_cmatmat44_multi_smp(N, S, A, B)
!
      integer(kind = kint), intent(in) :: N
      real(kind = kreal), intent(in) :: B(4,4,N), A(4,4)
      real(kind = kreal), intent(inout) :: S(4,4,N)
      integer(kind = kint) :: i
!
!
!$omp do
      do i = 1, N
        S(1,1,i) = A(1,1) * B(1,1,i) + A(1,2) * B(2,1,i)                &
     &            + A(1,3) * B(3,1,i) + A(1,4) * B(4,1,i)
        S(2,1,i) = A(2,1) * B(1,1,i) + A(2,2) * B(2,1,i)                &
     &            + A(2,3) * B(3,1,i) + A(2,4) * B(4,1,i)
        S(3,1,i) = A(3,1) * B(1,1,i) + A(3,2) * B(2,1,i)                &
     &            + A(3,3) * B(3,1,i) + A(3,4) * B(4,1,i)
        S(4,1,i) = A(4,1) * B(1,1,i) + A(4,2) * B(2,1,i)                &
     &            + A(4,3) * B(3,1,i) + A(4,4) * B(4,1,i)

        S(1,2,i) = A(1,1) * B(1,2,i) + A(1,2) * B(2,2,i)                &
     &            + A(1,3) * B(3,2,i) + A(1,4) * B(4,2,i)
        S(2,2,i) = A(2,1) * B(1,2,i) + A(2,2) * B(2,2,i)                &
     &            + A(2,3) * B(3,2,i) + A(2,4) * B(4,2,i)
        S(3,2,i) = A(3,1) * B(1,2,i) + A(3,2) * B(2,2,i)                &
     &            + A(3,3) * B(3,2,i) + A(3,4) * B(4,2,i)
        S(4,2,i) = A(4,1) * B(1,2,i) + A(4,2) * B(2,2,i)                &
     &            + A(4,3) * B(3,2,i) + A(4,4) * B(4,2,i)

        S(1,3,i) = A(1,1) * B(1,3,i) + A(1,2) * B(2,3,i)                &
     &            + A(1,3) * B(3,3,i) + A(1,4) * B(4,3,i)
        S(2,3,i) = A(2,1) * B(1,3,i) + A(2,2) * B(2,3,i)                &
     &            + A(2,3) * B(3,3,i) + A(2,4) * B(4,3,i)
        S(3,3,i) = A(3,1) * B(1,3,i) + A(3,2) * B(2,3,i)                &
     &            + A(3,3) * B(3,3,i) + A(3,4) * B(4,3,i)
        S(4,3,i) = A(4,1) * B(1,3,i) + A(4,2) * B(2,3,i)                &
     &            + A(4,3) * B(3,3,i) + A(4,4) * B(4,3,i)

        S(1,4,i) = A(1,1) * B(1,4,i) + A(1,2) * B(2,4,i)                &
     &            + A(1,3) * B(3,4,i) + A(1,4) * B(4,4,i)
        S(2,4,i) = A(2,1) * B(1,4,i) + A(2,2) * B(2,4,i)                &
     &            + A(2,3) * B(3,4,i) + A(2,4) * B(4,4,i)
        S(3,4,i) = A(3,1) * B(1,4,i) + A(3,2) * B(2,4,i)                &
     &            + A(3,3) * B(3,4,i) + A(3,4) * B(4,4,i)
        S(4,4,i) = A(4,1) * B(1,4,i) + A(4,2) * B(2,4,i)                &
     &            + A(4,3) * B(3,4,i) + A(4,4) * B(4,4,i)
      end do
!$omp end do nowait
!
      end subroutine  cal_cmatmat44_multi_smp
!
!  ---------------------------------------------------------------------
!
      end module transform_mat_operations
