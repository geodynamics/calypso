!>@file   set_composition_pe_range.f90
!!@brief  module set_composition_pe_range
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine s_set_composition_pe_range                           &
!!     &         (num_pe, num_pvr, istack_PVR_modes,                    &
!!     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!!@endverbatim
!
      module set_composition_pe_range
!
      use m_precision
!
      use t_rendering_vr_image
      use t_pvr_image_array
!
      implicit none
!
      private :: set_rank_to_write_tmp, set_rank_to_write_images
      private :: each_composition_pe_range
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_composition_pe_range                             &
     &         (num_pe, num_pvr, istack_PVR_modes,                      &
     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: istack_PVR_modes(0:6)
!
      integer(kind = kint), intent(in) :: num_pvr_images
      integer(kind = kint), intent(in) :: istack_pvr_images(0:num_pvr)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
!
!
      ist_pvr = istack_PVR_modes(0) + 1
      ied_pvr = istack_PVR_modes(2)
      call each_composition_pe_range                                    &
     &   (num_pe, num_pvr, ist_pvr, ied_pvr,                            &
     &    num_pvr_images, istack_pvr_images, pvr_rgb)
!
      do i_pvr = 3, 6
        ist_pvr = istack_PVR_modes(i_pvr-1) + 1
        ied_pvr = istack_PVR_modes(i_pvr)
        call each_composition_pe_range                                  &
     &     (num_pe, num_pvr, ist_pvr, ied_pvr,                          &
     &      num_pvr_images, istack_pvr_images, pvr_rgb)
      end do
!
      end subroutine s_set_composition_pe_range
!
!  ---------------------------------------------------------------------
!
      subroutine each_composition_pe_range                              &
     &         (num_pe, num_pvr, ist_pvr, ied_pvr,                      &
     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr, ist_pvr, ied_pvr
!
      integer(kind = kint), intent(in) :: num_pvr_images
      integer(kind = kint), intent(in) :: istack_pvr_images(0:num_pvr)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint), allocatable:: irank_image_tmp(:)
      integer(kind = kint), allocatable:: irank_end_tmp(:)
      integer(kind = kint), allocatable:: maxpe_composit_tmp(:)
!
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_img = istack_pvr_images(ist_pvr-1)
      num_img = istack_pvr_images(ied_pvr  ) - ist_img
!
      allocate(maxpe_composit_tmp(num_img))
      allocate(irank_image_tmp(num_img))
      allocate(irank_end_tmp(num_img))
!
!
      call set_rank_to_write_tmp(num_pe, num_img,                       &
     &    maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
      call set_rank_to_write_images                                     &
     &   (num_img, irank_image_tmp, irank_end_tmp, pvr_rgb(ist_img+1))
!
      deallocate(irank_image_tmp, irank_end_tmp, maxpe_composit_tmp)
!
      end subroutine each_composition_pe_range
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_tmp(num_pe, num_img,                 &
     &          maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_img
!
      integer(kind = kint), intent(inout)                               &
     &              :: maxpe_composit_tmp(num_img)
      integer(kind = kint), intent(inout)                               &
     &              :: irank_image_tmp(num_img)
      integer(kind = kint), intent(inout)                               &
     &              :: irank_end_tmp(num_img)
!
      integer(kind = kint) :: i_img, num
      real(kind = kreal) :: address
!
!
!$omp parallel workshare
      maxpe_composit_tmp(1:num_img) = num_pe
!$omp end parallel workshare
!
!$omp parallel do private(i_img,address)
      do i_img = 1, num_img
        address = dble((i_img-1) * num_pe) / dble(num_img)
        irank_image_tmp(i_img) = int(aint(address))
      end do
!$omp end parallel do
!
      do i_img = 1, num_img - 1
        num = irank_image_tmp(i_img+1) - irank_image_tmp(i_img)
        if(num .le. 0) then
           irank_end_tmp(i_img) = irank_image_tmp(i_img)
        else if(num .gt. maxpe_composit_tmp(i_img)) then
           irank_end_tmp(i_img)                                         &
     &       = irank_image_tmp(i_img) + maxpe_composit_tmp(i_img) - 1
        else
          irank_end_tmp(i_img) = irank_image_tmp(i_img+1) - 1
        end if
      end do
!
      num = num_pe - irank_image_tmp(num_img)
      if(num .gt. maxpe_composit_tmp(num_img)) then
         irank_end_tmp(num_img) = irank_image_tmp(num_img)              &
     &                         + maxpe_composit_tmp(num_img) - 1
      else
        irank_end_tmp(num_img) = num_pe - 1
      end if
!
      end subroutine set_rank_to_write_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_images                               &
     &         (num_img, irank_image_tmp, irank_end_tmp, pvr_rgb)
!
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: irank_image_tmp(num_img)
      integer(kind = kint), intent(in) :: irank_end_tmp(num_img)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
!
      integer(kind = kint) :: i_img
!
!
!$omp parallel do private(i_img)
      do i_img = 1, num_img
        pvr_rgb(i_img)%irank_image_file = irank_image_tmp(i_img)
        pvr_rgb(i_img)%irank_end_composit = irank_end_tmp(i_img)
!
        pvr_rgb(i_img)%npe_img_composit                                 &
     &      = pvr_rgb(i_img)%irank_end_composit                         &
     &       - pvr_rgb(i_img)%irank_image_file + 1
      end do
!$omp end parallel do
!
      end subroutine set_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_maxpe_composit_tmp(num_pe, num_pvr,           &
     &                                       maxpe_composit_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
!
      integer(kind = kint), intent(inout)                               &
     &              :: maxpe_composit_tmp(2*num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr
        maxpe_composit_tmp(2+i_pvr-1) = 2*num_pe
        maxpe_composit_tmp(2+i_pvr  ) = 2*num_pe
      end do
!
      end subroutine anaglyph_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      end module set_composition_pe_range
