!>@file   set_composition_pe_range.f90
!!@brief  module set_composition_pe_range
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine s_set_composition_pe_range(num_pe, num_pvr, pvr_ctl, &
!!     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!      subroutine set_anaglyph_composite_pe_range                      &
!!     &         (num_pe, num_pvr, pvr_ctl, pvr_rgb)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr)
!!@endverbatim
!
      module set_composition_pe_range
!
      use m_precision
!
      use t_control_data_4_pvr
      use t_rendering_vr_image
      use t_pvr_image_array
!
      implicit none
!
      private :: set_rank_to_write_tmp, set_rank_to_write_images
      private :: set_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_composition_pe_range(num_pe, num_pvr, pvr_ctl,   &
     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
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
!
      allocate(maxpe_composit_tmp(num_pvr_images))
      allocate(irank_image_tmp(num_pvr_images))
      allocate(irank_end_tmp(num_pvr_images))
!
      call set_maxpe_composit_tmp(num_pe, num_pvr, pvr_ctl,             &
     &    num_pvr_images, istack_pvr_images, maxpe_composit_tmp)
!
      call set_rank_to_write_tmp(num_pe, num_pvr_images,                &
     &    maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
      call set_rank_to_write_images(num_pvr_images,                     &
     &    irank_image_tmp, irank_end_tmp, pvr_rgb)
!
      deallocate(irank_image_tmp, irank_end_tmp, maxpe_composit_tmp)
!
      end subroutine s_set_composition_pe_range
!
!  ---------------------------------------------------------------------
!
      subroutine set_anaglyph_composite_pe_range                        &
     &         (num_pe, num_pvr, pvr_ctl, pvr_rgb)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr)
!
      integer(kind = kint), allocatable:: irank_image_tmp(:)
      integer(kind = kint), allocatable:: irank_end_tmp(:)
      integer(kind = kint), allocatable:: maxpe_composit_tmp(:)
!
!
      allocate(maxpe_composit_tmp(2*num_pvr))
      allocate(irank_image_tmp(2*num_pvr))
      allocate(irank_end_tmp(2*num_pvr))
!
      call anaglyph_maxpe_composit_tmp(num_pe, num_pvr, pvr_ctl,        &
     &                                 maxpe_composit_tmp)
!
      call set_rank_to_write_tmp(num_pe, (2*num_pvr),                   &
     &    maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
      call anaglyph_rank_to_write_images                                &
     &   (num_pvr, irank_image_tmp, irank_end_tmp, pvr_rgb)
!
      deallocate(irank_image_tmp, irank_end_tmp, maxpe_composit_tmp)
!
      end subroutine set_anaglyph_composite_pe_range
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_tmp(num_pe, num_pvr_rendering,       &
     &          maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in)                                  &
     &              :: maxpe_composit_tmp(num_pvr_rendering)
!
      integer(kind = kint), intent(inout)                               &
     &              :: irank_image_tmp(num_pvr_rendering)
      integer(kind = kint), intent(inout)                               &
     &              :: irank_end_tmp(num_pvr_rendering)
!
      integer(kind = kint) :: i_pvr, num
      real(kind = kreal) :: address
!
!
      do i_pvr = 1, num_pvr_rendering
        address = dble((i_pvr-1) * num_pe) / dble(num_pvr_rendering)
        irank_image_tmp(i_pvr) = aint(address)
      end do

      do i_pvr = 1, num_pvr_rendering - 1 
        num = irank_image_tmp(i_pvr+1) - irank_image_tmp(i_pvr)
        if(num .le. 0) then
           irank_end_tmp(i_pvr) = irank_image_tmp(i_pvr)
        else if(num .gt. maxpe_composit_tmp(i_pvr)) then
           irank_end_tmp(i_pvr)                                         &
     &       = irank_image_tmp(i_pvr) + maxpe_composit_tmp(i_pvr) - 1
        else
          irank_end_tmp(i_pvr) = irank_image_tmp(i_pvr+1) - 1
        end if
      end do
!
      num = num_pe - irank_image_tmp(num_pvr_rendering)
      if(num .gt. maxpe_composit_tmp(num_pvr_rendering)) then
         irank_end_tmp(num_pvr_rendering)                               &
     &     = irank_image_tmp(num_pvr_rendering)                         &
     &      + maxpe_composit_tmp(num_pvr_rendering) - 1
      else
        irank_end_tmp(num_pvr_rendering) = num_pe - 1
      end if
!
      end subroutine set_rank_to_write_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_images(num_pvr_images,               &
     &          irank_image_tmp, irank_end_tmp, pvr_rgb)
!
      integer(kind = kint), intent(in) :: num_pvr_images
      integer(kind = kint), intent(in)                                  &
     &              :: irank_image_tmp(num_pvr_images)
      integer(kind = kint), intent(in)                                  &
     &              :: irank_end_tmp(num_pvr_images)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_img
!
!
      do i_img = 1, num_pvr_images
        pvr_rgb(i_img)%irank_image_file = irank_image_tmp(i_img)
        pvr_rgb(i_img)%irank_end_composit = irank_end_tmp(i_img)
!
        pvr_rgb(i_img)%npe_img_composit                                 &
     &      = pvr_rgb(i_img)%irank_end_composit                         &
     &       - pvr_rgb(i_img)%irank_image_file + 1
      end do
!
      end subroutine set_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rank_to_write_images                          &
     &         (num_pvr, irank_image_tmp, irank_end_tmp, pvr_rgb)
!
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: irank_image_tmp(2*num_pvr)
      integer(kind = kint), intent(in) :: irank_end_tmp(2*num_pvr)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr
        pvr_rgb(i_pvr)%irank_image_file =   irank_image_tmp(2*i_pvr-1)
        pvr_rgb(i_pvr)%irank_end_composit = irank_end_tmp(2*i_pvr    )
      end do
!
      do i_pvr = 1, num_pvr
        pvr_rgb(i_pvr)%npe_img_composit                                 &
     &      = pvr_rgb(i_pvr)%irank_end_composit                         &
     &       - pvr_rgb(i_pvr)%irank_image_file + 1
      end do
!
      end subroutine anaglyph_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_maxpe_composit_tmp(num_pe, num_pvr, pvr_ctl,       &
     &          num_pvr_images, istack_pvr_images, maxpe_composit_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pvr_images(0:num_pvr)
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
      integer(kind = kint), intent(in) :: num_pvr_images
!
      integer(kind = kint), intent(inout)                               &
     &              :: maxpe_composit_tmp(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, nmax
      integer(kind = kint) :: i_img, ist_img, num_img
!
!
      do i_pvr = 1, num_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
!
        if(pvr_ctl(i_pvr)%maxpe_composit_ctl%iflag .gt. 0) then
          nmax = pvr_ctl(i_pvr)%maxpe_composit_ctl%intvalue
        else
          nmax = num_pe
        end if
        do i_img = 1, num_img
          maxpe_composit_tmp(i_img+ist_img) = nmax
        end do
      end do
!
      end subroutine set_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_maxpe_composit_tmp(num_pe, num_pvr,           &
     &          pvr_ctl, maxpe_composit_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: maxpe_composit_tmp(2*num_pvr)
!
      integer(kind = kint) :: i_pvr, nmax
!
!
      do i_pvr = 1, num_pvr
        if(pvr_ctl(i_pvr)%maxpe_composit_ctl%iflag .gt. 0) then
          nmax = pvr_ctl(i_pvr)%maxpe_composit_ctl%intvalue
        else
          nmax = num_pe
        end if
        maxpe_composit_tmp(2+i_pvr-1) = 2*nmax
        maxpe_composit_tmp(2+i_pvr  ) = 2*nmax
      end do
!
      end subroutine anaglyph_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      end module set_composition_pe_range
