!>@file  zlib_cvt_ucd_data.f90
!!       module zlib_cvt_ucd_data
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine defleate_ucd_vector(nnod, num, ntot_comp, vect,      &
!!     &          istack_merged_intnod, zbuf)
!!      subroutine defleate_ucd_connect                                 &
!!     &         (nele, ie, nnod_ele, istack_merged_ele, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_cvt_ucd_data
!
      use m_precision
      use m_constants
!
      use t_buffer_4_gzip
      use vtk_data_to_buffer
      use ucd_data_to_buffer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine defleate_ucd_vector(nnod, num, ntot_comp, vect,        &
     &          istack_merged_intnod, zbuf)
!
      integer(kind = kint_gl), intent(in) :: istack_merged_intnod
      integer(kind = kint_gl), intent(in) :: nnod, num
      integer(kind=kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: vect(nnod,ntot_comp)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod_gl, inod, ist
      integer :: nline
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
      real(kind = kreal)  :: dat_1(ntot_comp)
!
!
      inod_gl = 1
      dat_1(1:ntot_comp) = zero
      ilen_line = len(ucd_each_field(inod_gl, ntot_comp, dat_1))
      zbuf%ilen_gz                                                      &
     &      = int(real(num*ilen_line) * 1.01 + 24, KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        inod_gl = 1 + istack_merged_intnod
        dat_1(1:ntot_comp) = vect(1,1:ntot_comp)
        call gzip_defleat_once(ilen_line,                               &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((num - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          inod_gl = ist+1 + istack_merged_intnod
          dat_1(1:ntot_comp) = vect(ist+1,1:ntot_comp)
          call gzip_defleat_begin(ilen_line,                            &
     &        ucd_each_field(inod_gl, ntot_comp, dat_1),                &
     &        ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            inod_gl =    inod + istack_merged_intnod
            dat_1(1:ntot_comp) = vect(inod,1:ntot_comp)
            call gzip_defleat_cont(ilen_line,                           &
     &          ucd_each_field(inod_gl, ntot_comp, dat_1),              &
     &          ilen_in, ilen_used)
          end do
          inod_gl = ist + nline + istack_merged_intnod
          dat_1(1:ntot_comp) = vect(ist+nline,1:ntot_comp)
          call gzip_defleat_last(ilen_line,                             &
     &        ucd_each_field(inod_gl, ntot_comp, dat_1),                &
     &        ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. num) exit
        end do
      else
        zbuf%ilen_gzipped = 0
      end if
!
      end subroutine defleate_ucd_vector
!
! -----------------------------------------------------------------------
!
      subroutine defleate_ucd_connect                                   &
     &         (nele, ie, nnod_ele, istack_merged_ele, zbuf)
!
      integer(kind = kint_gl), intent(in) :: istack_merged_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist, iele_gl
      integer :: nline
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: ilen_tmp
      integer  :: ilen_line, ilen_used, ilen_in
!
!
      iele_gl = 1
      ie0(1:nnod_ele) = 0
      ilen_line = len(ucd_each_connect(iele_gl, nnod_ele, ie0))
      zbuf%ilen_gz                                                      &
     &      = int(dble(nele*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        iele_gl = 1
        ie0(1:nnod_ele) = ie(1,1:nnod_ele)
        call gzip_defleat_once(ilen_line,                               &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = int(min((nele - ist), huge_30/ilen_line))
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          iele_gl = ist+1 + istack_merged_ele
          ie0(1:nnod_ele) = ie(ist+1,1:nnod_ele)
          call gzip_defleat_begin(ilen_line,                            &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &        ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            iele_gl = i + istack_merged_ele
            ie0(1:nnod_ele) = ie(i,1:nnod_ele)
            call gzip_defleat_cont(ilen_line,                           &
     &          ucd_each_connect(iele_gl, nnod_ele, ie0),               &
     &          ilen_in, ilen_used)
          end do
!
          iele_gl = ist+nline + istack_merged_ele
          ie0(1:nnod_ele) = ie(ist+nline,1:nnod_ele)
          call gzip_defleat_last(ilen_line,                             &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &        ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      else
        zbuf%ilen_gzipped = 0
      end if
!
      end subroutine defleate_ucd_connect
!
! -----------------------------------------------------------------------
!
      end module zlib_cvt_ucd_data
