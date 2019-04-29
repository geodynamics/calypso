!>@file  zlib_cvt_vtk_data.f90
!!       module zlib_cvt_vtk_data
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usging MPI-IO
!!
!!@verbatim
!!      subroutine defleate_vtk_scalar(nnod, num, vect, zbuf)
!!      subroutine defleate_vtk_vector(nnod, num, vect, zbuf)
!!      subroutine defleate_vtk_tensor(nnod, num, vect, zbuf)
!!      subroutine defleate_vtk_celltype(nele, nnod_ele, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module zlib_cvt_vtk_data
!
      use m_precision
      use m_constants
!
      use t_buffer_4_gzip
      use vtk_data_to_buffer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine defleate_vtk_scalar(nnod, num, vect, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      integer(kind = kint_gl) :: inod, ist
      integer :: nline
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len(vtk_each_vector(zero, zero, zero))
      zbuf%ilen_gz                                                      &
     &      = int(real(3*num*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ilen_line, vtk_each_scalar(vect(1)),    &
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
          call gzip_defleat_begin                                       &
     &      (ilen_line, vtk_each_scalar(vect(ist+1)),                   &
     &       ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_scalar(vect(inod)), ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_scalar(vect(ist+nline)), ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. num) exit
        end do
      else
        zbuf%ilen_gzipped = 0
      end if
!
      end subroutine defleate_vtk_scalar
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_vector(nnod, num, vect, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod, ist
      integer :: nline
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len(vtk_each_vector(zero, zero, zero))
      zbuf%ilen_gz                                                      &
     &     = int(dble(num*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ilen_line,                               &
     &      vtk_each_vector(vect(1,1),vect(1,2),vect(1,3)),             &
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
          call gzip_defleat_begin(ilen_line,                            &
     &      vtk_each_vector(vect(ist+1,1),vect(ist+1,2),vect(ist+1,3)), &
     &      ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,1),vect(inod,2),vect(inod,3)),    &
     &          ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,1),vect(ist+nline,2),vect(ist+nline,3)), &
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
      end subroutine defleate_vtk_vector
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_tensor(nnod, num, vect, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod,6)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ilen_line = len(vtk_each_vector(zero, zero, zero))
      zbuf%ilen_gz                                                      &
     &     = int(real(3*num*ilen_line) * 1.01 + 24, KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(num .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_begin(ilen_line,                              &
     &      vtk_each_vector(vect(1,1),vect(1,2),vect(1,3)),             &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        call gzip_defleat_cont(ilen_line,                               &
     &      vtk_each_vector(vect(1,2),vect(1,4),vect(1,5)),             &
     &      ilen_in, ilen_used)
        call gzip_defleat_last(ilen_line,                               &
     &      vtk_each_vector(vect(1,3),vect(1,5),vect(1,6)),             &
     &      ilen_in, ilen_used)
        zbuf%ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = min((num - ist), huge_30/ilen_line)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call gzip_defleat_begin(ilen_line,                            &
     &      vtk_each_vector(vect(ist+1,1),vect(ist+1,2),vect(ist+1,3)), &
     &      ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
          call gzip_defleat_cont(ilen_line,                             &
     &      vtk_each_vector(vect(ist+1,2),vect(ist+1,4),vect(ist+1,5)), &
     &      ilen_in, ilen_used)
          call gzip_defleat_cont(ilen_line,                             &
     &      vtk_each_vector(vect(ist+1,3),vect(ist+1,5),vect(ist+1,6)), &
     &      ilen_in, ilen_used)
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,1),vect(inod,2),vect(inod,3)),    &
     &          ilen_in, ilen_used)
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,2),vect(inod,4),vect(inod,5)),    &
     &          ilen_in, ilen_used)
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,3),vect(inod,5),vect(inod,6)),    &
     &          ilen_in, ilen_used)
          end do
          call gzip_defleat_cont(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,1),vect(ist+nline,2),vect(ist+nline,3)), &
     &        ilen_in, ilen_used)
          call gzip_defleat_cont(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,2),vect(ist+nline,4),vect(ist+nline,5)), &
     &        ilen_in, ilen_used)
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,3),vect(ist+nline,5),vect(ist+nline,6)), &
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
      end subroutine defleate_vtk_tensor
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_connect(nele, ie, nnod_ele, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i, ist, nline
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      ie0(1:nnod_ele) = 0
      ilen_line = len(vtk_each_connect(nnod_ele, ie0))
      zbuf%ilen_gz                                                      &
     &    = int(real(nele*ilen_line) * 1.01 + 24, KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        ie0(1:nnod_ele) = ie(1,1:nnod_ele) - 1
        call gzip_defleat_once(ilen_line,                               &
     &      vtk_each_connect(nnod_ele, ie0),                            &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = min((nele - ist), huge_30/ilen_line)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          ie0(1:nnod_ele) = ie(ist+1,1:nnod_ele) - 1
          call gzip_defleat_begin(ilen_line,                            &
     &        vtk_each_connect(nnod_ele, ie0),                          &
     &        ilen_in, ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            ie0(1:nnod_ele) = ie(i,1:nnod_ele) - 1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_connect(nnod_ele, ie0),                        &
     &          ilen_in, ilen_used)
          end do
!
          ie0(1:nnod_ele) = ie(ist+nline,1:nnod_ele) - 1
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_connect(nnod_ele, ie0),                          &
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
      end subroutine defleate_vtk_connect
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_celltype(nele, nnod_ele, zbuf)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_ele
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: icellid
      integer(kind = kint_gl) :: i, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer :: ilen_line, ilen_used, ilen_in
!
!
      icellid = vtk_cell_type(nnod_ele)
      ilen_line = len(vtk_each_cell_type(icellid))
      zbuf%ilen_gz                                                      &
     &      = int(dble(nele*ilen_line) * 1.01 + 24,KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
!
      if(nele .eq. 1) then
        ilen_in = int(zbuf%ilen_gz)
        call gzip_defleat_once(ilen_line, vtk_each_cell_type(icellid),  &
     &      ilen_in, ilen_used, zbuf%gzip_buf(1))
        zbuf%ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        zbuf%ilen_gzipped = 0
        ilen_tmp = int(dble(huge_30)*1.01+24,KIND(ilen_tmp))
        do
          nline = min((nele - ist), huge_30/ilen_line)
          ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
!
          call gzip_defleat_begin(ilen_line,                            &
     &        vtk_each_cell_type(icellid), ilen_in, ilen_used,          &
     &        zbuf%gzip_buf(zbuf%ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_cell_type(icellid), ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_cell_type(icellid), ilen_in, ilen_used)
!
          zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
      else
        zbuf%ilen_gzipped = 0
      end if
!
      end subroutine defleate_vtk_celltype
!
! -----------------------------------------------------------------------
!
      end module zlib_cvt_vtk_data
