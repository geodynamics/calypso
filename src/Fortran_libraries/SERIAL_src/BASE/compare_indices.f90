!>@file   compare_indices.f90
!!        module compare_indices
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on Apr., 2006
!!
!>@brief compare vector and integers
!!
!!@verbatim
!!      integer(kind = kint) function compare_field_vector              &
!!     &      (n_point, numdir, fld_name, d_fld1, d_fld2)
!!        character(len=kchara), intent(in) :: fld_name
!!        integer(kind = kint), intent(in) :: n_point, numdir
!!        real(kind = kreal), intent(in) :: d_fld1(n_point,numdir)
!!        real(kind = kreal), intent(in) :: d_fld2(n_point,numdir)
!!
!!      integer(kind = kint) function                                   &
!!     &                    check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
!!      integer(kind = kint) function                                   &
!!     &                    check_3_on_2(i1, i2, j1, j2, j3)
!!      integer(kind = kint) function                                   &
!!     &                    check_2_on_1(i1, j1, j2)
!!@endverbatim
!!
!!@param i1   1st compared integer
!!@param i2   2nd compared integer
!!@param i3   3rd compared integer
!!@param j1   1st reference integer
!!@param j2   2nd reference integer
!!@param j3   3rd reference integer
!!@param j4   4th reference integer
!
!
      module compare_indices
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), parameter :: TINY = 1.0d-12
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      integer(kind = kint) function compare_field_vector                &
     &      (n_point, numdir, fld_name, d_fld1, d_fld2)
!
      use m_machine_parameter
!
      implicit none
!
      character(len=kchara), intent(in) :: fld_name
      integer(kind = kint), intent(in) :: n_point, numdir
      real(kind = kreal), intent(in) :: d_fld1(n_point,numdir)
      real(kind = kreal), intent(in) :: d_fld2(n_point,numdir)
!
      real(kind = kreal), allocatable :: vmin(:), vmax(:), size(:)
      real(kind = kreal) :: scale, diff
      integer(kind = kint) :: inod, ifld, icomp
      integer(kind = kint) :: iflag
!
      iflag = 0
      allocate(vmin(numdir))
      allocate(vmax(numdir))
      allocate(size(numdir))
!
!$omp parallel workshare
      vmin(1:numdir) = minval(d_fld1(1:n_point,1:numdir),1)
      vmax(1:numdir) = maxval(d_fld1(1:n_point,1:numdir),1)
!$omp end parallel workshare
      size(1:numdir) = vmax(1:numdir) - vmin(1:numdir)
!
      scale = 0.0d0
      do icomp = 1, numdir
        scale = scale + vmax(icomp)**2
      end do
      scale = sqrt(scale)
      if(iflag_debug .gt. 0) write(*,*) ifld, 'scale for ',             &
     &                                 trim(fld_name), ': ', scale
!
      do inod = 1, n_point
        do icomp = 1, numdir
          diff = d_fld2(inod,icomp) - d_fld1(inod,icomp)
          if((abs(diff) / scale) .gt. TINY) then
            write(*,*) icomp, '-component of ', trim(fld_name),     &
     &                ' at ', inod, ' is different: ', diff
            iflag = iflag + 1
          end if
        end do
      end do
      deallocate(vmin, vmax, size)
      compare_field_vector = iflag
!
      end function compare_field_vector
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
!
      integer(kind = kint), intent(in) :: i1, i2, i3, j1, j2, j3, j4
!
!
      if ( i1 .eq. j1 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j2, j3, j4)
      else if ( i1 .eq. j2 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j3, j4, j1)
      else if ( i1 .eq. j3 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j4, j1, j2)
      else if ( i1 .eq. j4 ) then
       check_4_on_3 = check_3_on_2(i2, i3, j1, j2, j3)
      else
       check_4_on_3 = 0
      end if
!
      end function check_4_on_3
!
!------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    check_3_on_2(i1, i2, j1, j2, j3)
!
      integer(kind = kint), intent(in) :: i1, i2, j1, j2, j3
!
      if ( i1 .eq. j1 ) then
       check_3_on_2 =  check_2_on_1(i2, j2, j3)
      else if ( i1 .eq. j2 ) then
       check_3_on_2 =  check_2_on_1(i2, j3, j1)
      else if ( i1 .eq. j3 ) then
       check_3_on_2 =  check_2_on_1(i2, j1, j2)
      else
       check_3_on_2 = 0
      end if
!
      end function check_3_on_2
!
!------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    check_2_on_1(i1, j1, j2)
!
      integer(kind = kint), intent(in) :: i1, j1, j2
!
!
      if ( i1 .eq. j1 ) then
       check_2_on_1 = 1
      else if ( i1 .eq. j2 ) then
       check_2_on_1 = 1
      else
       check_2_on_1 = 0
      end if
!
      end function check_2_on_1
!
!------------------------------------------------------------------
!
      end module compare_indices
