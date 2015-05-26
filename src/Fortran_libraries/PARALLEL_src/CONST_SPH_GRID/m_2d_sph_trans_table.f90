!>@file   m_2d_sph_trans_table.f90
!!@brief  module m_2d_sph_trans_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Indices of spherical transform for second harmonics
!!
!!@verbatim
!!      subroutine allocate_2d_sph_trans_table(ntheta, nphi, jmax)
!!      subroutine deallocate_2d_sph_trans_table
!!
!!      subroutine check_2d_sph_indices(my_rank, nphi, ltr, jmax)
!!      subroutine check_2d_sph_trans_table(my_rank, ntheta, nphi)
!!@endverbatim
!!
!!@param my_rank  Process ID
!!@param ntheta   Number of meridional grids
!!@param nphi     Number of zonal grids
!!@param ltr      Truncation degree
!!@param jmax     Number of spherical harmonics modes
!
      module m_2d_sph_trans_table
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: mdx_ispack(:)
!
      integer(kind = kint), allocatable :: jdx_fsph(:)
      integer(kind = kint), allocatable :: mspec_4_ispack(:)
      integer(kind = kint), allocatable :: mtbl_fft_2_lgd(:)
!
      integer(kind = kint), allocatable :: mdx_4_lgd(:)
      integer(kind = kint), allocatable :: jtbl_fsph(:,:)
!
      integer(kind = kint), allocatable :: jtbl_rj(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_2d_sph_trans_table(ntheta, nphi, jmax)
!
      integer(kind = kint), intent(in) :: ntheta, nphi, jmax
!
      allocate( jdx_fsph(-ntheta:ntheta) )
      allocate( mspec_4_ispack(-ntheta:ntheta) )
!
      allocate( mtbl_fft_2_lgd(0:nphi) )
      allocate( mdx_4_lgd(0:nphi) )
!
      allocate( mdx_ispack(nphi) )
!
      allocate( jtbl_rj(0:jmax,3)   )
      allocate( jtbl_fsph(0:jmax,3) )
!
      jdx_fsph = 0
      mspec_4_ispack = 0
      mtbl_fft_2_lgd = 0
      mdx_4_lgd = 0
      mdx_ispack = 0
!
      jtbl_rj =   0
      jtbl_fsph = 0
!
      end subroutine allocate_2d_sph_trans_table
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_2d_sph_trans_table
!
      deallocate( jdx_fsph )
      deallocate( mspec_4_ispack )
!
      deallocate( mtbl_fft_2_lgd )
      deallocate( mdx_4_lgd )
!
      deallocate( mdx_ispack )
!
      deallocate( jtbl_rj )
      deallocate( jtbl_fsph )
!
      end subroutine deallocate_2d_sph_trans_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_2d_sph_indices(my_rank, nphi, ltr, jmax)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nphi, jmax, ltr
!
      integer(kind = kint) :: m, j, l
!
!
      write(my_rank+50,*) 'zonal wave numbers after FFT'
      write(my_rank+50,*) 'm, mdx_ispack(m)'
      do m = 1, nphi
        write(my_rank+50,*) m, mdx_ispack(m)
      end do
!
      write(my_rank+50,*) 'zonal wave numbers before Legendre trans.'
      write(my_rank+50,*) 'm, mdx_4_lgd(m)'
      do m = 0, nphi
        write(my_rank+50,*) m, mdx_4_lgd(m)
      end do
!
      write(my_rank+50,*) 'spectr data after Legendre transform'
      write(my_rank+50,*) 'j, jtbl_fsph(j,1:3)'
      do j = 0, jmax
        write(my_rank+50,*) j, jtbl_fsph(j,1:3)
      end do
!
      write(my_rank+50,*) 'spectr data ordering for linear terms'
      write(my_rank+50,*) 'j, jtbl_fsph(j,1:3)'
      do j = 0, jmax
        write(my_rank+50,*) j, jtbl_rj(j,1:3)
      end do
!
      write(my_rank+50,*) 'spectr data for final distribution'
      write(my_rank+50,*) 'j, l, m'
      do l = 0, ltr
        do m = -l, l
          j = l*(l+1) + m
          write(my_rank+50,*) j, l, m
        end do
      end do
!
      end subroutine check_2d_sph_indices
!
! -----------------------------------------------------------------------
!
      subroutine check_2d_sph_trans_table(my_rank, ntheta, nphi)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ntheta, nphi
!
      integer(kind = kint) :: m
!
      write(my_rank+50,*) 'zonal wave number and tranfer table'
      write(my_rank+50,*) 'm, mspec_4_ispack(m), jdx_fsph(m)'
      do m = -ntheta, ntheta
        write(my_rank+50,*) m, mspec_4_ispack(m), jdx_fsph(m)
      end do
!
      write(my_rank+50,*)                                               &
     &        'tranfer table from zonal spectr for Lag. trans.'
      write(my_rank+50,*) 'm0, mtbl_fft_2_lgd, mdx_4_lgd(m0)'
      do m = 0, nphi
        write(my_rank+50,*) m, mtbl_fft_2_lgd(m), mdx_4_lgd(m)
      end do
!
      end subroutine check_2d_sph_trans_table
!
! -----------------------------------------------------------------------
!
      end module m_2d_sph_trans_table
